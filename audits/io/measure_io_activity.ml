(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

open Lwt
open Lwt.Syntax

(* Logging *)

let src = Logs.Src.create "tlpa"

module Log = (val Logs.src_log src : Logs.LOG)

let setup_logs () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level @@ Some Info)

(* IO stats helpers *)
module Stats = Irmin_pack_unix.Stats

let pp_activity = Repr.pp Stats.Io.Activity.t

(* let pp_io_stats = *)
(*   let module ReprMap = Repr.Of_map (struct *)
(*     include Irmin_pack_unix.Stats.Io.PathMap *)

(*     let key_t = Repr.string *)
(*   end) in *)
(*   Repr.pp @@ ReprMap.t Irmin_pack_unix.Stats.Io.Activity.t *)

let diff_io_stats a b =
  Stats.Io.(
    PathMap.merge
      (fun _path a b ->
        match (a, b) with
        | Some a, Some b -> Option.some @@ Activity.diff a b
        | Some a, None -> Some a
        | None, Some b -> Some b
        | None, None -> None)
      a b)

module ActivityCsv = struct
  module PathMap = Stats.Io.PathMap
  module IntMap = Map.Make (Int)

  module Mapping = struct
    type path = Stats.Io.path
    type t = { to_index : int PathMap.t; to_path : path IntMap.t }

    let empty = { to_index = PathMap.empty; to_path = IntMap.empty }

    let ensure_mapped activity mapping =
      activity |> PathMap.to_seq
      |> Seq.fold_left
           (fun m (path, _) ->
             if PathMap.mem path m.to_index then m
             else
               let i =
                 (IntMap.max_binding_opt m.to_path
                 |> Option.map fst |> Option.value ~default:(-1))
                 + 1
               in
               {
                 to_index = PathMap.add path i m.to_index;
                 to_path = IntMap.add i path m.to_path;
               })
           mapping
  end

  let flatten (m : Mapping.t) activity =
    IntMap.to_seq m.to_path
    |> Seq.flat_map (fun (_, path) ->
           match PathMap.find_opt path activity with
           | Some (activity : Stats.Io.Activity.t) ->
               List.to_seq
                 [
                   activity.bytes_read;
                   activity.nb_reads;
                   activity.bytes_written;
                   activity.nb_writes;
                 ]
           | None -> List.to_seq [ 0; 0; 0; 0 ])
    |> Seq.map string_of_int |> List.of_seq

  let emit level m activity =
    let out =
      String.concat "," @@ (string_of_int level :: flatten m activity)
    in
    print_endline out

  let header store_path (m : Mapping.t) =
    IntMap.to_seq m.to_path
    |> Seq.flat_map (fun (_, path) ->
           let path =
             Stringext.chop_prefix ~prefix:store_path path
             |> Option.value ~default:path
           in
           List.to_seq
             [
               path ^ " (bytes_read)";
               path ^ " (nb_reads)";
               path ^ " (bytes_written)";
               path ^ " (nb_writees)";
             ])
    |> Seq.append (Seq.return "level")
    |> List.of_seq |> String.concat ","
end

(* Run a replay and collect IO stats *)

module Context = Tezos_context_disk.Context
module Context_hash = Tezos_crypto.Hashed.Context_hash

(* let context_hash_of_level_2981990 = *)
(*   Context_hash.of_b58check_exn *)
(*     "CoVuewvaeiWqmvaiCnxyWCpGqiErzozZAtTkgFpTtPub7pbiRYPW" *)

let run (config : Replay.Config.t) =
  let header, state, actions = Replay.read_blocks config in

  Log.info (fun m -> m "Blocks in replay: %d." header.block_count);

  let* _final_state, activity_mapping =
    actions
    (* |> Seq.take 1000 *)
    |> Lwt_seq.of_seq
    |> Lwt_seq.fold_left_s
         (fun (state, activity_mapping) (block : Replay.Block.t) ->
           let level = block.level in

           (* Get the IO stats before executing block operations *)
           let stats = Replay.State.stats state in
           let pre_stats = Stats.Io.export stats.io in

           (* Setup a counter *)
           let counter = Mtime_clock.counter () in

           (* Execute block operations *)
           let[@landmark "exec"] state' = Replay.Block.exec block state in

           let* state' = state' in

           let time_s = Mtime_clock.count counter |> Mtime.Span.to_s in

           (* Get the IO stats after executing block operations *)
           let post_stats = Stats.Io.export stats.io in
           let activity = diff_io_stats post_stats pre_stats in

           (* Compute the total IO activity *)
           let total_activity =
             Stats.Io.PathMap.fold
               (fun _path total_activity path_activity ->
                 Stats.Io.Activity.sum
                   Seq.(return total_activity |> cons path_activity))
               activity Stats.Io.Activity.zero
           in

           (* Update mapping of activities to CSV indices *)
           let activity_mapping =
             ActivityCsv.Mapping.ensure_mapped activity activity_mapping
           in

           (* Emit as line of CSV *)
           ActivityCsv.emit level activity_mapping activity;

           let read_bw =
             float_of_int total_activity.bytes_read /. (time_s *. 1024. *. 1024.)
           in

           let write_bw =
             float_of_int total_activity.bytes_written
             /. (time_s *. 1024. *. 1024.)
           in

           Log.info (fun m ->
               m
                 "BLOCK - level: %d; time (s): %f; read_bw (MiB/s): %f; \
                  write_bw (MiB/s): %f; total_activity: %a"
                 level time_s read_bw write_bw pp_activity total_activity);
           return (state', activity_mapping))
         (state, ActivityCsv.Mapping.empty)
  in

  (* Emit a header on last line *)
  print_endline @@ ActivityCsv.header config.store_path activity_mapping;

  return_unit

let () =
  setup_logs ();

  let actions_trace_path = "/home/adatario/dev/tclpa/inputs/actions.trace" in

  let store_path = "/home/adatario/dev/tclpa/inputs/store-level-2981990" in
  (* let store_path = "/tmp/tezos-context-store-19991" in *)
  let (config : Replay.Config.t) =
    { store_path; actions_trace_path }
    |> Replay.Config.copy_store_to_temp_location
  in

  Lwt_main.run @@ run config
