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

let pp_io_stats =
  let module ReprMap = Repr.Of_map (struct
    include Irmin_pack_unix.Stats.Io.PathMap

    let key_t = Repr.string
  end) in
  Repr.pp @@ ReprMap.t Irmin_pack_unix.Stats.Io.Activity.t

let diff_io_stats a b =
  Irmin_pack_unix.Stats.Io.(
    PathMap.merge
      (fun _path a b ->
        match (a, b) with
        | Some a, Some b -> Option.some @@ Activity.diff a b
        | Some a, None -> Some a
        | None, Some b -> Some b
        | None, None -> None)
      a b)

(* Run a replay and collect IO stats *)

let run (config : Replay.Config.t) =
  let header, state, actions = Replay.read_blocks config in

  let block_count = header.block_count in
  Log.app (fun m -> m "block_count: %d" block_count);

  let* _final_state =
    actions
    |> Lwt_seq.fold_left_s
         (fun state (block : Replay.block) ->
           let level = block.level in

           Log.app (fun m -> m "Replaying block level: %d" block.level);

           let stats = Replay.State.stats state in

           let pre_stats = Irmin_pack_unix.Stats.Io.export stats.io in

           let* state' = Replay.exec_block block state in

           let post_stats = Irmin_pack_unix.Stats.Io.export stats.io in

           let stats_diff = diff_io_stats post_stats pre_stats in

           Log.app (fun m ->
               m "IO activity for block %d: %a" level pp_io_stats stats_diff);

           return state')
         state
  in

  return_unit

let () =
  setup_logs ();

  let actions_trace_path = "/home/adatario/dev/tclpa/inputs/actions.trace" in
  let store_path = "/home/adatario/dev/tclpa/inputs/store-level-2981990" in

  let (config : Replay.Config.t) =
    { store_path; actions_trace_path }
    |> Replay.Config.copy_store_to_temp_location
  in

  Lwt_main.run @@ run config
