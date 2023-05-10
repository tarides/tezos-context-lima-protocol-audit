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

module Context = Tezos_context_disk.Context
module Context_hash = Tezos_crypto.Hashed.Context_hash

let context_hash_of_level_2981990 =
  Context_hash.of_b58check_exn
    "CoVuewvaeiWqmvaiCnxyWCpGqiErzozZAtTkgFpTtPub7pbiRYPW"

(* let get_tree_diff state last_commit commit = *)
(*   match Replay.State.index state with *)
(*   | Some index -> *)
(*      let last_tree = Context.checkout index last_commit |> Option.get *)
(*                    |> Context.tree *)
(*      in *)

(*   | None -> *)
(*      return [] *)

let run (config : Replay.Config.t) =
  let header, state, actions = Replay.read_blocks config in

  Log.app (fun m -> m "Blocks in replay: %d." header.block_count);

  let* _final_state =
    actions
    |> Lwt_seq.fold_left_s
         (fun (state, last_commit) (block : Replay.block) ->
           let level = block.level in

           Log.app (fun m -> m "Replaying block level: %d" block.level);

           (* Get the IO stats before executing block operations *)
           let stats = Replay.State.stats state in
           let pre_stats = Irmin_pack_unix.Stats.Io.export stats.io in

           (* Execute block operations *)
           let* state' = Replay.exec_block block state in

           (* Get the IO stats after executing block operations *)
           let post_stats = Irmin_pack_unix.Stats.Io.export stats.io in
           let stats_diff = diff_io_stats post_stats pre_stats in

           (* Get the block_commit *)
           let block_commit =
             Replay.State.last_commit state |> Option.value ~default:last_commit
           in

           Log.app (fun m ->
               m "IO activity for block %d: %a" level pp_io_stats stats_diff);

           return (state', block_commit))
         (state, context_hash_of_level_2981990)
  in

  return_unit

let () =
  setup_logs ();

  let actions_trace_path = "/home/adatario/dev/tclpa/inputs/actions.trace" in
  (* let store_path = "/home/adatario/dev/tclpa/inputs/store-level-2981990" in *)
  let store_path = "/tmp/tezos-context-store-113038" in

  let (config : Replay.Config.t) =
    { store_path; actions_trace_path }
    (* |> Replay.Config.copy_store_to_temp_location *)
  in

  Lwt_main.run @@ run config
