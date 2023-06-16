(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

open Lwt

(* Logging *)

let src = Logs.Src.create "tlpa"

module Log = (val Logs.src_log src : Logs.LOG)

let setup_logs () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level @@ Some Info)

(* Run a replay and collect IO stats *)

module Context = Tezos_context_disk.Context
module Context_hash = Tezos_crypto.Hashed.Context_hash
module Replay_actions = Tezos_context_trace.Replay_actions

let classify_op (op : Replay.Operation.t) =
  Replay_actions.(
    match op with
    | Tree _ -> None
    | Find_tree ((_context, key), _) -> Some (`Read key)
    | Fold_start (_, _, _) -> None
    | Fold_step_enter _ -> None
    | Fold_step_exit _ -> None
    | Fold_end -> None
    | Add_tree ((_context, key, _), _) -> Some (`Write key)
    | Mem ((_context, key), _) -> Some (`Mem key)
    | Mem_tree ((_context, key), _) -> Some (`Mem key)
    | Find ((_context, key), _) -> Some (`Read key)
    | Get_protocol _ -> None
    | Hash _ -> None
    | Find_predecessor_block_metadata_hash _ -> None
    | Find_predecessor_ops_metadata_hash _ -> None
    | Get_test_chain _ -> None
    | Exists _ -> None
    | Add ((_context, key, _), _) -> Some (`Write key)
    | Remove _ -> None
    | Add_protocol _ -> None
    | Add_predecessor_block_metadata_hash _ -> None
    | Add_predecessor_ops_metadata_hash _ -> None
    | Add_test_chain _ -> None
    | Remove_test_chain _ -> None
    | Fork_test_chain _ -> None
    | Checkout _ -> None
    | Commit_genesis_start _ -> None
    | Commit_genesis_end _ -> None
    | Clear_test_chain _ -> None
    | Commit _ -> None
    | Init _ -> None
    | Patch_context_enter _ -> None
    | Patch_context_exit (_, _) -> None
    | Gc _ -> None
    | Split -> None)

let path key = String.concat "/" key

module Ops = struct
  type op = { read : int; mem : int; write : int }
  type t = (string, op) Hashtbl.t

  let init : unit -> t = fun () -> Hashtbl.create 1024

  let increment_at t key =
    let path, klass =
      match key with
      | `Mem key -> (path key, `Mem)
      | `Read key -> (path key, `Read)
      | `Write key -> (path key, `Write)
      | _ -> assert false
    in
    let op =
      match Hashtbl.find_opt t path with
      | None -> { read = 0; mem = 0; write = 0 }
      | Some op -> (
          match klass with
          | `Mem -> { op with mem = op.mem + 1 }
          | `Read -> { op with read = op.read + 1 }
          | `Write -> { op with write = op.write + 1 })
    in
    Hashtbl.replace t path op

  let iter = Hashtbl.iter
end

let run (config : Replay.Config.t) =
  let header, _state, blocks = Replay.read_blocks config in

  Log.info (fun m -> m "Blocks in replay: %d." header.block_count);

  let ops = Ops.init () in

  let () =
    blocks |> Seq.take 1
    |> Seq.flat_map (fun (block : Replay.Block.t) -> Array.to_seq block.ops)
    |> Seq.filter_map classify_op
    |> Seq.iter (fun key -> Ops.increment_at ops key)
  in

  let log path (op : Ops.op) = Log.app (fun m -> m "%s,%d,%d,%d" path op.mem op.read op.write) in
  Ops.iter log ops;

  return_unit

let () =
  setup_logs ();
  let base =
    "/home/adatario/dev/irmin-tezos-benchmarking/worktrees/main/inputs/202302-lima-rolling"
  in
  let actions_trace_path = Filename.concat base "100k-blocks/actions.trace" in
  let store_path = Filename.concat base "data-dir/context" in
  let (config : Replay.Config.t) = { store_path; actions_trace_path } in
  Lwt_main.run @@ run config
