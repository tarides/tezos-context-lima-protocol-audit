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

(* let context_hash_of_level_2981990 = *)
(*   Context_hash.of_b58check_exn *)
(*     "CoVuewvaeiWqmvaiCnxyWCpGqiErzozZAtTkgFpTtPub7pbiRYPW" *)

module Replay_actions = Tezos_context_trace.Replay_actions

let classify_op (op : Replay.Operation.t) =
  Replay_actions.(
    match op with
    | Tree _ -> None
    | Find_tree ((_context, key), _) -> Some key
    | Fold_start (_, _, _) -> None
    | Fold_step_enter _ -> None
    | Fold_step_exit _ -> None
    | Fold_end -> None
    | Add_tree _ -> None
    | Mem ((_context, key), _) -> Some key
    | Mem_tree _ -> None
    | Find ((_context, key), _) -> Some key
    | Get_protocol _ -> None
    | Hash _ -> None
    | Find_predecessor_block_metadata_hash _ -> None
    | Find_predecessor_ops_metadata_hash _ -> None
    | Get_test_chain _ -> None
    | Exists _ -> None
    | Add _ -> None
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

module Activity = struct
  module CountIndex = Map.Make (Int)

  (* type path = string *)

  let path key = String.concat "/" key

  module PathSet = Set.Make (String)

  type t = { by_path : int Art.t; mutable read_index : PathSet.t CountIndex.t }

  let init () = { by_path = Art.make (); read_index = CountIndex.empty }

  let increment_read_at t key =
    let path = path key in
    match Art.find_opt t.by_path (Art.key path) with
    | Some c ->
        let c' = c + 1 in
        let read_index' =
          t.read_index
          |> CountIndex.update c (function
               | None -> None
               | Some ps ->
                   let ps' = PathSet.remove path ps in
                   if PathSet.is_empty ps' then None else Some ps')
          |> CountIndex.update c' (function
               | None -> Option.some @@ PathSet.singleton path
               | Some ps -> Option.some @@ PathSet.add path ps)
        in

        Art.insert t.by_path (Art.key path) c';
        t.read_index <- read_index'
    | None ->
        let read_index' =
          t.read_index
          |> CountIndex.update 1 (function
               | None -> Option.some @@ PathSet.singleton path
               | Some ps -> Option.some @@ PathSet.add path ps)
        in

        Art.insert t.by_path (Art.key path) 1;
        t.read_index <- read_index'

  let pp = Art.pp Fmt.int
end

let run (config : Replay.Config.t) =
  let header, _state, blocks = Replay.read_blocks config in

  Log.info (fun m -> m "Blocks in replay: %d." header.block_count);

  let activity : Activity.t = Activity.init () in

  let () =
    blocks |> Seq.take 1000
    |> Seq.flat_map (fun (block : Replay.Block.t) -> Array.to_seq block.ops)
    |> Seq.filter_map classify_op
    |> Seq.iter (fun key -> Activity.increment_read_at activity key)
  in

  activity.read_index |> Activity.CountIndex.to_rev_seq
  |> Seq.iter (fun (count, ps) ->
         Log.app (fun m ->
             m "%d: %a" count
               Fmt.(braces @@ seq ~sep:semi string)
               (Activity.PathSet.to_seq ps)));

  ignore Activity.pp;

  (* Log.app (fun m -> m "%a" Activity.pp activity); *)
  return_unit

let () =
  setup_logs ();

  let actions_trace_path = "/home/adatario/dev/tclpa/inputs/actions.trace" in

  let store_path = "/home/adatario/dev/tclpa/inputs/store-level-2981990" in
  let (config : Replay.Config.t) = { store_path; actions_trace_path } in

  Lwt_main.run @@ run config
