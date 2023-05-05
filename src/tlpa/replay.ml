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

(* Tezos Context and Replay Actions *)

module Context = Tezos_context_disk.Context
module Replay_actions = Tezos_context_trace.Replay_actions

(* Replay state *)

module State = struct
  module TrackerMap = Map.Make (Optint.Int63)

  type t = {
    index : Context.index option;
    config : Config.t;
    (* Tracked objects *)
    contexts : Context.t TrackerMap.t;
  }

  let init config = { index = None; config; contexts = TrackerMap.empty }

  let track_context context (scope_end, id) state =
    match scope_end with
    | Replay_actions.Will_reoccur ->
        { state with contexts = TrackerMap.add id context state.contexts }
    | Replay_actions.Last_occurence -> state
end

(* Replay logic *)

module Ops = struct
  let pp = Repr.pp Replay_actions.event_t

  let error m =
    Log.err m;
    failwith "while handling operations"

  let exec_init (state : State.t) (readonly, ()) =
    let* index = Context.init ~readonly state.config.store_path in
    return { state with index = Some index }

  let of_commit_hash_lhs (_, _, hash_s) =
    Tezos_crypto.Hashed.Context_hash.of_string_exn hash_s

  let exec_checkout (state : State.t) (input, output) =
    let hash = of_commit_hash_lhs input in
    let* context =
      Context.checkout (Option.get state.index) hash >>= function
      | Some context -> return context
      | None -> error (fun m -> m "could not checkout context")
    in
    state |> State.track_context context output |> return

  let exec state op =
    Replay_actions.(
      match op with
      | Init args -> exec_init state args
      | Checkout args -> exec_checkout state args
      | op -> error (fun m -> m "unhandled operation: %a" pp op))
end

let exec_block state (row : Replay_actions.row) =
  Log.app (fun m -> m "level: %d" row.level);

  (* execute all operations in block *)
  Array.to_seq row.ops |> Lwt_seq.of_seq |> Lwt_seq.fold_left_s Ops.exec state

let run (config : Config.t) =
  let version, header, actions =
    Replay_actions.open_reader config.actions_trace_path
  in

  let block_count = header.block_count in
  Log.app (fun m -> m "Version: %d; block_count: %d" version block_count);

  let* _final_state =
    actions |> Lwt_seq.of_seq
    |> Lwt_seq.fold_left_s exec_block (State.init config)
  in

  return_unit
