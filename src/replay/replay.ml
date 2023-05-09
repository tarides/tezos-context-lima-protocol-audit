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

module FoldStack = struct
  (** Datastructure to handle calls to `Context.fold`

     When `Context.fold` is called a sequence of `Fold_start`,
     multiple `Fold_step_enter` and `Fold_step_exit`s followed by a
     `Fold_end` is recorded. For example:

     ```
     Fold_start

     Fold_step_enter
     (some other operations)
     Fold_step_exit

     Fold_step_enter
     (some other operations)
     Fold_step_exit

     (potentially many Fold_step blocks)


     Fold_exit
     ```

     We handle this by calling `Context.fold` when encoutering a
     `Fold_start` and storing all trees that are returned in a fifo
     queue. For every `Fold_step_enter` and `Fold_step_exit` we deque
     a tree and use it to handle the fold step.

     In order to handle nested folds, we maintain a stack of queues of
     trees. This module implements this stack of queues.

   *)

  module Queue = struct
    type 'a t = 'a list * 'a list

    let empty = ([], [])
    let is_empty = function [], [] -> true | _, _ -> false
    let enqueue x (pop, push) = (pop, x :: push)

    let rec dequeue = function
      | [], [] -> (None, empty)
      | x :: xs, push -> (Some x, (xs, push))
      | [], push -> dequeue (List.rev push, [])
  end

  module Stack : sig
    type 'a t

    val empty : 'a t
    val push : 'a -> 'a t -> 'a t
    val pop : 'a t -> 'a option * 'a t
  end = struct
    type 'a t = 'a list

    let empty = []
    let push x s = x :: s
    let pop = function [] -> (None, empty) | x :: rest -> (Some x, rest)
  end

  type 'a t = 'a Queue.t Stack.t

  let empty = Stack.empty
  let fold_start fs = Stack.push Queue.empty fs

  let fold_push_tree x fs =
    match Stack.pop fs with
    | Some q, fs' -> Stack.push (Queue.enqueue x q) fs'
    | None, _fs' -> failwith "fold stack empty"

  let fold_pop_tree fs =
    match Stack.pop fs with
    | Some q, fs' ->
        let tree, q' = Queue.dequeue q in
        (tree, Stack.push q' fs')
    | None, _ -> failwith "fold stack empty"

  let fold_end fs =
    let _, fs' = Stack.pop fs in
    fs'
end

(* Replay state *)

module State = struct
  (** State of the replay *)

  module TrackerMap = Map.Make (Optint.Int63)
  (* Large objects in the trace are not serialized, but instead
     tracked via an Int63.t identifier. We maintain maps to keep track of
     the objects. *)

  type t = {
    index : Context.index option;
    config : Config.t;
    (* Irmin Stats*)
    stats : Irmin_pack_unix.Stats.t;
    (* Tracked objects *)
    contexts : Context.t TrackerMap.t;
    trees : Context.tree TrackerMap.t;
    (* Fold Stack *)
    folds : Context.tree FoldStack.t;
    tree_folds : Context.tree FoldStack.t;
  }

  let init config =
    {
      index = None;
      config;
      stats = Context.irmin_stats ();
      contexts = TrackerMap.empty;
      trees = TrackerMap.empty;
      folds = FoldStack.empty;
      tree_folds = FoldStack.empty;
    }

  type tracker_id = Optint.Int63.t
  type context_tracked = Replay_actions.scope_end * tracker_id

  let track_context context ((scope_end, id) : context_tracked) state =
    match scope_end with
    | Replay_actions.Will_reoccur ->
        { state with contexts = TrackerMap.add id context state.contexts }
    | Replay_actions.Last_occurence -> state

  let get_context ((scope_end, id) : context_tracked) state =
    let context = TrackerMap.find id state.contexts in
    let state' =
      match scope_end with
      | Replay_actions.Will_reoccur -> state
      | Replay_actions.Last_occurence ->
          { state with contexts = TrackerMap.remove id state.contexts }
    in
    (context, state')

  type tree_tracked = Replay_actions.scope_end * tracker_id

  let track_tree context ((scope_end, id) : tree_tracked) state =
    match scope_end with
    | Replay_actions.Will_reoccur ->
        { state with trees = TrackerMap.add id context state.trees }
    | Replay_actions.Last_occurence -> state

  let get_tree ((scope_end, id) : tree_tracked) state =
    let tree = TrackerMap.find id state.trees in
    let state' =
      match scope_end with
      | Replay_actions.Will_reoccur -> state
      | Replay_actions.Last_occurence ->
          { state with trees = TrackerMap.remove id state.trees }
    in
    (tree, state')

  let fold_start state = { state with folds = FoldStack.fold_start state.folds }

  let tree_fold_start state =
    { state with tree_folds = FoldStack.fold_start state.tree_folds }

  let fold_push_tree tree state =
    { state with folds = FoldStack.fold_push_tree tree state.folds }

  let tree_fold_push_tree tree state =
    { state with tree_folds = FoldStack.fold_push_tree tree state.tree_folds }

  let fold_pop_tree state =
    let tree_opt, folds = FoldStack.fold_pop_tree state.folds in
    match tree_opt with
    | Some tree -> (tree, { state with folds })
    | None ->
        failwith
          "attempting to pop a tree during fold while tree queue is empty"

  let tree_fold_pop_tree state =
    let tree_opt, tree_folds = FoldStack.fold_pop_tree state.tree_folds in
    match tree_opt with
    | Some tree -> (tree, { state with tree_folds })
    | None ->
        failwith
          "attempting to pop a tree during Tree.fold while tree queue is empty"
end

(* Replay logic *)

module Ops = struct
  let pp = Repr.pp Replay_actions.event_t

  let error m =
    Log.err m;
    failwith "while handling operations"

  let bad_result m =
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

  let exec_get_protocol state (input, _output) =
    let context, state' = State.get_context input state in
    let* _ = Context.get_protocol context in
    return state'

  let exec_get_test_chain state (context_t, _) =
    let context, state' = State.get_context context_t state in
    let* _ = Context.get_test_chain context in
    return state'

  let exec_find state ((context_t, key), res) =
    let context, state' = State.get_context context_t state in
    let* res' = Context.find context key >|= Option.is_some in
    if res <> res' then bad_result (fun m -> m "bad result") else ();
    return state'

  let exec_find_tree state ((context_t, key), expected) =
    let context, state' = State.get_context context_t state in
    let* res' = Context.find_tree context key in
    match (expected, res') with
    | Some expected_tree_c, Some tree ->
        state' |> State.track_tree tree expected_tree_c |> return
    | None, None -> return state
    | _ -> bad_result (fun m -> m "wrong result in find tree")

  let exec_add_tree state ((context_t, key, tree_t), output_context_t) =
    let context, state' = State.get_context context_t state in
    let tree, state'' = State.get_tree tree_t state' in
    let* result = Context.add_tree context key tree in
    state'' |> State.track_context result output_context_t |> return

  let exec_mem state ((context_t, key), res) =
    let context, state' = State.get_context context_t state in
    let* res' = Context.mem context key in
    if res <> res' then bad_result (fun m -> m "bad result for mem");
    return state'

  let exec_add state ((context_t, key, value), output) =
    let context, state' = State.get_context context_t state in
    let* output_context = Context.add context key value in
    state' |> State.track_context output_context output |> return

  let exec_remove state ((context_t, key), output) =
    let context, state' = State.get_context context_t state in
    let* output_context = Context.remove context key in
    state' |> State.track_context output_context output |> return

  let exec_fold_start state ((depth, order), context_t, key) =
    let context, state' = State.get_context context_t state in
    Context.fold ?depth ~order context key ~init:(State.fold_start state')
      ~f:(fun _key tree state -> return @@ State.fold_push_tree tree state)

  let exec_fold_step_enter state tree_t =
    let tree, state' = State.fold_pop_tree state in
    state' |> State.track_tree tree tree_t |> return

  let exec_fold_step_exit state _tree_t = return state

  let exec_fold_end (state : State.t) =
    return { state with folds = FoldStack.fold_end state.folds }

  let exec_tree_fold_start state ((depth, order), tree_t, key) =
    let tree, state' = State.get_tree tree_t state in
    Context.Tree.fold ?depth ~order tree key
      ~init:(State.tree_fold_start state') ~f:(fun _key tree state ->
        return @@ State.tree_fold_push_tree tree state)

  let exec_tree_fold_step_enter state tree_t =
    let tree, state' = State.tree_fold_pop_tree state in
    state' |> State.track_tree tree tree_t |> return

  let exec_tree_fold_step_exit state _tree_t = return state

  let exec_tree_fold_end (state : State.t) _ =
    return { state with tree_folds = FoldStack.fold_end state.tree_folds }

  let exec_commit state ((time, message, context_t), _hash_t) =
    let time = Tezos_base.Time.Protocol.of_seconds time in
    let context, state' = State.get_context context_t state in
    let* _hash = Context.commit ~time ?message context in
    return state'

  let exec_add_predecessor_block_metadata_hash state
      ((context_t, hash_s), output_context_t) =
    let hash = Tezos_crypto.Hashed.Block_metadata_hash.of_string_exn hash_s in
    let context, state' = State.get_context context_t state in
    let* output_context =
      Context.add_predecessor_block_metadata_hash context hash
    in
    state' |> State.track_context output_context output_context_t |> return

  let exec_add_predecessor_ops_metadata_hash state
      ((context_t, hash_s), output_context_t) =
    let hash =
      Tezos_crypto.Hashed.Operation_metadata_list_list_hash.of_string_exn hash_s
    in
    let context, state' = State.get_context context_t state in
    let* output_context =
      Context.add_predecessor_ops_metadata_hash context hash
    in
    state' |> State.track_context output_context output_context_t |> return

  let exec state op =
    Replay_actions.(
      match op with
      | Init args -> exec_init state args
      | Checkout args -> exec_checkout state args
      | Get_protocol args -> exec_get_protocol state args
      | Get_test_chain args -> exec_get_test_chain state args
      | Find args -> exec_find state args
      | Find_tree args -> exec_find_tree state args
      | Add_tree args -> exec_add_tree state args
      | Mem args -> exec_mem state args
      | Add args -> exec_add state args
      | Remove args -> exec_remove state args
      | Fold_start (options, context_t, key) ->
          exec_fold_start state (options, context_t, key)
      | Fold_step_enter args -> exec_fold_step_enter state args
      | Fold_step_exit args -> exec_fold_step_exit state args
      | Fold_end -> exec_fold_end state
      | Tree (Tree.Fold_start (options, tree_t, key)) ->
          exec_tree_fold_start state (options, tree_t, key)
      | Tree (Tree.Fold_step_enter args) -> exec_tree_fold_step_enter state args
      | Tree (Tree.Fold_step_exit args) -> exec_tree_fold_step_exit state args
      | Tree (Tree.Fold_end args) -> exec_tree_fold_end state args
      | Commit args -> exec_commit state args
      | Add_predecessor_block_metadata_hash args ->
          exec_add_predecessor_block_metadata_hash state args
      | Add_predecessor_ops_metadata_hash args ->
          exec_add_predecessor_ops_metadata_hash state args
      | op -> error (fun m -> m "unhandled operation: %a" pp op))
end

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

let exec_block (state : State.t) (row : Replay_actions.row) =
  Log.app (fun m -> m "level: %d" row.level);

  let pre_stats = Irmin_pack_unix.Stats.Io.export state.stats.io in

  (* execute all operations in block *)
  let* state' =
    Array.to_seq row.ops |> Lwt_seq.of_seq |> Lwt_seq.fold_left_s Ops.exec state
  in

  let post_stats = Irmin_pack_unix.Stats.Io.export state.stats.io in

  let stats_diff = diff_io_stats post_stats pre_stats in

  let level = row.level in

  Log.app (fun m ->
      m "IO activity for block %d: %a" level pp_io_stats stats_diff);

  return state'

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
