(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

open Lwt
open Lwt.Syntax

(* Logging *)

let setup_logs () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level @@ Some Info)

let store_path = "/home/adatario/dev/tclpa/inputs/store-level-3081990"

(* The Tezos Context *)
module Context = Tezos_context_disk.Context
module Context_hash = Tezos_crypto.Hashed.Context_hash

(* Do the Irmin Functor dance *)
module Store = struct
  open Tezos_context_encoding.Context
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
end

(* Get some internal modules *)
module Index = Store.Internal.Index
module File_manager = Store.Internal.File_manager
module Dict = Store.Internal.Dict
module Dispatcher = Store.Internal.Dispatcher

(* let pp_path = Repr.pp @@ Store.path_t *)
(* let pp_node = Repr.pp @@ Store.node_t *)

let context_hash_of_level_3081990 =
  Context_hash.of_b58check_exn
    "CoUr71WP9479RrsT6BdeFatazHbLdZGsCPfY2YaMgZhAeYe5z7QU"

module Stats = struct
  type t = {
    (* Stats for internal nodes. Not much more we can get out from the exposed API. *)
    node_count : int;
    (* Stats for content *)
    content_count : int;
    content_total_size : int;
    content_max_size : Store.path * int;
    content_min_size : Store.path * int;
  }

  let null =
    {
      node_count = 0;
      content_count = 0;
      content_total_size = 0;
      content_max_size = ([], -1);
      content_min_size = ([], max_int);
    }

  let pp =
    Fmt.(
      record
        [
          field "node_count" (fun t -> t.node_count) int;
          field "content_count" (fun t -> t.content_count) int;
          field "content_total_size" (fun t -> t.content_total_size) int;
          field "content_max_size" (fun t -> t.content_max_size)
          @@ pair (Repr.pp Store.path_t) int;
          field "content_min_size" (fun t -> t.content_min_size)
          @@ pair (Repr.pp Store.path_t) int;
        ])
end

let main root =
  let* repo =
    Store.Repo.v
      (Irmin_pack.config ~readonly:true
         ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal root)
  in

  (* Load the tree of block 3081990 *)
  let hash =
    Tezos_context_encoding.Context.Hash.of_context_hash
      context_hash_of_level_3081990
  in
  let* commit = Store.Commit.of_hash repo hash >|= Option.get in
  let tree = Store.Commit.tree commit in

  (* Fold over the tree to get some information about the store structure *)
  let* stats =
    Store.Tree.fold tree
      ~node:(fun _path _node (stats : Stats.t) ->
        return { stats with node_count = stats.node_count + 1 })
      ~contents:(fun path content (stats : Stats.t) ->
        let len = Bytes.length content in

        let content_max_size =
          if len > snd stats.content_max_size then (path, len)
          else stats.content_max_size
        in

        let content_min_size =
          if len < snd stats.content_min_size then (path, len)
          else stats.content_min_size
        in

        return
          {
            stats with
            content_count = stats.content_count + 1;
            content_total_size = stats.content_total_size + len;
            content_max_size;
            content_min_size;
          })
      Stats.null
  in

  Logs.app (fun m -> m "%a" Stats.pp stats);

  return_unit

let () =
  setup_logs ();
  Lwt_main.run @@ main store_path
