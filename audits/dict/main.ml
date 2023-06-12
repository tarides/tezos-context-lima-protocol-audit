(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

open Lwt
open Lwt.Syntax

(* Logging *)

let src = Logs.Src.create "dict"

module Log = (val Logs.src_log src : Logs.LOG)

let setup_logs () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level @@ Some Info)

(* let store_path = "/tmp/tlpa-store" *)
(* let store_path = "/tmp/tezos-context-store-113038" *)
let store_path = "/home/adatario/dev/tclpa/inputs/store-level-3081990"

(* The Tezos Context *)
module Context = Tezos_context_disk.Context
module Context_hash = Tezos_crypto.Hashed.Context_hash

(* Do the Irmin Functor dance *)
module Store = struct
  open Tezos_context_encoding.Context
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
  module Schema = Tezos_context_encoding.Context.Schema
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

let main root =
  let* repo =
    Store.Repo.v
      (Irmin_pack.config ~readonly:true
         ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal root)
  in

  (* let fm = Store.Internal.file_manager repo in *)
  (* let index = Store.Internal.File_manager.index fm in *)
  let dict = Store.Internal.dict repo in

  (* snippet to dump the dict *)
  (* Seq.init 100_000 (fun x -> x) *)
  (* |> Seq.iter (fun i -> *)
  (*        match Dict.find dict i with *)
  (*        | Some s -> Log.app (fun m -> m "%d: %s" i s) *)
  (*        | None -> Log.app (fun m -> m "%d: NONE" i)); *)
  let hash =
    Tezos_context_encoding.Context.Hash.of_context_hash
      context_hash_of_level_3081990
  in

  let* commit = Store.Commit.of_hash repo hash >|= Option.get in
  let tree = Store.Commit.tree commit in

  let* hits, misses =
    Store.Tree.fold
      ~contents:(fun path _content hm ->
        List.fold_left
          (fun (hits, misses) step ->
            Log.app (fun m ->
                m "hits: %d misses: %d hit_rate: %f" hits misses
                  (100. *. (float_of_int hits /. float_of_int (misses + hits))));

            if Option.is_some @@ Dict.index dict step then (hits + 1, misses)
            else (hits, misses + 1))
          hm path
        |> return)
      tree (0, 0)
  in

  Log.app (fun m ->
      m "hits: %d misses: %d hit_rate: %f" hits misses
        (100. *. (float_of_int hits /. float_of_int (misses + hits))));
  return_unit

(* let offset, length, kind = Index.find index hash |> Option.get in *)

(* Log.app (fun m -> *)
(*     m "offset: %a, length: %d, kind: %a" Optint.Int63.pp offset length *)
(*       Irmin_pack.Pack_value.Kind.pp kind); *)
(* return_unit *)

let () =
  setup_logs ();
  Lwt_main.run @@ main store_path
