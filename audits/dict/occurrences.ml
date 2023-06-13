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
module StringMap = Map.Make (String)

module Occurrence = struct
  include Binning.Make (StringMap)

  type increment = Init | Hit
  type nonrec t = (string, increment, int) t

  let empty =
    create ~projection:Fun.id ~add:(fun increment summary_opt ->
        match increment with
        | Init ->
            if Option.is_some summary_opt then (
              Log.err (fun m -> m "duplicate!");
              Some 0)
            else Some 0
        | Hit -> Option.map Int.succ summary_opt)

  let init segment b = add segment Init b
  let hit segment b = add segment Hit b
end

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

  let dict = Store.Internal.dict repo in

  let occurences : Occurrence.t =
    (* init occurences with dict entries *)
    Seq.init 100_000 (fun x -> x)
    |> Seq.fold_left
         (fun occurences i ->
           match Dict.find dict i with
           | Some s -> Occurrence.init s occurences
           | None -> occurences)
         Occurrence.empty
  in

  let hash =
    Tezos_context_encoding.Context.Hash.of_context_hash
      context_hash_of_level_3081990
  in

  let* commit = Store.Commit.of_hash repo hash >|= Option.get in
  let tree = Store.Commit.tree commit in

  let* occurences =
    Store.Tree.fold
      ~pre:(fun _path steps occurences ->
        let last_seg = List.nth steps (List.length steps - 1) in
        Occurrence.hit last_seg occurences |> return)
      ~contents:(fun path _content occurences ->
        let last_seg = List.nth path (List.length path - 1) in
        Occurrence.hit last_seg occurences |> return)
      tree occurences
  in

  let sorted_occurences =
    occurences |> Occurrence.to_seq |> List.of_seq
    |> List.sort (fun (_, a_count) (_, b_count) -> Int.compare a_count b_count)
  in

  List.iter
    (fun (entry, count) -> Format.printf "%s,\t%d\n" entry count)
    sorted_occurences;

  return_unit

let () =
  setup_logs ();
  Lwt_main.run @@ main store_path
