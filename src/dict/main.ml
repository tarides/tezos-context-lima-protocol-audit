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

let store_path = "/tmp/tlpa-store"

(* The Tezos Context *)
module Context = Tezos_context_disk.Context

module Store = struct
  open Tezos_context_encoding.Context
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
  module Schema = Tezos_context_encoding.Context.Schema
end

module Index = Store.Internal.Index
module File_manager = Store.Internal.File_manager
module Dict = Store.Internal.Dict
module Dispatcher = Store.Internal.Dispatcher

let main root =
  let* repo =
    Store.Repo.v
      (Irmin_pack.config ~readonly:true
         ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal root)
  in

  let fm = Store.Internal.file_manager repo in
  let index = Store.Internal.File_manager.index fm in

  let hash =
    Tezos_context_encoding.Context.Hash.of_context_hash
    @@ Tezos_crypto.Hashed.Context_hash.of_b58check_exn
         "CoVuewvaeiWqmvaiCnxyWCpGqiErzozZAtTkgFpTtPub7pbiRYPW"
  in

  match Index.find index hash with
  | None -> fail_with "Could not find hash in index"
  | Some (offset, length, kind) ->
      Log.app (fun m ->
          m "offset: %a, length: %d, kind: %a" Optint.Int63.pp offset length
            Irmin_pack.Pack_value.Kind.pp kind);
      return_unit

let () =
  setup_logs ();
  Lwt_main.run @@ main store_path
