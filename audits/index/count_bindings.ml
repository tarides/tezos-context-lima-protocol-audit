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

let cound_bindings index =
  let count = ref 0 in
  Index.iter (fun _key _value -> count := !count + 1) index;
  !count

let main root =
  let* repo =
    Store.Repo.v
      (Irmin_pack.config ~readonly:true
         ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal root)
  in

  let fm = Store.Internal.file_manager repo in
  let index = File_manager.index fm in

  let bindings = cound_bindings index in

  Logs.app (fun m -> m "Number of bindings in Index: %d" bindings);

  return_unit

let () =
  setup_logs ();
  Lwt_main.run @@ main store_path
