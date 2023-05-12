(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

open Lwt
(* open Lwt.Syntax *)

(* Logging *)

let setup_logs () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level @@ Some Info)

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

let main index_path =
  let index =
    Index.v ~readonly:true ~log_size:2_500_000 index_path |> Result.get_ok
  in

  let bindings = cound_bindings index in

  Logs.app (fun m -> m "Number of bindings in Index: %d" bindings);

  return_unit

let () =
  setup_logs ();

  let index_path = Sys.argv.(1) in
  Lwt_main.run @@ main index_path
