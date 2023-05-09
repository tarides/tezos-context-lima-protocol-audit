(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

(* Logging *)

let src = Logs.Src.create "tlpa"

module Log = (val Logs.src_log src : Logs.LOG)

let setup_logs () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level @@ Some Info)

(* Actions trace *)

module Replay_actions = Tezos_context_trace.Replay_actions

let actions_trace_path = "/home/adatario/dev/tclpa/inputs/actions.trace"

(* let pp_row = Repr.pp Tezos_context_trace.Replay_actions.row_t *)
(* let pp_event = Repr.pp Replay_actions.event_t *)

let () =
  setup_logs ();

  let (config : Config.t) =
    { store_path = "/tmp/tlpa-store"; actions_trace_path }
  in

  Lwt_main.run @@ Replay.run config
