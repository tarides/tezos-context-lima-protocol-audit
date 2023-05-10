(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

module Config : sig
  type t = { actions_trace_path : string; store_path : string }

  val copy_store_to_temp_location : t -> t
end

module State : sig
  type t

  val stats : t -> Irmin_pack_unix.Stats.t
end

module Replay_actions = Tezos_context_trace.Replay_actions

type block_level = int
type hash = string

type header = Replay_actions.header = {
  initial_block : (block_level * hash) option;
  last_block : block_level * hash;
  block_count : int;
}

type block = Replay_actions.row = {
  level : int;
  tzop_count : int;
  tzop_count_tx : int;
  tzop_count_contract : int;
  tz_gas_used : int;
  tz_storage_size : int;
  tz_cycle_snapshot : int;
  tz_time : int;
  tz_solvetime : int;
  ops : Replay_actions.event array;
  uses_patch_context : bool;
}
(** Type of a serialized sequence of operations that make a Tezos
block plus some metadata. *)

val exec_block : block -> State.t -> State.t Lwt.t
val read_blocks : Config.t -> header * State.t * block Lwt_seq.t
