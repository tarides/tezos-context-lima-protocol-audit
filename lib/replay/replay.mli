(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

module Config : sig
  type t = { actions_trace_path : string; store_path : string }

  val copy_store_to_temp_location : t -> t
end

module Context = Tezos_context_disk.Context

module State : sig
  type t

  val stats : t -> Irmin_pack_unix.Stats.t
  val index : t -> Context.index option
  val last_commit : t -> Tezos_crypto.Hashed.Context_hash.t option
end

module Replay_actions = Tezos_context_trace.Replay_actions

type block_level = int
type hash = string

module Header : sig
  type t = Replay_actions.header = {
    initial_block : (block_level * hash) option;
    last_block : block_level * hash;
    block_count : int;
  }
end

module Operation : sig
  type t = Replay_actions.event

  val pp : t Fmt.t
  val is_split : t -> bool
  val is_gc : t -> bool

  val exec : t -> State.t -> State.t Lwt.t
  (** [exec op state] execute the operation [op] and return the state after operation is executed. *)
end

module Block : sig
  type t = Replay_actions.row = {
    level : int;
    tzop_count : int;
    tzop_count_tx : int;
    tzop_count_contract : int;
    tz_gas_used : int;
    tz_storage_size : int;
    tz_cycle_snapshot : int;
    tz_time : int;
    tz_solvetime : int;
    ops : Operation.t array;
    uses_patch_context : bool;
  }
  (** Type of a serialized sequence of operations that make a Tezos
block plus some metadata. *)

  val exec : t -> State.t -> State.t Lwt.t
  (** [exec block state] executes the operations in [block] and returns the state after operations are executed. *)
end

val read_blocks : Config.t -> Header.t * State.t * Block.t Lwt_seq.t
