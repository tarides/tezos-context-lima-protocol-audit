(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: CECILL-B
 *)

(* Inspired by https://github.com/pveber/binning which is released
   under the terms of the CECILL-B license. *)

module type S = sig
  type bin
  type ('a, 'increment, 'summary) t

  val create :
    projection:('a -> bin) ->
    add:('increment -> 'summary option -> 'summary option) ->
    ('a, 'increment, 'summary) t

  val add :
    'a ->
    'increment ->
    ('a, 'increment, 'summary) t ->
    ('a, 'increment, 'summary) t

  val to_seq : (_, _, 'summary) t -> (bin * 'summary) Seq.t
end

module Make (Map : Map.S) : S with type bin = Map.key
