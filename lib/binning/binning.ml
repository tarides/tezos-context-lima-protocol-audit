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

module Make (Map : Map.S) = struct
  type bin = Map.key

  type ('a, 'increment, 'summary) t = {
    bins : 'summary Map.t;
    projection : 'a -> bin;
    add : 'increment -> 'summary option -> 'summary option;
  }

  let create ~projection ~add = { bins = Map.empty; projection; add }

  let add (x : 'a) (y : 'increment) (b : ('a, 'increment, 'summary) t) :
      ('a, 'increment, 'summary) t =
    let bin = b.projection x in
    let bins = Map.update bin (b.add y) b.bins in
    { b with bins }

  let to_seq b = Map.to_seq b.bins
end
