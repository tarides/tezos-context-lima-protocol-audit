(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

open Lwt

let setup_logs () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level @@ Some Info)

module Key = struct
  type t = string [@@deriving repr]

  let t = Repr.string
  let equal = String.equal
  let hash = Hashtbl.hash
  let hash_size = 30
  let encode s = s

  (** The size used in irmin-pack. For some reason it is not 32. *)
  let encoded_size = 30

  let decode s off = String.sub s off 30

  let random () =
    Seq.init encoded_size (fun _ -> Random.int 255)
    |> Seq.map Char.chr |> String.of_seq
end

module Value = struct
  type t = string [@@deriving repr]

  let t = Repr.string
  let encode s = s

  (** The size used in irmin-pack *)
  let encoded_size = (64 / 8) + (32 / 8) + 1

  let empty = String.make encoded_size @@ Char.chr 0
  let decode s off = String.sub s off encoded_size
end

module Index = Index_unix.Make (Key) (Value) (Index.Cache.Unbounded)
module IntMap = Map.Make (Int)

let fill_index index count =
  Seq.forever Key.random |> Seq.take count
  |> Seq.fold_lefti
       (fun map i key ->
         Index.replace index key Value.empty;
         IntMap.add i key map)
       IntMap.empty

let main count index_path =
  (* open the index *)
  let index = Index.v ~readonly:false ~log_size:2_500_000 index_path in

  (* fill index with random key/values and return a set of keys *)
  let keys = fill_index index count in

  (* close the index *)
  Index.close index;

  (* re-open the index *)
  let index = Index.v ~readonly:false ~log_size:2_500_000 index_path in

  let random_keys =
    Seq.forever (fun () -> Random.int count)
    |> Seq.map (fun i -> IntMap.find i keys)
  in

  random_keys |> Seq.take count
  |> Seq.iter (fun key ->
         let[@landmark "index_find"] _ = Index.find index key in
         ());

  return_unit

let () =
  setup_logs ();

  let count = int_of_string @@ Sys.argv.(1) in
  let index_path = Sys.argv.(2) in
  Lwt_main.run @@ main count index_path
