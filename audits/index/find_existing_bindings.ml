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
end

module Value = struct
  type t = string [@@deriving repr]

  let t = Repr.string
  let encode s = s

  (** The size used in irmin-pack *)
  let encoded_size = (64 / 8) + (32 / 8) + 1

  let decode s off = String.sub s off encoded_size
end

module Index = Index_unix.Make (Key) (Value) (Index.Cache.Unbounded)
module IntMap = Map.Make (Int)

let get_keys index =
  let keys = ref IntMap.empty in
  let i = ref 0 in
  Index.iter
    (fun key _value ->
      keys := IntMap.add !i key !keys;
      i := !i + 1)
    index;
  !keys

let main find_count index_path =
  (* open the index *)
  let index = Index.v ~readonly:false ~log_size:2_500_000 index_path in

  (* fill index with random key/values and return a set of keys *)
  let keys = get_keys index in

  let count = IntMap.cardinal keys in

  Logs.info (fun m -> m "Got %d keys from index" count);

  (* close the index *)
  Index.close index;

  (* re-open the index *)
  let index = Index.v ~readonly:false ~log_size:2_500_000 index_path in

  let random_keys =
    Seq.forever (fun () -> Random.int count)
    |> Seq.map (fun i -> IntMap.find i keys)
  in

  Logs.info (fun m -> m "Finding %d random keys" find_count);

  random_keys |> Seq.take find_count
  |> Seq.iter (fun key ->
         let[@landmark "index_find"] _ = Index.find index key in
         ());

  return_unit

let () =
  setup_logs ();

  let find_count = int_of_string @@ Sys.argv.(1) in
  let index_path = Sys.argv.(2) in

  Lwt_main.run @@ main find_count index_path
