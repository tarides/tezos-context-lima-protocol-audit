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

let cound_bindings index =
  let count = ref 0 in
  Index.iter (fun _key _value -> count := !count + 1) index;
  !count

let fill_index index count =
  Seq.forever Key.random |> Seq.take count
  |> Seq.iter (fun key -> Index.replace index key Value.empty)

let main index_path =
  let index = Index.v ~readonly:false ~log_size:2_500_000 index_path in

  let () = fill_index index 3_500_000 in

  let bindings = cound_bindings index in

  Logs.app (fun m -> m "Number of bindings in Index: %d" bindings);

  Index.close index;
  return_unit

let () =
  setup_logs ();

  let index_path = Sys.argv.(1) in
  Lwt_main.run @@ main index_path
