(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

let setup_logs () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level @@ Some Info)

let block_find_split_gc (block : Replay.Block.t) =
  Array.fold_left
    (fun (has_split, has_gc) op ->
      ( has_split || Replay.Operation.is_split op,
        has_gc || Replay.Operation.is_gc op ))
    (false, false) block.ops

let run (config : Replay.Config.t) =
  let header, _state, actions = Replay.read_blocks config in

  Logs.info (fun m -> m "Blocks in replay: %d." header.block_count);

  actions
  |> Lwt_seq.iter (fun (block : Replay.Block.t) ->
         let has_split, has_gc = block_find_split_gc block in
         if has_split || has_gc then
           Logs.app (fun m ->
               m "Level: %d, Operation count: %d, has_split: %b, has_gc: %b"
                 block.level (Array.length block.ops) has_split has_gc)
         else ())

let () =
  setup_logs ();

  let actions_trace_path = "/home/adatario/dev/tclpa/inputs/actions.trace" in
  let store_path = "/home/adatario/dev/tclpa/inputs/store-level-2981990" in

  (* let store_path = "/tmp/tezos-context-store-113038" in *)
  let (config : Replay.Config.t) =
    { store_path; actions_trace_path }
    |> Replay.Config.copy_store_to_temp_location
  in

  Lwt_main.run @@ run config
