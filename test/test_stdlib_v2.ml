open Stdlib_v2

type _ Effect.t +=
  | Incr : int -> int Effect.t
  | Log : string -> unit Effect.t
  | Ask : int Effect.t

let run_handler f =
  let logs = ref [] in
  let res =
    match f () with
    | v -> v
    | effect (Incr n), k -> Effect.Deep.continue k (n + 1)
    | effect (Log msg), k ->
        logs := msg :: !logs;
        Effect.Deep.continue k ()
    | effect Ask, k -> Effect.Deep.continue k 100
  in
  (res, List.rev !logs)

let () =
  (* --- 1. Option --- *)
  (* Pure *)
  assert (Option.map (fun x -> x + 1) (Some 41) = Some 42);
  assert (Option.bind (Some 41) (fun x -> Some (x + 1)) = Some 42);
  (* Effectful *)
  let res_opt, _ = run_handler (fun () ->
    Option.map (fun x -> Effect.perform (Incr x)) (Some 10)
  ) in
  assert (res_opt = Some 11);

  (* --- 2. List --- *)
  (* Pure *)
  assert (List.map (fun x -> x * 2) [1; 2; 3] = [2; 4; 6]);
  assert (List.filter (fun x -> x mod 2 = 0) [1; 2; 3; 4] = [2; 4]);
  assert (List.fold_left ( + ) 0 [1; 2; 3; 4] = 10);
  assert (List.sort Int.compare [3; 1; 4; 1; 5; 9] = [1; 1; 3; 4; 5; 9]);

  (* Effectful map *)
  let res_list, logs = run_handler (fun () ->
    List.map (fun x ->
      Effect.perform (Log (string_of_int x));
      Effect.perform (Incr x)
    ) [1; 2; 3]
  ) in
  assert (res_list = [2; 3; 4]);
  assert (logs = ["1"; "2"; "3"]);

  (* Effectful fold_left *)
  let res_fold, _ = run_handler (fun () ->
    List.fold_left (fun acc x ->
      acc + Effect.perform (Incr x)
    ) 0 [1; 2; 3]
  ) in
  assert (res_fold = 9); (* (1+1) + (2+1) + (3+1) = 2 + 3 + 4 = 9 *)

  (* Effectful sort *)
  let res_sort, _ = run_handler (fun () ->
    List.sort (fun a b ->
      let delta = Effect.perform Ask in
      Int.compare (a + delta) (b + delta)
    ) [5; 2; 8; 1]
  ) in
  assert (res_sort = [1; 2; 5; 8]);

  (* --- 3. Array --- *)
  (* Pure *)
  let arr = [| 3; 1; 2 |] in
  Array.sort Int.compare arr;
  assert (arr = [| 1; 2; 3 |]);
  assert (Array.map (fun x -> x * 10) [| 1; 2 |] = [| 10; 20 |]);

  (* Effectful Array.map *)
  let res_arr, _ = run_handler (fun () ->
    Array.map (fun x -> Effect.perform (Incr x)) [| 10; 20; 30 |]
  ) in
  assert (res_arr = [| 11; 21; 31 |]);

  (* Effectful Array.sort *)
  let arr_eff = [| 50; 20; 30 |] in
  let (), logs_arr = run_handler (fun () ->
    Array.sort (fun a b ->
      Effect.perform (Log (Printf.sprintf "cmp %d %d" a b));
      Int.compare a b
    ) arr_eff
  ) in
  assert (arr_eff = [| 20; 30; 50 |]);
  assert (List.length logs_arr > 0);

  (* --- 4. Result --- *)
  assert (Result.map (fun x -> x + 1) (Ok 10) = Ok 11);
  let res_res, _ = run_handler (fun () ->
    Result.map (fun x -> Effect.perform (Incr x)) (Ok 99)
  ) in
  assert (res_res = Ok 100);

  (* --- 5. Seq --- *)
  let seq = List.to_seq [1; 2; 3] in
  let sum = ref 0 in
  Seq.iter (fun x -> sum := !sum + x) seq;
  assert (!sum = 6);

  let res_seq_fold, logs_seq = run_handler (fun () ->
    Seq.fold_left (fun acc x ->
      Effect.perform (Log ("seq_" ^ string_of_int x));
      acc + Effect.perform (Incr x)
    ) 0 seq
  ) in
  assert (res_seq_fold = 9);
  assert (logs_seq = ["seq_1"; "seq_2"; "seq_3"]);

  (* Effectful Seq *)
  let eff_seq : (int, -[ Incr, Log ]-) Seq.eff =
    fun () ->
      Effect.perform (Log "gen_1");
      Seq.Cons (Effect.perform (Incr 10), fun () ->
        Effect.perform (Log "gen_2");
        Seq.Cons (Effect.perform (Incr 20), fun () -> Seq.Nil))
  in
  let res_eff_list, logs_eff_seq = run_handler (fun () ->
    List.of_seq (Seq.take 2 eff_seq)
  ) in
  assert (res_eff_list = [11; 21]);
  assert (logs_eff_seq = ["gen_1"; "gen_2"]);

  (* Test interoperability of Seq.empty, Seq.return, List.to_seq, Array.to_seq, Option.to_seq with eff_seq *)
  let res_interop, _ = run_handler (fun () ->
    let s1 = Seq.append Seq.empty eff_seq in
    let s2 = Seq.append (Seq.return 1) s1 in
    let s3 = Seq.append (List.to_seq [2; 3]) s2 in
    let s4 = Seq.append (Array.to_seq [| 4; 5 |]) s3 in
    let s5 = Seq.append (Option.to_seq (Some 6)) s4 in
    let arr = Array.of_seq (Seq.take 8 s5) in
    arr
  ) in
  assert (res_interop = [| 6; 4; 5; 2; 3; 1; 11; 21 |]);

  (* Test Array.to_seqi *)
  let arr_seqi = Array.to_seqi [| "a"; "b" |] in
  assert (List.of_seq arr_seqi = [(0, "a"); (1, "b")]);

  (* Test Option.to_seq with None *)
  assert (List.of_seq (Option.to_seq None) = []);


  (* --- 6. Fun (protect) --- *)
  let finally_called = ref false in
  let res_prot, _ = run_handler (fun () ->
    Fun.protect
      ~finally:(fun () -> finally_called := true)
      (fun () -> Effect.perform (Incr 41))
  ) in
  assert (res_prot = 42);
  assert (!finally_called);

  print_endline "All Stdlib_v2 tests passed successfully!"
