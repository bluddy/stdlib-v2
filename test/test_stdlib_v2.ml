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
  (* --- 7. String --- *)
  assert (String.map Char.uppercase_ascii "abc" = "ABC");
  let str_eff, logs_str = run_handler (fun () ->
    String.map (fun c ->
      Effect.perform (Log (String.make 1 c));
      Char.chr (Char.code c + 1)
    ) "abc"
  ) in
  assert (str_eff = "bcd");
  assert (logs_str = ["a"; "b"; "c"]);

  let str_seq = String.to_seq "hello" in
  assert (String.of_seq str_seq = "hello");

  let str_seqi = String.to_seqi "ab" in
  assert (List.of_seq str_seqi = [(0, 'a'); (1, 'b')]);

  let res_str_fold, _ = run_handler (fun () ->
    String.fold_left (fun acc c ->
      acc + Effect.perform (Incr (Char.code c))
    ) 0 "A"
  ) in
  assert (res_str_fold = 66); (* 65 + 1 = 66 *)

  (* --- 8. Bytes --- *)
  let b = Bytes.of_string "foo" in
  assert (Bytes.map Char.uppercase_ascii b = Bytes.of_string "FOO");
  let bytes_eff, _ = run_handler (fun () ->
    Bytes.map (fun c ->
      let delta = Effect.perform Ask in
      Char.chr (Char.code c + (delta - 100))
    ) b
  ) in
  assert (Bytes.to_string bytes_eff = "foo");

  let b_seq = Bytes.to_seq (Bytes.of_string "world") in
  assert (Bytes.to_string (Bytes.of_seq b_seq) = "world");

  (* --- 9. In_channel / Out_channel --- *)
  let temp_path = Filename.temp_file "stdlib_v2_test" ".txt" in
  let () =
    let _, _ = run_handler (fun () ->
      Out_channel.with_open_text temp_path (fun oc ->
        Effect.perform (Log "writing");
        Out_channel.output_string oc "line1\nline2\n"
      )
    ) in
    let lines, _ = run_handler (fun () ->
      In_channel.with_open_text temp_path (fun ic ->
        Effect.perform (Log "reading");
        In_channel.fold_lines (fun acc line ->
          Effect.perform (Incr 0) |> ignore;
          line :: acc
        ) [] ic
      )
    ) in
    assert (lines = ["line2"; "line1"]);
    Sys.remove temp_path
  in

  (* --- 10. Either --- *)
  let e1 = Either.Left 10 in
  let e2 = Either.Right "hello" in
  let res_e1, _ = run_handler (fun () ->
    Either.map_left (fun x -> Effect.perform (Incr x)) e1
  ) in
  assert (res_e1 = Either.Left 11);
  let res_e2, _ = run_handler (fun () ->
    Either.map_right (fun s -> Effect.perform (Log s); s ^ "!") e2
  ) in
  assert (res_e2 = Either.Right "hello!");

  (* --- 11. Buffer --- *)
  let buf = Buffer.create 16 in
  let (), _ = run_handler (fun () ->
    Buffer.add_substitute buf (fun var ->
      Effect.perform (Log var);
      string_of_int (Effect.perform Ask)
    ) "Value is $val!"
  ) in
  assert (Buffer.contents buf = "Value is 100!");
  let buf_seq = Buffer.to_seq buf in
  let buf2 = Buffer.of_seq buf_seq in
  assert (Buffer.contents buf2 = "Value is 100!");

  (* --- 12. Queue --- *)
  let q = Queue.create () in
  Queue.push 1 q;
  Queue.push 2 q;
  let q_sum = ref 0 in
  let (), _ = run_handler (fun () ->
    Queue.iter (fun x ->
      q_sum := !q_sum + Effect.perform (Incr x)
    ) q
  ) in
  assert (!q_sum = 5); (* (1+1) + (2+1) = 5 *)
  let q_seq = Queue.to_seq q in
  let q_from_seq = Queue.of_seq q_seq in
  assert (Queue.length q_from_seq = 2);
  assert (Queue.pop q_from_seq = 1);
  assert (Queue.pop q_from_seq = 2);

  (* --- 13. Stack --- *)
  let stk = Stack.create () in
  Stack.push 10 stk;
  Stack.push 20 stk;
  let stk_sum = ref 0 in
  let (), _ = run_handler (fun () ->
    Stack.iter (fun x ->
      stk_sum := !stk_sum + Effect.perform (Incr x)
    ) stk
  ) in
  assert (!stk_sum = 32); (* (20+1) + (10+1) = 32 *)
  let stk_from_seq = Stack.of_seq (Stack.to_seq stk) in
  assert (Stack.pop stk_from_seq = 10);
  assert (Stack.pop stk_from_seq = 20);

  (* --- 14. Hashtbl --- *)
  let ht = Hashtbl.create 8 in
  Hashtbl.add ht "a" 1;
  Hashtbl.add ht "b" 2;
  let ht_sum = ref 0 in
  let (), _ = run_handler (fun () ->
    Hashtbl.iter (fun _k v ->
      ht_sum := !ht_sum + Effect.perform (Incr v)
    ) ht
  ) in
  assert (!ht_sum = 5); (* 2 + 3 = 5 *)

  let (), _ = run_handler (fun () ->
    Hashtbl.filter_map_inplace (fun _k v ->
      Some (Effect.perform (Incr v))
    ) ht
  ) in
  assert (Hashtbl.find ht "a" = 2);
  assert (Hashtbl.find ht "b" = 3);

  let ht_seq = Hashtbl.to_seq ht in
  let ht_copy = Hashtbl.of_seq ht_seq in
  assert (Hashtbl.find ht_copy "a" = 2);
  assert (Hashtbl.find ht_copy "b" = 3);

  (* Hashtbl.Make *)
  let module IntHashtbl = Hashtbl.Make (struct
    type t = int
    let equal = Int.equal
    let hash = Hashtbl.hash
  end) in
  let iht = IntHashtbl.create 8 in
  IntHashtbl.add iht 42 "forty-two";
  let iht_seq = IntHashtbl.to_seq iht in
  let iht_copy = IntHashtbl.of_seq iht_seq in
  assert (IntHashtbl.find iht_copy 42 = "forty-two");

  (* --- 15. Map --- *)
  let module IntMap = Map.Make (Int) in
  let m = IntMap.empty |> IntMap.add 1 "one" |> IntMap.add 2 "two" in
  let m_eff, _ = run_handler (fun () ->
    IntMap.map (fun v ->
      let d = Effect.perform Ask in
      v ^ string_of_int d
    ) m
  ) in
  assert (IntMap.find 1 m_eff = "one100");
  assert (IntMap.find 2 m_eff = "two100");

  let m_seq = IntMap.to_seq m in
  let m_copy = IntMap.of_seq m_seq in
  assert (IntMap.equal ( = ) m m_copy);

  let (found_k, found_v) = run_handler (fun () ->
    IntMap.find_first (fun k ->
      Effect.perform (Log ("check " ^ string_of_int k));
      k >= 2
    ) m
  ) |> fst in
  assert (found_k = 2);
  assert (found_v = "two");

  (* --- 16. Set --- *)
  let module IntSet = Set.Make (Int) in
  let set = IntSet.empty |> IntSet.add 1 |> IntSet.add 2 |> IntSet.add 3 in
  let set_eff, _ = run_handler (fun () ->
    IntSet.map (fun x -> Effect.perform (Incr x)) set
  ) in
  assert (IntSet.elements set_eff = [2; 3; 4]);

  let set_seq = IntSet.to_seq set in
  let set_copy = IntSet.of_seq set_seq in
  assert (IntSet.equal set set_copy);

  let first_even = run_handler (fun () ->
    IntSet.find_first (fun x ->
      Effect.perform (Log ("set_check " ^ string_of_int x));
      x mod 2 = 0
    ) set
  ) |> fst in
  assert (first_even = 2);

  print_endline "All Stdlib_v2 tests passed successfully!"
