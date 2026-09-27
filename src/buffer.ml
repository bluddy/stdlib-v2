include Stdlib.Buffer

let advance_to_closing opening closing k s start =
  let rec advance k i lim =
    if i >= lim then raise Not_found else
    if s.[i] = opening then advance (k + 1) (i + 1) lim else
    if s.[i] = closing then
      if k = 0 then i else advance (k - 1) (i + 1) lim
    else advance k (i + 1) lim in
  advance k start (String.length s)

let advance_to_non_alpha s start =
  let rec advance i lim =
    if i >= lim then lim else
    match s.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> advance (i + 1) lim
    | _ -> i in
  advance start (Stdlib.String.length s)

let find_ident s start lim =
  if start >= lim then raise Not_found else
  match s.[start] with
  | '(' | '{' as c ->
     let new_start = start + 1 in
     let stop = advance_to_closing c (if c = '(' then ')' else '}') 0 s new_start in
     Stdlib.String.sub s new_start (stop - start - 1), stop + 1
  | _ ->
     let stop = advance_to_non_alpha s start in
     if stop = start then raise Not_found else
     Stdlib.String.sub s start (stop - start), stop

let add_substitute : 'e. t -> (string -['e]-> string) -> string -['e]-> unit =
  fun b f s ->
    let lim = Stdlib.String.length s in
    let rec subst previous i =
      if i < lim then begin
        match s.[i] with
        | '$' as current when previous = '\\' ->
           add_char b current;
           subst ' ' (i + 1)
        | '$' ->
           let j = i + 1 in
           begin match find_ident s j lim with
           | ident, next_i ->
             add_string b (f ident);
             subst ' ' next_i
           | exception Not_found ->
             add_char b '$';
             subst ' ' j
           end
        | current ->
           if previous = '\\' then add_char b previous;
           if current <> '\\' then add_char b current;
           subst current (i + 1)
      end else
      if previous = '\\' then add_char b previous in
    subst ' ' 0

let to_seq b = Seq.of_stdlib_seq (Stdlib.Buffer.to_seq b)

let to_seqi b = Seq.of_stdlib_seq (Stdlib.Buffer.to_seqi b)

let add_seq : 'e. t -> (char, 'e) Seq.eff -['e]-> unit =
  fun b seq ->
    let f : char -['e]-> unit = fun c -> add_char b c in
    Seq.iter_eff f seq

let of_seq : 'e. (char, 'e) Seq.eff -['e]-> t =
  fun seq ->
    let b = create 32 in
    add_seq b seq;
    b
