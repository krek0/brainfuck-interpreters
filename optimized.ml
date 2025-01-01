type prog =
  | Mutate of int
  | Move of int
  | Loop of prog list
  | Print
  | Read
  | Set of int
  | Add_mult of int*int (*Position of number to set, coefiscient*)
  | Move_until of int
  | If of prog

let parse code =
  let code_len = String.length code in
  let rec aux acc i =
    if i < code_len then match code.[i], acc with
      | _,Mutate 0::t -> aux t i
      | _,Move 0::t -> aux t i

      | '+',Mutate v::t -> aux (Mutate (v+1)::t) (i+1)
      | '+',_ -> aux (Mutate 1::acc) (i+1)
      | '-',Mutate v::t -> aux (Mutate (v-1)::t) (i+1)
      | '-',_ -> aux (Mutate (-1)::acc) (i+1)
      
      | '>',Move v::t -> aux (Move (v+1)::t) (i+1)
      | '>',_ -> aux (Move 1::acc) (i+1)
      | '<',Move v::t -> aux (Move (v-1)::t) (i+1)
      | '<',_ -> aux (Move (-1)::acc) (i+1)

      | '[',_ ->
        let loop_content,i' = aux [] (i+1) in
        aux (Loop loop_content::acc) (i'+1)
      | ']',_ -> (List.rev acc),i
      
      | '.',_ -> aux (Print::acc) (i+1)
      | ',',_ -> aux (Read::acc) (i+1)
      | _ -> aux acc (i+1)
    else
      (List.rev acc),i
  in
  fst @@ aux [] 0


let run prog tape_size =
  let tape = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout tape_size in
  for i = 0 to tape_size - 1 do
    tape.{i} <- 0;
  done;
  let rec aux prog tape_pos =
    match prog with
    | Mutate i::t ->
     tape.{tape_pos} <- tape.{tape_pos} + i;
      aux t tape_pos
    | Move i::t -> aux t (tape_pos+i)
    | Loop l::t ->
      let rec loop tape_pos =
        if tape.{tape_pos} <> 0 then loop (aux l tape_pos)
        else aux t tape_pos
      in loop tape_pos
    | Print::t ->
      print_char @@ Char.chr tape.{tape_pos};
      flush stdout;
      aux t tape_pos
    | Read::t -> 
     tape.{tape_pos} <- (Char.code @@ input_char stdin);
      aux t tape_pos
    | Add_mult (pos,a)::t ->
     tape.{tape_pos + pos} <- tape.{tape_pos + pos} + tape.{tape_pos}*a;
     tape.{tape_pos} <- 0;
      aux t tape_pos
    | Set i::t ->
     tape.{tape_pos} <- i;
      aux t tape_pos
    | Move_until i::t ->
      let a = ref tape_pos in while tape.{!a} !=0 do a := !a+i done;
      aux t !a
    | If p::t ->
      if tape.{tape_pos} <> 0 then
        aux (p::prog) tape_pos
      else
        aux t tape_pos
    | [] -> tape_pos
  in
  ignore @@ aux prog 0

let optimize prog =
  let rec aux acc prog = 
    match prog with
    (*Loop*)
    | Loop[]::t -> aux acc t (*Remove empty loop*)
    | Loop[Move i]::t -> aux (Move_until i::acc) t (*Optimise [>>]*)
    | Loop[Mutate (-1)]::t -> aux (Set 0::acc) t (*Optimise [-]*)
    | h::t -> aux (h::acc) t
    | [] -> List.rev acc
  in
  aux [] prog

let interpret code tape_size =
  let prog = parse code in
  let prog = optimize prog in
  run prog tape_size
  
let read_file f =
  In_channel.with_open_bin f In_channel.input_all

let () =
  let args = Sys.argv in
  if Array.length args = 2 && Sys.file_exists args.(1) then
    let code = read_file args.(1) in
    interpret code 30_000
  else
    let rec aux i acc =
      if i = Array.length args then acc
      else aux (i+1) (acc^args.(i))
    in
    let code = aux 0 "" in
    interpret code 30_000
