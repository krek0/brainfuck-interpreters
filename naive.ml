
let interpret code tape_size =
  let tape = Array.make tape_size 0 in
  let code_len = String.length code in

  (* Match "[" with "]" in a hash table*)
  let loops = Hashtbl.create 0 in
  let rec match_loops i loop_stack =
    if i < code_len then match code.[i] with
      | '[' -> match_loops (i+1) (i::loop_stack)
      | ']' -> (match loop_stack with
                | [] -> failwith "Unclosed loop detected."
                | h::t -> (Hashtbl.replace loops h i; Hashtbl.replace loops i h; match_loops (i+1) t))
      | _ -> match_loops (i+1) loop_stack
    else if loop_stack <> [] then
      failwith "No open loop found."
  in
  match_loops 0 [];
    
  let rec run code_pos tape_pos =
    if code_pos != code_len then match code.[code_pos] with
      | '>' -> run (code_pos+1) (tape_pos+1)
      | '<' -> run (code_pos+1) (tape_pos-1)
      
      | '+' -> tape.(tape_pos) <- tape.(tape_pos)+1;
               run (code_pos+1) tape_pos      
      | '-' -> tape.(tape_pos) <- tape.(tape_pos)-1;
               run (code_pos+1) tape_pos
             
      | '.' -> print_char @@ Char.chr tape.(tape_pos);
               flush stdout;
               run (code_pos+1) tape_pos
      | ',' -> tape.(tape_pos) <- Char.code @@ input_char stdin;
               run (code_pos+1) tape_pos

      | '[' -> if tape.(tape_pos) = 0 then
                 run (Hashtbl.find loops code_pos + 1) tape_pos
               else
                 run (code_pos+1) tape_pos
      | ']' -> if tape.(tape_pos) = 0 then
                 run (code_pos+1) tape_pos
               else
                 run (Hashtbl.find loops code_pos + 1) tape_pos

      | _ -> run (code_pos+1) tape_pos
  in
  run 0 0

let read_file f =
  In_channel.with_open_bin f In_channel.input_all

let () =
  let args = Sys.argv in
  if Array.length args = 2 && Sys.file_exists args.(1) then
    let code = read_file args.(1) in
    interpret code 30_000
  else
    let rec run i acc =
      if i = Array.length args then acc
      else run (i+1) (acc^args.(i))
    in
    let code = run 0 "" in
    interpret code 30_000
