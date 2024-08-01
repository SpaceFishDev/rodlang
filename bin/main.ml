let read_file fn =
  if Sys.file_exists fn then
    (let ch = open_in_bin fn in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s )
  else
    failwith ("File "^fn^" Doesnt Exist!") 

let help_msg = "HELP:\ncomp [file] -> Compiles a file to a.out.\nhelp -> Prints this help message."

type token_type = NUM | KEYWORD | NONE | STRING | EQ | PLUS | MINUS | DIVIDE | MULTIPLY | OPENBLOCK | GREATER | LESS | AND | CLOSEBLOCK | SQROPEN | SQRCLOSE | BRACKETOPEN | BRACKETCLOSE
type token = {_type: token_type; ln: int; col: int; value: string}
type lexer_data = {src: string; pos: int; line: int; col: int}

let token_init tp ln col value = 
  {_type = tp; ln = ln; col=col; value=value}

let string_of_token_type = function 
| NUM -> "Number"
| NONE -> "None"
| KEYWORD -> "Keyword"
| STRING -> "String"
| EQ -> "Equals"
| PLUS -> "Plus"
| MINUS -> "Minus"
| DIVIDE -> "Divide"
| MULTIPLY -> "Multiply"
| OPENBLOCK -> "Open-block"
| CLOSEBLOCK -> "Close-block"
| SQROPEN -> "Sqr-Open"
| SQRCLOSE -> "Sqr-Close"
| AND -> "And"
| GREATER -> "Greater"
| LESS -> "Less"
| BRACKETOPEN -> "Bracket-Open"
| BRACKETCLOSE -> "Bracket-Close"

let string_of_token t = 
  Printf.sprintf "TOKEN [ln: %d, col: %d]: [%s]=[%s]" t.ln t.col (string_of_token_type t._type) t.value

let rec lex_number lex_dat temp =( 
  if lex_dat.pos < String.length lex_dat.src then
    match lex_dat.src.[lex_dat.pos] with
    | c when c >= '0' && c <= '9'-> (lex_number ({src = lex_dat.src; pos = lex_dat.pos+1; line = lex_dat.line; col = lex_dat.col+1}) (temp^(String.make 1 c)))
    | c when c = '.' -> (lex_number ({src = lex_dat.src; pos = lex_dat.pos+1; line = lex_dat.line; col = lex_dat.col+1}) (temp^(String.make 1 c)))
    | _ -> (token_init NUM lex_dat.line lex_dat.col temp)
  else 
    (token_init NUM lex_dat.line lex_dat.col temp)
)

let is_keyword_char ch = 
  match ch with
  | c when c >= 'a' && c <= 'z' -> true
  | c when c >= 'A' && c <= 'Z' -> true
  | c when c >= '0' && c <= '9' -> true
  | '_' -> true
  | _ -> false

let rec lex_keyword lex_dat temp = 
  if lex_dat.pos < String.length lex_dat.src then
    match lex_dat.src.[lex_dat.pos] with
    | c when (is_keyword_char c) -> lex_keyword {src = lex_dat.src; pos = lex_dat.pos+1; line = lex_dat.line; col = lex_dat.col+1;} (temp^(String.make 1 c))
    | _ -> token_init KEYWORD lex_dat.line lex_dat.col temp
  else 
    token_init KEYWORD lex_dat.line lex_dat.col temp

let rec lex_string lex_dat temp = 
  if lex_dat.pos < String.length lex_dat.src then
    match lex_dat.src.[lex_dat.pos] with
    | '"' | '\'' -> token_init STRING lex_dat.line lex_dat.col temp
    | c -> lex_string {src = lex_dat.src; pos = lex_dat.pos+1; line = lex_dat.line; col = lex_dat.col+1} (temp^(String.make 1 c))
  else
    token_init STRING lex_dat.line lex_dat.col temp

let lex_single lex_dat = 
  if lex_dat.pos < String.length lex_dat.src then 
    match lex_dat.src.[lex_dat.pos] with
    | c when c >= '0' && c <= '9' -> lex_number lex_dat ""
    | c when c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' -> lex_keyword lex_dat ""
    | '"' | '\'' -> lex_string {src = lex_dat.src; pos = lex_dat.pos + 1; line = lex_dat.line; col = lex_dat.col+1} ""
    | '=' -> token_init EQ lex_dat.line (lex_dat.col+1) "="
    | '+' -> token_init PLUS lex_dat.line (lex_dat.col+1) "+"
    | '-' -> token_init MINUS lex_dat.line (lex_dat.col+1) "-"
    | '/' -> token_init DIVIDE lex_dat.line (lex_dat.col+1) "/"
    | '*' -> token_init MULTIPLY lex_dat.line (lex_dat.col+1) "*"
    | '{' -> token_init OPENBLOCK lex_dat.line (lex_dat.col+1) "{"
    | '}' -> token_init CLOSEBLOCK lex_dat.line (lex_dat.col+1) "}"
    | '[' -> token_init SQROPEN lex_dat.line (lex_dat.col+1) "["
    | ']' -> token_init SQRCLOSE lex_dat.line (lex_dat.col+1) "]"
    | '&' -> token_init AND lex_dat.line (lex_dat.col+1) "&"
    | '>' -> token_init GREATER lex_dat.line (lex_dat.col+1) ">"
    | '<' -> token_init LESS lex_dat.line (lex_dat.col+1) "<"
    | '(' -> token_init BRACKETOPEN lex_dat.line (lex_dat.col+1) "("
    | ')' -> token_init BRACKETCLOSE lex_dat.line (lex_dat.col+1) ")"
    | _ -> token_init NONE lex_dat.line lex_dat.col "NONE"
  else 
    token_init NONE lex_dat.line lex_dat.col "NONE"
let rec lex_all lex_dat tokens =
  if lex_dat.pos >= String.length lex_dat.src then
    tokens
  else 
    if lex_dat.src.[lex_dat.pos] = ' ' || lex_dat.src.[lex_dat.pos] = '\t' then
      (lex_all ({src = lex_dat.src; pos = lex_dat.pos + 1; col = lex_dat.col+1; line = lex_dat.line}) tokens)
    else
      if lex_dat.src.[lex_dat.pos] = '\n' then 
        (lex_all ({src = lex_dat.src; pos = lex_dat.pos + 1; col = 0; line = lex_dat.line+1}) tokens)
      else
        let tok = lex_single lex_dat in 
        let diff = String.length tok.value in 
        print_endline (string_of_token tok);
        match tok._type with
        | NONE -> tokens
        | STRING -> (lex_all ({src = lex_dat.src; pos = lex_dat.pos + diff + 2; col = tok.col + 2; line = tok.ln}) tokens@[tok])
        | _ -> (lex_all ({src = lex_dat.src; pos = lex_dat.pos + diff; col = tok.col; line = tok.ln}) tokens@[tok])

let string_of_token_list ls = 
    let buf = Buffer.create 2000 in
    List.iter (fun tok -> Buffer.add_string buf ((string_of_token tok)^"\n")) ls;
    Buffer.contents buf

type node_type = PROG | FUNCTION | ARGS | VARNAME | BLOCK | NONE | TYPENAME | RETURN_TYPE
type node = {_type: node_type;  _token: token; children: node list}

let string_of_node_type = function
| PROG -> "Program"
| FUNCTION -> "Function"
| ARGS -> "Args"
| VARNAME -> "Var Name" 
| BLOCK -> "Block"
| NONE -> "None"
| TYPENAME -> "Type Name"
| RETURN_TYPE -> "Return Type"

let rec string_of_node_impl n level =
  let buf = Buffer.create 0 in 
  
  if List.length n.children > 0 then(
    Buffer.add_string buf (indent level (Printf.sprintf "NODE: [%s]=[%s]\n" (string_of_node_type n._type) (string_of_token n._token)));
    List.iter (fun child -> Buffer.add_string buf (string_of_node_impl child (level+1))) n.children;
  Buffer.contents buf)
else
  indent level (Printf.sprintf "NODE: [%s]=[%s]\n" (string_of_node_type n._type) (string_of_token n._token))
  and indent x temp = 
    if x = 0 then
      temp
    else 
      indent (x-1) "    "^temp
      
let string_of_node n = 
  string_of_node_impl n 0
 
let init_node _type _token _children = 
  {_type = _type; _token = _token; children = _children}


let parse_var (tokens : token list) : node * int =
  match tokens with
  | head::tail when head._type = KEYWORD ->( match tail with
  | h::t when h._type = SQROPEN ->(
        match t with
        | h::_ when h._type = SQRCLOSE -> (init_node VARNAME {_type = head._type; value = head.value^"[]"; col = head.col; ln = head.ln;} [],3)
        | _ -> failwith "Expected ']'"
    )
      | _ -> (init_node VARNAME head [],1)
  )
  | h::_ -> (init_node NONE h [],1)
  | [] -> failwith "Empty"

let rec list_offset ls off = 
  if off = 0 then
    ls
  else
    match ls with 
    | _::t -> (list_offset t (off-1))
    | [] -> ls
let rec gather_args args tokens offset = 
  match tokens with
  | [] -> (args,offset)
  | _::_ -> let (n, off) = (parse_var tokens) in
  match n._type with 
  | NONE ->  (args, offset)
  | _ -> gather_args (args@[n]) (list_offset tokens off) (offset+off)

let parse_type (tokens : token list) : node * int =
  match tokens with
  | head::tail when head._type = KEYWORD ->( match tail with
    | h::t when h._type = SQROPEN ->(
        match t with
        | h::_ when h._type = SQRCLOSE -> (init_node TYPENAME {_type = head._type; value = head.value^"[]"; col = head.col; ln = head.ln;} [],3)
        | _ -> failwith "Expected ']'"
    )
      | _ -> (init_node TYPENAME head [],1)
  )
  | h::_ -> (init_node NONE h [],1)
  | [] -> failwith "Empty"

let parse_args tokens =
  match tokens with 
  | [] -> failwith "Empty"
  | h::_ -> let (nodes, off) = (gather_args [] tokens 0) in
   ([init_node ARGS h nodes], (off+1))

let parse_block tokens = 
  ((init_node BLOCK (List.hd tokens) []), 1)

let parse_return tokens = 
  match tokens with
  | h::t when h.value = "-" -> (
    match t with 
    | h::tail when h.value = ">" -> 
    (
      match tail with 
      | h::_ when h._type = KEYWORD -> let (n,off) = parse_type tail in 
      (init_node RETURN_TYPE n._token [n], off) 
      | _ -> failwith "Expected return type"
    )
    | _ -> failwith "Expected return type"
  )
  | _ -> failwith "Expected return type"

let parse_function tokens = 
  let name = List.hd tokens in
  let (args, offset) = parse_args (List.tl tokens) in
  let (return, r_off) = parse_return (list_offset tokens offset) in
  let (block, b_off) = parse_block (list_offset tokens (offset+r_off)) in
  (init_node FUNCTION name ((args)@[return]@[block]), offset+r_off+b_off) 

let parse tokens = 
  let p = init_node PROG {_type = NONE; ln = 0; col = 0; value = "Program Token"} [] in 
  let parse_next tokens = (
  match tokens with 
  | h::t when h.value = "def" -> (
    parse_function t
  ) 
  | _ -> failwith "Not Implimented")
in 
let (n, _) = parse_next tokens in
(init_node PROG (p._token) [n]) 
let compile_begin file = 
  let file_dat = read_file file in
  let t = List.rev (lex_all {src = file_dat; pos = 0; line = 1; col = 0} []) in
  print_endline (read_file file); Printf.printf "%s\n" (string_of_token_list t); Printf.printf "%s\n" (string_of_node (parse t))



let () =
match List.tl (Array.to_list Sys.argv) with
| h::_ when h = "help" -> Printf.printf "%s\n" help_msg
| h::t when h = "comp" -> (
  match t with
  | [] -> Printf.printf "%s\n" help_msg
  | h::_ -> compile_begin h
)
| h::_ -> print_endline h
| [] -> print_endline "No arguments given\n"; print_endline help_msg
 
