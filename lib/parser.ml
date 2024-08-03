open Lexer

type node_type = PROG | INDEX | FUNCTION | ARGS | VARNAME | BLOCK | NONE | CALL | ELSE |  TYPENAME | RETURN_TYPE | ELIF | BASIC_EXPR | DEREF | IF | BIN_EXPR | VARIABLE_DECL | ASSIGNMENT | ARR_DECL
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
| BASIC_EXPR -> "Basic Expression"
| BIN_EXPR -> "Binary Expression"
| VARIABLE_DECL -> "Variable Declaration"
| ASSIGNMENT -> "Assignment"
| ARR_DECL -> "Array Declaration"
| DEREF -> "Dereference"
| IF -> "If Statement"
| CALL -> "Function Call"
| INDEX -> "Index"
| ELSE -> "Else Statement"
| ELIF -> "Else If Statement"
let rec string_of_node_impl n level =
  let buf = Buffer.create 0 in 
  
  if List.length n.children > 0 then(
    Buffer.add_string buf (indent level (Printf.sprintf "NODE: [%s]=[%s]\n" (string_of_node_type n._type) (string_of_token n._token)));
    List.iter (fun child -> Buffer.add_string buf ( string_of_node_impl child (level+1))) n.children;
  Buffer.contents buf)
else
  indent level (Printf.sprintf "NODE: [%s]=[%s]\n" (string_of_node_type n._type) (string_of_token n._token))
  and indent x temp = 
    if x = 0 then
      temp
    else 
      indent (x-1) "|-> "^temp
      
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
        | h::t when h._type = NUM ->  (let idx = init_node INDEX h [] in
        match t with
        | h::_ when h._type = SQRCLOSE -> (init_node VARNAME head [idx],4)
        | _ -> failwith "Expected ']'" 
        )
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
    | h::_ when h._type = MULTIPLY -> (init_node TYPENAME {_type = head._type; value = head.value^"*"; col = head.col; ln = head.ln} [],2)
      | _ -> (init_node TYPENAME head [],1)
  )
  | h::_ -> (init_node NONE h [],1)
  | [] -> failwith "Empty"

let parse_basic_expression (tokens : token list ) : node * int = 
  match tokens with
  | h::_ when h._type = NUM  || h._type = STRING -> ((init_node BASIC_EXPR h []),1)
  | h::_ when h._type = KEYWORD -> let (n,off) = parse_var tokens in
  (init_node BASIC_EXPR n._token [n], off) 
  | h::_ -> ((init_node NONE h []),1)
  | _ -> failwith "Expected expr or end of block"


let rec parse_factor (tokens : token list) : node * int =( 
  let (left, off) = parse_basic_expression tokens in
  let tokens = list_offset tokens off in
  match tokens with
  | h::t when h._type = PLUS || h._type = MINUS || h._type = MULTIPLY || h._type = DIVIDE || h._type = BOOLEQ || h._type = BOOLNEQ -> let (right, offset) = parse_factor t in
  ((init_node BIN_EXPR h [left; right]), off+offset+1)
  | _ ->  (left, off)
  )
and parse_primary (tokens : token list) : node * int = 
  let (left, off) = (parse_factor tokens) in(
  let tokens = list_offset tokens off in
  match tokens with
  | h::t when h._type = MULTIPLY || h._type = DIVIDE -> let (right, offset) = parse_factor t in
  ((init_node BIN_EXPR h [left; right]), off+offset+1)
  | _ ->  (left, off)
  )


let rec parse_expr (tokens : token list) : node * int = 
  match tokens with
  | h::_ when h.value = "}" -> (init_node NONE h [],1)
  | h::t when h._type = KEYWORD && h.value = "var" -> let (_type, offset) = parse_type t in
  let (v,off) = parse_var (list_offset t offset) in
  let (assign, off2) = (parse_assignment (list_offset t (offset+off))) in
  (init_node VARIABLE_DECL v._token [_type;assign] , off+offset+1+off2)
  | h::t when h._type = NUM || h._type = STRING || h._type = KEYWORD ->(
    match h.value with 
    | "deref" ->
      let (n,off) = parse_var t in
      (init_node DEREF h [n], off+1)
    | "if" -> 
      let (n, off) = parse_primary t in
      let (block, off2) = parse_block (list_offset t off) in
      let (if_n, offset) = (init_node IF h [n;block], off+off2) in
      let tl = list_offset t offset in(
      match tl with
      | h::_ when h.value = "elif" -> (
        let (elif,off) = parse_elif tl in
         (init_node if_n._type if_n._token (if_n.children@[elif]), offset + off)
      )
      | h::t when h.value = "else" ->
        let (block, off2) = parse_block t in
        let else_node = (init_node ELSE h [block]) in
         (init_node if_n._type if_n._token (if_n.children@[else_node]),off2+offset+1)
      | _ -> (if_n, offset+1)
        ) 
      | "call" ->
      (
        match t with 
        | h::_ when h._type = KEYWORD -> parse_call t
        | _ -> failwith "Call expected" 
      )
    | _ -> (
      match t with
      | h::_ when h.value = "=" -> let (v,off) = (parse_var tokens) in
        let (assign, off2) =  parse_assignment (list_offset tokens off) in
        (init_node VARNAME v._token [assign], off2+off)
      | hd::tail when hd.value = "[" -> (
        match tail with 
        | head::_ when head.value = "]" -> if h._type = NUM then
          (init_node ARR_DECL h [], 3)
        else
          failwith "Cant init array with non-integer token."
        | _ -> failwith "Expected closing brace."
      )
      | _ -> parse_primary tokens
    )
  )
  | h::_ -> ((init_node NONE h []),1)
  | _ -> failwith "Expected expr or end of block"
  
and accumulate_elif tokens elifs offset = 
(
  match tokens with 
  | h::_ when h.value = "elif" -> let (n,off) = parse_elif tokens in
  accumulate_elif (list_offset tokens (off)) (elifs@[n]) (offset+off)
  | _ -> (elifs, offset+1) 
)
and parse_elif (tokens : token list) : node * int = (
  let toks = tokens in
  let tokens = List.tl tokens in
  let (expr, off) = parse_expr tokens in
  let (block, off2) = parse_block (list_offset tokens (off)) in
  let tail = (list_offset tokens (off+off2)) in
  match tail with 
  | h::_ when h.value = "elif" -> let (children, offset) = accumulate_elif tail [] 0 in
  (init_node ELIF (List.hd toks) ([expr;block]@children), off+off2+offset+1)
  | _ ->  (init_node ELIF (List.hd toks) [expr;block], off+off2+1)
)
and parse_assignment (tokens : token list) : node * int = (
  match tokens with 
  | h::t when h.value = "=" -> let (n,offset) = (parse_expr t) in
  ((init_node ASSIGNMENT h [n]), offset+1) 
  | _ -> failwith "eish" 
)

and accumulate_exprs tokens exprs offset = 
  match tokens with 
  | [] -> (exprs, offset)
  | _ -> let (n,off) = parse_expr tokens in
  (
    match n._type with
    | NONE -> (exprs, (off+offset))
    | _ ->  accumulate_exprs (list_offset tokens (off)) (exprs@[n]) (offset+off)
  )

and parse_args tokens =
  match tokens with 
  | [] -> failwith "Empty"
  | h::_ -> let (nodes, off) = (gather_args [] tokens 0) in
   ([init_node ARGS h nodes], (off+1))

and parse_block tokens = 
  let (exprs, offset) = accumulate_exprs (List.tl tokens) [] 0 in
  ((init_node BLOCK (List.hd tokens) exprs), offset+1)
and accumulate_call_args tokens exprs offset = 
  match tokens with 
  | [] -> (exprs, offset)
  | _ -> let (n,off) = parse_expr tokens in
  (
    match n._type with
    | BIN_EXPR | BASIC_EXPR | CALL -> accumulate_call_args (list_offset tokens (off)) (exprs@[n]) (offset+off)
    | _ -> (exprs, (offset))
    )
and parse_call tokens = 
  let name = (List.hd tokens) in
  let (args, off) = accumulate_call_args(List.tl tokens) [] 0 in
  if off == 0 then
  (init_node CALL name args,1)
  else 
  (init_node CALL name args,off+2)

let parse_return tokens = 
  match tokens with
  | h::t when h.value = "-" -> (
    match t with 
    | h::tail when h.value = ">" -> 
    (
      match tail with 
      | h::_ when h._type = KEYWORD -> let (n,off) = parse_type tail in 
      (init_node RETURN_TYPE n._token [n], (off+2)) 
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