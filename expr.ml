(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;
(* pg 14 of chpt 13 *)

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming - pg 23 on chpt 13 *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var x -> SS.singleton x
  | Num _ | Bool _ -> SS.empty
  | Unop(_, arg) -> free_vars arg
  | Binop(_, arg1, arg2) -> SS.union (free_vars arg1) (free_vars arg2)
  | Conditional (e1, e2, e3) -> SS.union (SS.union (free_vars e1) (free_vars e2)) (free_vars e3)
  | Fun (v, e) -> SS.remove v (free_vars e)
  | Let (v, def, body) -> SS.union (free_vars def) (SS.remove v (free_vars body))
  | Letrec (v, e1, e2) -> SS.union (SS.remove v (free_vars e1)) (SS.remove v (free_vars e2)) 
  | Raise | Unassigned -> SS.empty  
  | App (e1, e2) ->  SS.union (free_vars e1) (free_vars e2)
;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname =  (* why does this not compute with varid? It was set to string above... *)
  let suffix = ref 0 in
  fun () -> let symbol = "x" ^ string_of_int !suffix in
             incr suffix;
             symbol ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let rec sub_this (exp : expr) : expr =
    match exp with
    | Var x -> if x = var_name then repl else exp
    | Num e -> Num e
    | Bool b -> Bool b
    | Unop (op, arg) -> Unop(op, sub_this arg)
    | Binop (op, arg1, arg2) -> Binop(op, sub_this arg1, sub_this arg2)
    | Conditional (e1, e2, e3) -> Conditional (sub_this e1, sub_this e2, sub_this e3)
    | Fun (v, e) -> if v = var_name then exp
                    else if SS.mem v (free_vars repl) then
                      let new_v = new_varname () in
                      Fun (new_v, sub_this (subst v (Var new_v) e))
                    else Fun(v, sub_this e)
    | Let (x, def, body) -> if x = var_name then Let(x, sub_this def, body)
                            else Let(x, sub_this def, sub_this body)
    | Letrec (x, def, body) -> if x = var_name then exp
                              else if SS.mem x (free_vars repl) then
                                let new_v = new_varname () in
                                Letrec (new_v, sub_this (subst x (Var new_v) def), 
                                        sub_this (subst x (Var new_v) body))
                              else Letrec (x, sub_this def, sub_this body)
    | Raise | Unassigned -> exp
    | App (e1, e2) -> App (sub_this e1, sub_this e2)
  in
  sub_this exp ;;
 
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)

let unop_string (u : unop) : string =
  match u with 
  | Negate -> "~-";;
let binop_string (b : binop) : string =
  match b with 
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Equals -> "="
  | LessThan -> "<";;

let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | Var e -> e
  | Num i -> string_of_int i
  | Bool b -> string_of_bool b
  | Unop (u, e) -> unop_string(u) ^ exp_to_concrete_string(e)
  | Binop (b, e1, e2) -> exp_to_concrete_string(e1) ^ " " ^ 
                         binop_string(b) ^ " " ^exp_to_concrete_string(e2)
  | Conditional (e1, e2, e3) -> "if " ^ exp_to_concrete_string(e1) ^ " then " ^
                                exp_to_concrete_string(e2) ^ " else " ^ 
                                exp_to_concrete_string(e3)
  | Fun (f, e1) -> "fun " ^ f ^ " -> " ^ exp_to_concrete_string(e1) 
  | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ exp_to_concrete_string(e1) ^ " in " ^
                       exp_to_concrete_string(e2)
  | Letrec (v, e1, e2) -> "let rec " ^ v ^ " = " ^ exp_to_concrete_string(e1) ^ " in " ^
                          exp_to_concrete_string(e2)
  | Raise -> "Raise "
  | Unassigned -> "Unassigned"
  | App (e1, e2) -> exp_to_concrete_string(e1) ^ " " ^ exp_to_concrete_string(e2);;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)


let unop_abstract_string (u : unop) : string = 
  match u with 
  | Negate -> "Negate";;

let binop_abstract_string (b : binop ) : string =
  match b with 
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Equals -> "Equals"
  | LessThan -> "LessThan";;

let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^  ")"                        
  | Num n -> "Num(" ^ string_of_int n ^  ")" 
  | Bool b -> "Bool(" ^ string_of_bool b ^  ")"                    
  | Unop (u, e) -> "Unop(" ^ unop_abstract_string u ^ ", "  ^ 
                    exp_to_abstract_string e ^  ")"         
  | Binop (b, e1, e2) -> "Binop(" ^ binop_abstract_string b ^ ", " ^ 
                          exp_to_abstract_string e1 ^ ", " ^
                          exp_to_abstract_string e2 ^  ")" 
  | Conditional (e1, e2, e3) -> "Conditional(" ^ exp_to_abstract_string e1 ^
                                ", " ^ exp_to_abstract_string e2 ^
                                ", " ^ exp_to_abstract_string e3 ^  ")"
  | Fun (v, e) -> "Fun(" ^ v ^ ", " ^ exp_to_abstract_string e ^  ")"                
  | Let (v, def, body) -> "Let(" ^ v ^ ", " ^ exp_to_abstract_string def ^ 
                          ", " ^ exp_to_abstract_string body ^  ")"      
  | Letrec (v, def, body) -> "Letrec(" ^ v ^ ", " ^ exp_to_abstract_string def ^ 
                             ", " ^ exp_to_abstract_string body ^  ")"        
  | Raise -> "Raise"                               
  | Unassigned -> "Unassigned"                          
  | App (e1, e2) -> "App(" ^ exp_to_abstract_string e1 ^ ", " ^ 
                    exp_to_abstract_string e2 ^  ")"
;;

