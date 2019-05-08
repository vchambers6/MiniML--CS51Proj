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
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
(* this does not eliminate bound variables*)
let free_vars (exp : expr) : varidset =
  let vars = SS.empty in 
  let rec get_vars (exp: expr) : varidset = 
    match exp with 
    | Var v -> SS.add v vars
    | Num n -> SS.empty                      
    | Bool b -> SS.empty               
    | Unop (_u, e1) -> get_vars e1        
    | Binop (_b, e1, e2) -> SS.union (get_vars e2) (SS.union (get_vars e1) vars)   
    | Conditional (e1, e2, e3) -> SS.union (get_vars e3) (SS.union (get_vars e2) (SS.union (get_vars e1) vars)) 
    | Fun (v, e1) -> SS.remove v (SS.union (get_vars e1) vars)
    | Let (v, e1, e2) ->  (SS.union (SS.union (SS.remove v (get_vars e2)) (get_vars e1)) (SS.remove v vars))(* THIS IS WRONG REMOVE THE THINGS FROM THE SET. use set module*)
    | Letrec (v, e1, e2) -> (SS.union (SS.union (SS.remove v (get_vars e2)) (SS.remove v (get_vars e1))) (SS.remove v vars))
    | Raise -> SS.empty                             
    | Unassigned -> SS.empty                         
    | App (e1, e2) -> SS.union (get_vars e2) (SS.union (get_vars e1) vars)
  in
  get_vars exp ;;
  
(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname () : varid =
  let gensym : string -> string =
    let ctr = ref 0 in 
    fun s -> 
      let v = s ^ string_of_int !ctr in 
      ctr := !ctr + 1; v 
  in gensym "var" ;; 

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let rec subst_helper (exp : expr) : expr = 
    match exp with  
    | Var v -> if v = var_name then repl else exp
    | Num _ | Bool _ | Raise | Unassigned -> exp              
    | Unop (u, e1) -> Unop (u, subst_helper e1)        
    | Binop (b, e1, e2) -> Binop (b, subst_helper e1, subst_helper e2)   
    | Conditional (e1, e2, e3) -> Conditional (subst_helper e1, subst_helper e2, subst_helper e3) 
   (*)| Fun (v, e1) -> Fun (v, subst_helper e1 (SS.remove v fvs)) *)
    | Fun (v, e1) ->
        if v = var_name 
            then exp 
        else if SS.mem v (free_vars repl) 
            then let z = new_varname () in
            Fun(z, subst_helper (subst v (Var z) e1))
        else
            Fun(v, subst_helper e1)
    
    | Let (v, e1, e2) -> 
        if v = var_name 
            then Let(v, subst_helper e1, e2)
        else if SS.mem v (free_vars repl)
            then let z = new_varname () in 
            Let(z, subst_helper e1, subst_helper (subst v (Var z) e2))
        else 
            Let(v, subst_helper e1, subst_helper e2)

    | Letrec (v, e1, e2) ->
        if v = var_name
            then Letrec (v, e1, e2)
        else 
            Letrec (v, subst_helper e1, subst_helper e2)                     
    | App (e1, e2) -> App (subst_helper e1, subst_helper e2) 
  in

  subst_helper exp ;;

(*......................................................................
  String representations of expressions
 *)
   
    
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | Var v -> v ^ " "                       
  | Num n -> (string_of_int n) ^ " "                           
  | Bool b -> Printf.sprintf "%B " b                         
  | Unop (uop, expr1) -> 
      (match uop with 
      | Negate -> "~- " ^ (exp_to_concrete_string expr1) )
  | Binop (bop, expr1, expr2) -> 
      (match bop with
      | Plus -> (exp_to_concrete_string expr1) ^ " + " ^ 
                (exp_to_concrete_string expr2)
      | Minus -> (exp_to_concrete_string expr1) ^ " - " ^ 
                 (exp_to_concrete_string expr2)
      | Times -> (exp_to_concrete_string expr1) ^ " * " ^ 
                 (exp_to_concrete_string expr2)
      | Equals -> (exp_to_concrete_string expr1) ^ " = " ^ 
                  (exp_to_concrete_string expr2)
      | LessThan -> (exp_to_concrete_string expr1) ^ " < " ^ 
                    (exp_to_concrete_string expr2) )       
  | Conditional (expr1, expr2, expr3) -> "if " ^ (exp_to_concrete_string expr1) ^
                                         "then " ^ (exp_to_concrete_string expr2) ^
                                         "else " ^ (exp_to_concrete_string expr1)
  | Fun (v, expr1) -> "fun " ^ v ^ " -> "  ^ (exp_to_concrete_string expr1)                 
  | Let (v, expr1, expr2) -> "let " ^ v ^ " = " ^ (exp_to_concrete_string expr1) ^
                            (exp_to_concrete_string expr2)
  | Letrec (v, expr1, expr2) -> "let rec " ^ v ^ " = " ^ (exp_to_concrete_string expr1) ^
                            (exp_to_concrete_string expr2)
  | Raise -> "Raise"                               
  | Unassigned -> "Unassigned"                          
  | App (expr1, expr2) ->  (exp_to_concrete_string expr1) ^
                            (exp_to_concrete_string expr2) ;;


(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with 
  | Var v -> "Var(" ^ v ^ ")"                       
  | Num n -> Printf.sprintf "Num(%d)" n                         
  | Bool b -> Printf.sprintf "Bool(%B)" b                       
  | Unop (uop, expr1) -> 
      (match uop with 
      | Negate -> "Unop(Negate, " ^ (exp_to_abstract_string expr1) ^ ")")            
  | Binop (bop, expr1, expr2) -> 
    (match bop with 
      | Plus -> "Binop(Plus, " ^ 
                (exp_to_abstract_string expr1) ^ ", " ^ 
                (exp_to_abstract_string expr2) ^ ")"
      | Minus -> "Binop(Minus, " ^ 
                (exp_to_abstract_string expr1) ^ ", " ^ 
                (exp_to_abstract_string expr2) ^ ")"
      | Times -> "Binop(Times, " ^ 
                (exp_to_abstract_string expr1) ^ ", " ^ 
                (exp_to_abstract_string expr2) ^ ")"
      | Equals -> "Binop(Equals, " ^ 
                (exp_to_abstract_string expr1) ^ ", " ^ 
                (exp_to_abstract_string expr2) ^ ")"
      | LessThan -> "Binop(LessThan, " ^ 
                (exp_to_abstract_string expr1) ^ ", " ^ 
                (exp_to_abstract_string expr2) ^ ")"  )
  | Conditional (expr1, expr2, expr3) -> "Conditional(" ^ (exp_to_abstract_string expr1) ^ ", " ^ 
                (exp_to_abstract_string expr2)  ^ ", " ^ (exp_to_abstract_string expr3) ^ ")"
  | Fun (v, expr1) -> "Fun(" ^ v ^ ", " ^(exp_to_abstract_string expr1) ^ ")"               
  | Let (v, expr1, expr2) -> "Let(" ^ v ^ ", " ^ (exp_to_abstract_string expr1) ^ ", " ^ 
                              (exp_to_abstract_string expr2) ^ ")"         
  | Letrec (v, expr1, expr2) -> "Letrec(" ^ v ^ ", " ^ (exp_to_abstract_string expr1) ^ ", " ^ 
                              (exp_to_abstract_string expr2) ^ ")"    
  | Raise -> "Raise"                          
  | Unassigned  -> "Unassigned"                         
  | App (expr1, expr2) -> "App(" ^(exp_to_abstract_string expr1) ^ ", " ^ 
                          (exp_to_abstract_string expr2) ^ ")";;
