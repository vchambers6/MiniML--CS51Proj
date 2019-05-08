(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)
    
open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error *)
exception EvalError of string ;;
(* Exception for evaluator runtime, generated by an explicit "raise" construct *)
exception EvalException ;;


(*......................................................................
  Environments and values 
 *)

module type Env_type = sig
    type env
    type value =
      | Val of expr
      | Closure of (expr * env)
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : Env_type =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    (* Creates an empty environment *)
    let create () : env = [] ;;

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp : expr) (env : env) : value =
      Closure (exp, env) ;;

    (* Looks up the value of a variable in the environment *)
    
    let lookup (env : env) (varname : varid) : value = 
      try !(List.assoc varname env) 
      with 
      | _ -> raise (EvalError "var not found") ;;

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let rec extend (env : env) (varname : varid) (loc : value ref) : env =
      match env with 
      | (a, b) :: tl -> 
          if a = varname 
            then (b := !loc; (a, b) :: tl)
          else (varname, loc) :: env
      | [] -> (varname, loc) :: env ;;

    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    

    (* Returns a printable string representation of an environment *)
    let rec env_to_string (env : env) : string =
      "[" ^ 
      
      let rec helper (env : env ) : string = 
        (match env with 
        | (a, b) :: tl ->  (match !b with 
        | Val c ->  "{" ^ a ^ " |-> " ^ (exp_to_concrete_string c) ^ "}" ^ "; " ^ (helper tl)
        | Closure (d, e) -> "{" ^ a ^ " |-> " ^ ((exp_to_concrete_string d) ^ " -> " ^ helper e)
                             ^ "}" ^ "; " ^ (helper tl) )
        | [] -> "" )
      in
      (helper env) ^ "]" ;;

    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      (* how should i use printevp ???*)  
      match v with
      | Val a -> exp_to_concrete_string a 
      | Closure (a, b) -> 
          if printenvp 
            then (env_to_string b) ^ "(" ^ (exp_to_concrete_string a) ^ ")"
          else   
            "(" ^ (exp_to_concrete_string a) ^ ")" ;;

  end
;;


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below, evaluates an expression exp
  in an enviornment env returning a result of type value. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a value and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type expr -> Env.env -> Env.value for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as eval_e below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)
   
let eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec eval_s_solve (exp : expr) : expr = 
    print_endline (exp_to_abstract_string exp); 
    match exp with 
      | Var v -> Unassigned (*i think this should raise an error *)
      | Num n -> exp 
      | Bool b -> exp
      | Unop (uop, e1) -> 
        (match uop with 
        | Negate -> 
            match e1 with 
            | Num n -> Num (~-n)
            | Bool b -> Bool (not b) 
            | Binop (a, b, c) -> (match eval_s_solve (Binop (a, b, c)) with 
                | Num n -> Num (~-n)
                | Bool boo -> Bool (not boo)
                | _ -> raise (EvalError "Type Error Unary Operator")  )
            | _ -> eval_s_solve e1 ) (* HELP *)
      | Binop (bop, e1, e2) -> 
        (match eval_s_solve e1, eval_s_solve e2 with 
        | Num n1, Num n2 -> 
            (match bop with 
            | Plus -> Num (( + ) n1 n2)
            | Minus -> Num (( - ) n1 n2)
            | Times -> Num (( * ) n1 n2)
            | Equals -> Bool (n1 = n2)
            | LessThan -> Bool (n1 < n2) )
        | Bool b1, Bool b2 -> 
            (match bop with 
            | Equals -> Bool (b1 = b2)
            | LessThan -> Bool (b1 < b2)
            | _ -> raise (EvalError "Type Error") )
        | _ -> raise (EvalError "Type Error")  
        )
      | Conditional (e1, e2, e3) -> if ((eval_s_solve e1) = (Bool true)) then (eval_s_solve e2) else 
                                    (eval_s_solve e3)
      | Fun (_v, _e1) -> exp
      | Let (v, e1, e2) -> eval_s_solve (subst v (eval_s_solve e1) e2)

      | Letrec (v, e1, e2) -> (let ve1 = (eval_s_solve e1) in let a = subst v (Letrec (v, ve1, Var(v))) ve1 in 
        eval_s_solve (subst v a e2) ) (* CAN SIMPLIFY *)

      | Raise -> raise (Invalid_argument "doesnt work idk")
      | Unassigned -> raise (EvalError "Unbound value")
      | App (e1, e2) ->
          (match eval_s_solve e1 with
          | Fun(e3, e4) -> eval_s_solve (subst e3 (eval_s_solve e2) e4)
          | _ -> raise (EvalError "type error app ")
          )
  in
  Val (eval_s_solve exp)
;;

(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
   

let eval_d (exp : expr) (env : Env.env) : Env.value = 

  let rec eval (exp : expr) (env : Env.env) : Env.value = 
  print_endline (exp_to_abstract_string exp); 
  print_endline (Env.env_to_string env);
  match exp with 
    | Var v -> Env.lookup env v 
    | Num _ | Bool _ | Raise | Unassigned | Fun (_, _) -> Env.Val exp
    | Unop (uop, e1) -> 
        (match uop with 
        | Negate ->(
          match e1 with 
          | Num n -> Env.Val (Num ~-n)
          | Bool boo -> Env.Val (Bool (not boo)) 
          | _ -> raise (EvalError "Type Error Unary") ) 

        | _ -> eval exp env)
    | Binop (bop, e1, e2) ->
        (match eval e1 env, eval e2 env with 
        | Env.Val (Num n1), Env.Val (Num n2) ->
          (match bop with 
          | Plus -> Env.Val (Num (( + ) n1 n2))
          | Minus -> Env.Val (Num (( - ) n1 n2))
          | Times -> Env.Val (Num (( * ) n1 n2))
          | Equals -> Env.Val (Bool (n1 = n2))
          | LessThan -> Env.Val (Bool (n1 < n2))
          )
        | Env.Val (Bool b1), Env.Val (Bool b2) ->
          (match bop with 
          | Equals -> Env.Val (Bool (b1 = b2))
          | LessThan -> Env.Val (Bool (b1 < b2))
          | _ -> raise (EvalError "Binop type error")
          )
        | _ -> raise (EvalError "wrong")
      )
    | Conditional (e1, e2, e3) -> if ((eval e1 env) = (Env.Val (Bool 
        true))) then (eval e2 env) else (eval e3 env)
    | Let (v, e1, e2) -> 
        (match eval e1 env with 
        | Env.Val v2 -> eval e2 (Env.extend env v (ref (Env.Val v2)))  
        | Env.Closure _ -> raise (EvalError "Closure found")

        )
    | Letrec (v, e1, e2) ->
      (match eval e1 env with 
      | Env.Val _ -> eval e2 (Env.extend env v ( ref  (eval e1 (Env.extend env v (ref (Env.Val(Letrec (v, e1, e2)))))) ))
      | Env.Closure _ -> raise (EvalError "Closure found")
      )
    | App (e1, e2) ->
        (match eval e1 env with 
        | Env.Val (Fun (v, e3)) -> 
            (match eval e2 env with 
            | Env.Val v2 -> eval e3 (Env.extend env v (ref (Env.Val v2))) 
            | Env.Closure _ -> raise (EvalError "nooo")
            )
        | _ -> raise (EvalError "nooo") 
        )
  in 

  eval exp env ;; 
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
   
let eval_l (_exp : expr) (_env : Env.env) : Env.value =
  failwith "eval_l not implemented" ;;

(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within eval_s, eval_d, or eval_l. *)

let eval_e _ =
  failwith "eval_e not implemented" ;;
  
(* Connecting the evaluators to the external world. The REPL in
   miniml.ml uses a call to the single function evaluate defined
   here. Initially, evaluate is the trivial evaluator eval_t. But you
   can define it to use any of the other evaluators as you proceed to
   implement them. (We will directly unit test the four evaluators
   above, not the evaluate function, so it doesn't matter how it's set
   when you submit your solution.) *)
   
let evaluate = eval_d;;
