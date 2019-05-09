(* CS51 final project tests *)

open Miniml ;; 
open Evaluation ;; 
open Expr;;

let env = Env.create () ;;

(* VALUES FOR EVALUATION FUNCTION TESTS *)
let simpletest = Num(1) (* 1 + 1*) ;;
let eval_s_simp = eval_s simpletest env ;;
let eval_d_simp = eval_d simpletest env ;;
let eval_l_simp = eval_l simpletest env ;; 

let factorialtest = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), 
					Num(1), Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), 
					Num(1)))))), App(Var("f"), Num(4)))
					(* let rec f = fun x -> if x = 0 then 1 else x * (f (x -1)) in f 4 *)
let eval_s_fact = eval_s factorialtest env ;;
let eval_d_fact = eval_d factorialtest env ;;

let lexdyntest = Let("g", Num(1), Let("y", Num(2), Let("z", Fun("x", Binop(Plus, Var("x"), 
				 Var("g"))), Let("g", Num(100), Binop(Plus, App(Var("z"), Var("g")), 
				 Var("y"))))))
				 (*let g = 1 in let y = 2 in let z = fun x -> x + g in let g = 100 in z g + y*)
let eval_s_ld = eval_s lexdyntest env ;;
let eval_d_ld = eval_d lexdyntest env ;;
let eval_l_ld = eval_l lexdyntest env ;;
					

let apptest = Let("f", Fun("x", Var("x")), App(App(App(Var("f"), Var("f")), Var("f")), Num(3)))
			  (* let f = fun x -> x in f f f 3 *)
let eval_s_app = eval_s apptest env ;;
let eval_d_app = eval_d apptest env ;;
let eval_l_app = eval_l apptest env ;;

let boolcondtest = Let("f", Fun("x", Conditional(Binop(Equals, Var("x"), Bool(true)), Bool(true), 
				   Bool(false))), App(Var("f"), Binop(LessThan, Num(100), Num(7))))
				   (* let f = fun x -> if x = true then true else false in f (100 < 7) ;; *)
let eval_s_bc = eval_s boolcondtest env ;; 
let eval_d_bc = eval_d boolcondtest env ;; 
let eval_l_bc = eval_l boolcondtest env ;; 

(* FOR ENVIRONMENT FUNCTION TESTS *)
let env2 = Env.create () ;; 
 Env.extend env2 "x" (ref (Env.Val (Bool true)));; 
 Env.extend env2 "y" (ref (Env.Val (Num 5)));; 
 Env.extend env2 "x" (ref (Env.Val (Bool false)));;
 Env.extend env2 "g" (ref (Env.Val (Num 6)));; 

let _ =
  (* simple tests *)
  assert(eval_s_simp = Env.Val (Num 1));
  assert(eval_d_simp = Env.Val (Num 1));
  assert (eval_l_simp = Env.Val (Num 1));
  assert ((eval_l_simp = eval_d_simp));

  (* factorial tests *)
  assert(eval_s_fact = Env.Val (Num 24)); 
  assert(eval_d_fact = Env.Val (Num 24));


  (* lexical vs dynamic tests *)
  assert(eval_d_ld = Env.Val (Num 202));
  assert(eval_l_ld = Env.Val (Num 103));
  assert(eval_s_ld = Env.Val (Num 103)); 
  assert(eval_l_ld != eval_d_ld); 

  (* function application tests *)
  assert(eval_s_app = Env.Val (Num 3)); 
  assert(eval_d_app =  Env.Val (Num 3));
  assert(eval_l_app = Env.Val (Num 3));

  (* boolean + conditional tests *)
  assert(eval_s_bc = Env.Val (Bool false));
  assert(eval_d_bc = Env.Val (Bool false));
  assert(eval_l_bc = Env.Val (Bool false))

;;

