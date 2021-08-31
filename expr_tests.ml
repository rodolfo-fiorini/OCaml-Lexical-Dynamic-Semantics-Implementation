

module Ex = Expr;;

(* Commented out testing due to compilation issues. *)

(* 
(* *)
let set1 = free_vars (Binop(Plus, Var "x", Var "y")) in
let test1 = Printf.sprintf ("free_vars set1 - %b") (SS.elements set1 = ["x"; "y"]) in
test1;; (* good *)

let set2 = free_vars (Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3)))) in 
SS.elements set2 = [];; (* good *)

let set3 = free_vars (Let("y", Binop(Plus, Var("x"), Num(2)), Binop(Plus, Var("y"), Num(3))) ) in 
SS.elements set3 = ["x"];; (* good *)

let sub2 = free_vars (Letrec("f", 
                  Fun("x", 
                    Conditional(Binop(Equals, Var("x"), Num(0)), Num(1), 
                      Binop(Times, Var("x"), App(Var("f"), 
                        Binop(Minus, Var("x"), Num(1)))))), App(Var("f"), Num(4)))) in 
SS.elements sub2;; (* good *)

let sub1 = subst "x" (Binop(Plus, Num 3, Num 5)) (Letrec("y", Binop(Plus, Num 3, Num 5), Var("x"))) in
sub1 = Letrec ("y", Binop (Plus, Num 3, Num 5), Binop (Plus, Num 3, Num 5)) ;; (* good *)


let sub2 = subst "z" (Var("x")) (Letrec("f", 
Fun("x", 
  Conditional(Binop(Equals, Var("x"), Num(0)), Num(1), 
    Binop(Times, Var("z"), App(Var("f"), 
      Binop(Minus, Var("x"), Num(1)))))), App(Var("f"), Num(4)))) in 
sub2;;
              
 *)




  (* let set2 = free_vars (Let(f, Fun(x, Var(x)), App(App(Var(f), Var(f)), Num(3)))) in 
  SS.elements set2 = [];; *)


(*
#mod_use "expr.ml";;
open Expr;;

*)