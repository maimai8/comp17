(* η変換 *)

open Knormal

(* メイン *)

let rec g expr = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (name)
  | Op (name1, op, name2) -> Op (name1, op, name2)
  | IfEqual (name1, name2, expr3, expr4) ->
    IfEqual (name1, name2, g expr3, g expr4)
  | IfLess (name1, name2, expr3, expr4) ->
    IfLess (name1, name2, g expr3, g expr4)
  | Let ((name, t), expr1, expr2) ->
    if Variable (name) = expr2 then g expr1
    else
      let new_expr1 = g expr1 in
      let new_expr2 = g expr2 in
      Let ((name, t), new_expr1, new_expr2)
  | LetRec ((name, t), args, expr1, expr2) ->
    let new_expr1 = g expr1 in
    let new_expr2 = g expr2 in
    LetRec ((name, t), args, new_expr1, new_expr2)
  | Application (name, name_list) -> Application (name, name_list)
      
(* Eta.f : Knormal.t -> Knormal.t *)

let f expr = g expr
