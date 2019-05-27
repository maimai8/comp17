z(* letの結合性変換 *)

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
  | Let ((name, t1), expr1, expr2) -> ( match expr1 with
        Let ((n, t2), e1, e2) -> Let ((n, t2), g e1, Let ((name, t1), g e2, g expr2))
      | _ ->
        let new_expr1 = g expr1 in
        let new_expr2 = g expr2 in
        Let ((name, t1), new_expr1, new_expr2))
  | LetRec ((name, t), args, expr1, expr2) ->
    let new_expr1 = g expr1 in
    let new_expr2 = g expr2 in
    LetRec ((name, t), args, new_expr1, new_expr2)
  | Application (name, name_list) -> Application (name, name_list)
      
(* Eta.f : Knormal.t -> Knormal.t *)

let f expr = g expr
