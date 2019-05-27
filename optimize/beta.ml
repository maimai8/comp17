(* β変換 *)

open Knormal

(* チェック関数 *)
let check env key =
  try
    Env.get env key
  with
  Env.UnboundVariable (s) -> key

(* メイン *)

let rec g expr env = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (check env name)
  | Op (arg1, op, arg2) -> Op (check env arg1, op, check env arg2)
  | IfEqual (arg1, arg2, arg3, arg4) ->
    IfEqual (check env arg1, check env arg2,
	     g arg3 env, g arg4 env)
  | IfLess (name1, name2, expr3, expr4) ->
    IfLess (check env name1, check env name2,
	    g expr3 env,
	    g expr4 env)
  | Let ((name, t), expr1, expr2) -> ( match expr1 with
        Variable (expr1) ->
        let new_env = Env.add env name expr1 in
        g expr2 new_env
      | _ ->
        let new_expr1 = g expr1 env in
        let new_expr2 = g expr2 env in
        Let ((name, t), new_expr1, new_expr2)
    )
  | LetRec ((name, t), params, expr1, expr2) ->
    LetRec ((name, t), params, g expr1 env, g expr2 env)
  | Application (name, name_list) ->
    Application (check env name,
		 List.map (fun var -> check env var) name_list)
      
(* Beta.f : Knormal.t * (string * string) list -> Knormal.t *)
      
let f expr = g expr Env.empty_env
