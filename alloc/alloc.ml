(* Alloc.ml:
   (非常に単純な)レジスタ割り当てを行うプログラム *)

let rec g_expr expr env = match expr with
    First.Number (num) -> First.Number (num)
  | First.Real (f) -> First.Real (f)
  | First.Variable (name) ->
    if Register.is_register name then
      First.Variable (name)
    else
      First.Variable (Env.get env name)
  | First.Op (arg1, op, arg2) ->
    First.Op (Env.get env arg1, op, Env.get env arg2)
  | First.IfEqual (arg1, arg2, arg3, arg4) ->
    First.IfEqual (Env.get env arg1, Env.get env arg2, g_expr arg3 env, g_expr arg4 env)
  | First.IfLess (arg1, arg2, arg3, arg4) ->
    First.IfLess (Env.get env arg1, Env.get env arg2, g_expr arg3 env, g_expr arg4 env)
  | First.Let ((name, t), arg1, arg2) ->
    if Register.is_register name then
      First.Let ((name, t), g_expr arg1 env, g_expr arg2 env)
    else
      let i = 11 - List.length env in
      let r = Register.make_register i in
      let newenv = Env.add env name r in
      First.Let ((r, Type.gen_type()), g_expr arg1 env, g_expr arg2 newenv)
  | First.Application (name, args) -> First.Application (name, args)

let g_def env def = match def with
    First.FunDef (f, rlst, expr) -> First.FunDef (f, rlst, g_expr expr env) 

let g_program program env = match program with
  First.Program (def, expr) ->
  First.Program (List.map (g_def env) def, g_expr expr env)
    
(* Alloc.f: レジスタ割り当て前処理プログラムの入口 *)

let f program = g_program program Env.empty_env
