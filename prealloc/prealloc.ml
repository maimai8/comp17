(* Prealloc.g : レジスタ割り当て前処理プログラムの本体 *)

let rec g_expr expr = match expr with
    First.Number (num) -> First.Number (num)
  | First.Real (f) -> First.Real (f)
  | First.Variable (name) -> First.Variable (name)
  | First.Op (arg1, op, arg2) -> First.Op (arg1, op, arg2)
  | First.IfEqual (arg1, arg2, arg3, arg4) ->
    First.IfEqual (arg1, arg2, g_expr arg3, g_expr arg4)
  | First.IfLess (arg1, arg2, arg3, arg4) ->
    First.IfLess (arg1, arg2, g_expr arg3, g_expr arg4)
  | First.Let ((name, t), arg1, arg2) ->
    First.Let ((name, t), g_expr arg1, g_expr arg2)
  | First.Application (name, args) ->
    let rlst = List.mapi (fun i a -> Register.make_register (i+1)) args in
    let rlst_t = List.map (fun name -> (name, Type.gen_type())) rlst in
    let app = First.Application (name, rlst) in
    List.fold_right2 (fun a b c -> First.Let (a, First.Variable(b), c)) rlst_t args app

let g_def def = match def with
    First.FunDef (f, args, expr) ->
    let rlst = List.mapi (fun i arg -> (Register.make_register (i+1))) args in
    let freglst = List.map (fun name -> (name, Type.gen_type())) rlst in
    let letexpr = List.fold_right2 (fun a b c -> First.Let (a, First.Variable(b), c)) args rlst (g_expr expr) in
    First.FunDef (f, freglst, letexpr)

let g_program program = match program with
  First.Program (def,expr) ->
  First.Program (List.map g_def def, g_expr expr)
(* Prealloc.f: レジスタ割り当て前処理プログラムの入口 *)

let f program = g_program program
