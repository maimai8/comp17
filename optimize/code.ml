(* Intel 用コード生成 *)

open First
open Register
open Operator

(* registers *)

let r_sp = "_R_sp"
let r_bp = "_R_bp"
let r_ax = "_R_ax"
let r_dx = "_R_dx"

(* instructions *)

let label l =	  l ^ ":\n"
let movqi i r2  = "	movq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let movq r1 r2    = "	movq " ^ r1 ^ ", " ^ r2 ^ "\n"
let addq r1 r2  = "	addq " ^ r1 ^ ", " ^ r2 ^ "\n"
let subq r1 r2  = "	subq " ^ r1 ^ ", " ^ r2 ^ "\n"
let subqi i r2  = "	subq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let imulq r1 r2 = "	imulq " ^ r1 ^ ", " ^ r2 ^ "\n"
let sarqi i r2 = "	sarq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let idivq r    = "	idivq " ^ r ^ "\n"
let cmpq r1 r2  = "	cmpq " ^ r1 ^ ", " ^ r2 ^ "\n"
let jne l =	"	jne " ^ l ^ "\n"
let jle l =	"	jle " ^ l ^ "\n"
let jmp l =	"	jmp " ^ l ^ "\n"
let pushq r =	"	pushq " ^ r ^ "\n"
let popq r  =	"	popq " ^ r ^ "\n"
let call f =	"	call " ^ f ^ "\n"
let ret =	"	ret\n"

(* headers *)

let top =	"	.text\n"
let middle =	"\n" ^
		"	.globl _asm_main\n" ^
		"_asm_main: # main entry point\n" ^
			pushq "%rbx" ^
			pushq "%r12" ^
			pushq "%r13" ^
			pushq "%r14" ^
			pushq "%r15" ^
			pushq r_bp ^
			movq r_sp r_bp ^
		"    # main program start\n"
let last =	"    # main program end\n" ^
			movq (make_register 0) r_ax ^
			movq r_bp r_sp ^
			popq r_bp ^
			popq "%r15" ^
			popq "%r14" ^
			popq "%r13" ^
			popq "%r12" ^
			popq "%rbx" ^
			ret

(* push/pop registers *)

let rec push_live live = match live with
    [] -> ""
  | var :: rest -> pushq var ^ push_live rest

let rec pop_live live = match live with
    [] -> ""
  | var :: rest -> pop_live rest ^ popq var
  

(* メイン *)

exception NotSupported

(* remove : string list -> string -> string list *)
let rec remove lst name = match lst with
    [] -> []
  | arg :: rest -> if arg = name then remove rest name
    else [arg] @ remove rest name
           
(* 自由変数を返す関数 (First.t -> string list) *)
let rec fv expr = match expr with
    Number (num) -> []
  | Real (f) -> []
  | Variable (name) -> [name] 
  | Op (arg1, op, arg2) -> [arg1; arg2]
  | IfEqual (arg1, arg2, arg3, arg4) ->
    [arg1; arg2] @ fv arg3 @ fv arg4
  | IfLess (arg1, arg2, arg3, arg4) ->
    [arg1; arg2] @ fv arg3 @ fv arg4
  | Let ((name, t), arg1, arg2) ->
    fv arg1 @ (remove (fv arg2) name)
  | Application (name, args) -> args


(* First.t * string * string list -> string *)
let rec g_expr expr z live = match expr with
    Number (num) -> movqi num z
  | Real (f) -> raise NotSupported
  | Variable (name) ->
    if name = z then ""
    else movq name z
  | IfEqual (arg1, arg2, arg3, arg4) ->
    let l1 = Gensym.f "l" in
    let l2 = Gensym.f "l" in
    cmpq arg1 arg2 ^
    jne l1 ^
    g_expr arg3 z live ^
    jmp l2 ^
    label l1 ^
    g_expr arg4 z live ^
    label l2
  | IfLess (arg1, arg2, arg3, arg4) ->
    let l1 = Gensym.f "l" in
    let l2 = Gensym.f "l" in
    cmpq arg1 arg2 ^
    jle l1 ^
    g_expr arg3 z live ^
    jmp l2 ^
    label l1 ^
    g_expr arg4 z live ^
    label l2
  | Let ((name, t), arg1, arg2) ->
    g_expr arg1 name (live @ (remove (fv arg2) name)) ^
    g_expr arg2 z live
  | Application (name, args) ->
    push_live live ^
    call name ^
    movq (make_register 0) z ^
    pop_live live
  | Op (arg1, op, arg2) -> match op with
      Plus ->
      movq arg1 r_ax ^
      addq arg2 r_ax ^
      movq r_ax z
    | Minus ->
      movq arg1 r_ax ^
      subq arg2 r_ax ^
      movq r_ax z
    | Times ->
      movq arg1 r_ax ^
      imulq arg2 r_ax ^
      movq r_ax z
    | Divide ->
      sarqi 63 r_dx ^
      movq arg1 r_ax ^
      idivq arg2 ^
      movq r_ax z
    | Mod ->
      sarqi 63 r_dx ^
      movq arg1 r_ax ^
      idivq arg2 ^
      movq r_dx z
    | PlusDot | MinusDot | TimesDot | DivideDot ->
      raise NotSupported
      

(* First.def_t -> string *)
let rec g_def def = match def with
    FunDef ((f, t), rlst, expr) ->
    label f ^
    pushq r_bp ^
    movq r_sp r_bp ^
    (* subqi ((List.length (fv expr)) * 8) r_sp ^ *)
    g_expr expr (make_register 0) [] ^
    movq r_bp r_sp ^
    popq r_bp ^
    ret

(* 目的：listの要素にd_defを作用させる *)
(* g_def_list : First.def_t list -> string list *)
let rec g_def_list lst = match lst with
    [] -> ""
  | arg :: rest -> g_def arg ^ "\n" ^ g_def_list rest
    
(* First.prog_t -> string *)
let rec g_prog program = match program with
    Program (dlst, expr) ->
      top ^
      g_def_list dlst ^
      middle ^
      (* subqi ((List.length (fv expr)) * 8) r_sp ^ *)
      g_expr expr (make_register 0) [] ^
      last      
                                      
(* Code.f : First.prog_t -> string *)

let f program = g_prog program
