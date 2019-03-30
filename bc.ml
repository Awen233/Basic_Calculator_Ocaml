type expr =
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of expr * string * expr
    | Fct of string * expr list

type statement =
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list

type fType = string list*statement list

type kvPair =
    | KVPair of string * float
    | KFPair of string * fType

type map = kvPair list

type block = statement list

let memory = []

let rec get (key : string) (map : map) : float =
    match map with
    | KVPair(k, v) :: tail -> if key = k then v else get key tail
    | _::tail -> get key tail
    | [] -> 0.0

let rec getFnc (key: string) (map : map) : fType =
    match map with
    | KFPair(k , v) :: tail -> if key = k then v else getFnc key tail
    | _ :: tail -> getFnc key tail
    | [] -> ([], [])

let rec putFnc (key: string) (value : fType) (map: map) : map =
    match map with
        | KFPair(k,v) :: tail -> if key = k then KFPair(key, value) :: tail else KFPair(k,v) :: (putFnc key value tail)
        | _ :: tail -> putFnc key value tail
        | [] -> [KFPair(key, value)]

let rec put (key : string) (value : float) (map : map) : map =
    match map with
        | KVPair(k,v) :: tail -> if key = k then  KVPair(key,value) :: tail else KVPair(k,v) :: (put key value tail)
        |  _ ::tail -> put key value tail
        | [] -> [KVPair(key, value)]

let printkvpair kv =
    match kv with
        | KVPair (k, v) ->  print_string ( k^" "^(string_of_float v))
        | KFPair (k, f) ->  print_string k
        | _ -> print_string ""

let rec print_env e =
    match e with
        | kv::kvs -> printkvpair kv; print_env kvs
        | _ :: tail -> print_env tail
        | [] -> ""

type scope = map list

let rec getScope (key : string) (scope: scope) : float =
    match scope with
        | map :: tail -> let res = get key map in if res = 0.0 then getScope key tail else res
        | _ -> 0.0

let rec getScopeFunc (key : string) (scope : scope) : fType =
    match scope with
        | map :: tail -> let res = getFnc key map in if res = ([], []) then getScopeFunc key tail else res
        | _ -> ([],[])

let putScope (key : string) (value : float) (scope : scope) : scope =
    match scope with
        | map :: lis -> let res = put key value map in res :: lis
        | [] -> [put key value [] ]

let rec putScope (key : string) (value : float) (scope : scope) : scope =
    match scope with
        | map :: lis -> let condi = getScope key lis in
            if(condi = 0.0) then
                begin
                    let res = put key value map in res :: lis
                end
            else
            map :: ( putScope key value lis )
        | [] -> [put key value [] ]


let putScopeFunc (key : string) (value : fType) (scope : scope) : scope =
    match scope with
        | map :: lis -> let res = putFnc key value map in res :: lis
        | [] -> [putFnc key value [] ]

let addScope (scope : scope) (map : map) : scope =
    match scope with
        | lis  -> map :: lis
        | [] -> scope

let removeScope (scope : scope) : scope =
    match scope with
        | top :: lis -> lis
        | [] -> []

let rec printScope (scope : scope) =
    match scope with
        | map :: lis -> print_env map ; print_endline "break " ; printScope lis
        | [] ->  print_endline "end scope "

(*type sExpr =*)
    (*| Atom of string*)
    (*| List of sExpr list*)

(*let varEval (_v: string) (_q:envQueue): float  = 0.0*)

let evalEquality (left: float) (op : string) (right : float) : float =
    match op with
        | ">" -> if left > right then 1. else 0.
        | "<" -> if left < right then 1. else 0.
        | "<=" -> if left <= right then 1. else 0.
        | ">=" -> if left >= right then 1. else 0.
        | "==" -> if left = right then 1. else 0.
        | "!=" -> if left <> right then 1. else 0.


let rec evalOp (left : expr) (op : string) (right : expr) (scope: scope) : float =
    match op with
        | "+" -> evalExpr left scope +. evalExpr right scope
        | "-" -> evalExpr left scope -. evalExpr right scope
        | "*" -> evalExpr left scope *. evalExpr right scope
        | "/" -> evalExpr left scope /. evalExpr right scope
        | "^" -> evalExpr left scope ** evalExpr right scope
        | ">" | "<" | "<=" | ">=" | "==" | "!=" ->
            evalEquality (evalExpr left scope) op (evalExpr right scope )
        | _ -> 0.0

(*type block = statement list*)
and evalExpr ( e: expr) (scope : scope): float  =
    match e with
        | Num(f) -> f
        | Var(v) -> getScope v scope
        | Op2 (first,op,second) -> evalOp first op second scope
        | Op1 (s, Var(v)) ->  (match s with
                           | "++" -> let res = (evalExpr (Var(v)) scope) +. 1.  in putScope v res scope; res
                           | "--" -> let res = (evalExpr (Var(v)) scope) -. 1.  in putScope v res scope; res
                           )
        | Fct (name, parameters) ->  let (para, code) = getScopeFunc name scope in
                                            assignParameter para parameters scope |> evalCode code; 0.0
        | _ -> 0.0

and evalCode (_code: block) (scope: scope) : scope =
    match _code with
        | x :: tail -> evalStatement x scope |> evalCode tail
        | [] ->  scope

(*print_endline "environment: "; print_env map; print_endline "block end";*)

and evalStatement (s: statement) (map: scope): scope =
    match s with
        | Assign(var, ex) ->  putScope var (evalExpr ex map) map
        | If(e, codeT, codeF) ->
            let cond = evalExpr e map in
                if(cond>0.0) then
                    addScope map [] |> evalCode codeT |> removeScope
                else
                    addScope map [] |> evalCode codeF |> removeScope
        | While (e, codeT) ->
            let cond = evalExpr e map in
                            if(cond>0.0) then
                               begin
                                addScope map [] |> evalCode codeT |> removeScope |> evalStatement s
                               end
                             else
                                map
        | Expr(e) -> (match e with
            | Op1(s, Var(v)) -> (
                            match s with
                           | "++" -> let res = (evalExpr (Var(v)) map) +. 1.  in print_endline (string_of_float res); putScope v res map
                           | "--" -> let res = (evalExpr (Var(v)) map) -. 1.  in putScope v res map
            )
            | _ -> evalExpr e map |> string_of_float |> print_endline; map
        )
        | For(sta, e, sta2, codeT) -> (
            let newMap = ref ( evalStatement sta (addScope map []) ) in
                while (evalExpr e !newMap > 0.0 ) do
                  newMap := evalCode codeT !newMap |> evalStatement sta2;
                done;
                !newMap |> removeScope
        )
        | FctDef(name, parameters, code) -> putScopeFunc name (parameters, code) map
        | _ -> map (*ignore *)

and assignParameter (params: string list) (values: expr list) (map : scope) : scope =
    match params with
        | s :: tail -> (
            match values with
                | v :: t -> putScope s (evalExpr v map) map |> assignParameter tail t
                | [] -> putScope s 0. map |> assignParameter tail []
        )
        | [] -> map



let block = [
    Assign("v", Num(1.0));
    If(
        Op2(Var("v"), ">", Num(10.0)),
        [Assign("v", Op2(Var("v"), "+", Num(1.0)))],
        [For(
            Assign("i", Num(2.0)),
            Op2(Var("i"),"<", Num(10.0)),
            Expr(Op1("++", Var("i"))),
            [
                Assign("v", Op2(Var("v"),"*", Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]


let _ = evalCode block [] |> printScope