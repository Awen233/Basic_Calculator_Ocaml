test1:

(*
define add (x; y) {
    x + y
}
*)
let params = ["x"; "y"]
let name = "add"
let expr1 = Op2(Var("x"), "+", Var("y"))
let sta1 = Expr(expr1)
let funcTest = FctDef(name, params, [sta1])
let funcCall = Fct("add", [Num(1.); Num(2.)])
let _ = evalStatement funcTest [] |> evalExpr funcCall
let _ = evalExpr

test2:

(*
i = 3;
if(i < 4){
    ++i
    x = 5;
}
*)
let sta0 = Assign("a", Num(3.0))
let condi = Op2(Var("a"), "<=", Num(4.))
let exp2 = Op1("++", Var("a"))
let sta1 = Expr(exp2)
let sta2 = Assign("b", Num(4.0))
let codeT = [sta1; sta2]
let if_test = If(condi, codeT, [])
let _ = evalStatement if_test (evalStatement sta0 []) |> printScope

test3:

(*
i = 3.
while(i < 10.){
    ++i
    b = 5
}
*)
let sta0 = Assign("a", Num(3.0))
let exp1 = Op2(Var("a"), "<=", Num(9.2))
let sta2 = Expr(Op1("++", Var("a")))
let sta3 = Assign("b", Num(4.0))
let sta4 = Expr(Var("b"))
let while_test = While(exp1, [sta2; sta3; sta4])
let _ = evalStatement while_test (evalStatement sta0 []) |> printScope


test4:
(*
for( a = 3.0; a <= 9.2; ++a){
    b = 4.0
}
*)

let sta0 = Assign("a", Num(3.0))
let exp1 = Op2(Var("a"), "<=", Num(9.2))
let sta1 = Expr(Op1("++", Var("a")))
let sta3 = Assign("b", Num(4.0))
let for_test = For(sta0, exp1, sta1, [sta3])

let _ = evalStatement for_test [] |> printScope

test5:
(*
for( a = 3.0; a <= 9.2; ++a){
    b = 4.0
}
a = 3.0
*)

let sta0 = Assign("a", Num(3.0))
let exp1 = Op2(Var("a"), "<=", Num(9.2))
let sta1 = Expr(Op1("++", Var("a")))
let sta3 = Assign("b", Num(4.0))
let for_test = For(sta0, exp1, sta1, [sta3])

let _ = evalStatement for_test [] |> evalStatement sta0 |> printScope


test6:

let block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)),
        [Assign("v", Op2("+", Var("v"), Num(1.0)))],
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++a", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let _ = evalCode block [] |> printScope