structure Test = struct
open Absyn Eval

val p : ErrorMsg.pos*ErrorMsg.pos = (0,0)
val id = Symbol.symbol

(* fun id(x:int):int = x
   fun main(arg:int):int = id(3)
*)
val prog1 =
  [(p,(id"id",id"x",Inttp,Inttp,Id(id"x"))),
   (p,(id"main",id"arg",Inttp,Inttp, Call (Id(id"id"),Int 3)))]

(* fun fac(x:int):int = if x = 0 then 1 else x * fac(x - 1)
   fun main(arg:int):int = fac(5)
*)
val prog2 =
 [(p,
     (id"fac",id"x",Inttp,Inttp,
       If(Op (Eq,[Id(id"x"),Int 0]),
	 Int 1,
         Op (Mul,[Id(id"x"),Call (Id(id"fac"), Op (Sub,[Id(id"x"),Int 1]))])
       )
     )),
   (p,(id"main",id"arg",Inttp,Inttp, Call (Id(id"fac"),Int 5)))]

(* fun addsome(x:int):int = x + 5
   fun add(x:<int, int>):int = #0 x + #1 x
   fun gt(t:<int, int>):int =
      let a = #0 t in 
      let b = #1 t in not a < b & not a = b
   fun main(arg:int):int =
      let x = (5 + 3) * 7 in 
      let y = x + 7 in 
        if gt(<x, 0>) then
          if not x = 7 then addsome(add(<x, y>)) else -1
        else
          not -2
*)
val prog3 =
[(p,
  (id"addsome",id"x",Inttp,Inttp,
   Op (Add,[Id(id"x"),Int 5]))),
 (p,
  (id"add",id"x",Tupletp [Inttp,Inttp],Inttp,
   Op (Add,[Proj (0,Id(id"x")),Proj (1,Id(id"x"))]))),
 (p,
  (id"gt",id"t",Tupletp [Inttp,Inttp],Inttp,
   Let (id"a",Proj (0,Id(id"t")),
   Let (id"b",Proj (1,Id(id"t")),
        If (If (Op (LT,[Id(id"a"),Id(id"b")]),Int 0,Int 1),
	    If (If (Op (Eq,[Id(id"a"),Id(id"b")]),Int 0,Int 1),Int 1,Int 0),
	    Int 0))))),
 (p,(id"main",id"arg",Inttp,Inttp,
Let (id"x",Op (Mul,[Op (Add,[Int 5,Int 3]),Int 7]),
Let (id"y",Op (Add,[Id(id"x"),Int 7]),
     If (Call (Id(id"gt"),Tuple [Id(id"x"),Int 0]),
	 If (If (Op (Eq,[Id(id"x"),Int 7]),Int 0,Int 1),
	     Call (Id(id"addsome"),Call (Id(id"add"),Tuple [Id(id"x"),Id(id"y")])),
	     Op (Sub,[Int 0,Int 1])),
	 If (Op (Sub,[Int 0,Int 2]),Int 0,Int 1))))))]

(* fun add(x:<int, int>):int = #0 x + #1 x
   fun main(arg:int):int =
      let x = add(y) in 
      let z = add(<0, add>) in 
        add(<<0, 1>, <>>)
*)
val prog4 =
[(p,
  (id"add",id"x",Tupletp [Inttp,Inttp],Inttp,
   Op (Add,[Proj (0,Id(id"x")),Proj (1,Id(id"x"))]))),
 (p,(id"main",id"arg",Inttp,Inttp,
  Let (id"x",Call (Id(id"add"),Id(id"y")),
  Let (id"z",Call (Id(id"add"),Tuple [Int 0,Id(id"add")]),
       Call (Id(id"add"),Tuple [Tuple [Int 0,Int 1],Tuple []])))))]

(* fun inc(x:int ref):int = let a = !x in 
      let b = x := a + 1 in a
   fun main(arg:int):int =
      let x = ref 3 in 
        <inc(x), inc(x), inc(x), printint(1), printint(2)>
*)
val prog5 =
[((1,1),(id"inc", id"x", Reftp(Inttp), Inttp,
  Let (id"a", Op(Get,[Id(id"x")]),
       Let (id"b", Op(Set, [Id(id"x"), Op(Add,[Id(id"a"), Int 1])]),
            Id(id"a"))))),
 (p,(id"main",id"arg",Inttp,Inttp,
  Pos((2,2),
   Let (id"x", Op(Ref, [Int 3]), 
   Pos((3,3),
     Tuple([ Call(Id(id"inc"),Id(id"x")),
             Call(Id(id"inc"),Id(id"x")),
             Call(Id(id"inc"),Id(id"x")),
             Call(Id(id"printint"), Int 1),
             Call(Id(id"printint"), Int 2)]))))))]

fun runprog p = let
    val _ = print "Program Code:\n"
    val _ = FunPP.print_prog p
    val v = eval_prog p
in
    print "\nProgram Output:\n";
    FunPP.print_val v;
    print "\n\n"
end 
    handle Eval => 
	   print ("Evaluation failed\n")
         | Unimplemented =>
           print ("\nI can't run this.  You haven't finished implementing the interpreter yet!\n\n")

fun run() = 
    (runprog prog1;
    runprog prog2;
    runprog prog3;
    runprog prog4;
    runprog prog5)

end
