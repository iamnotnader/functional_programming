structure Compile = 
struct 
  structure A = Absyn

  fun string2absyn s = Parse.parse ("", TextIO.openString s)
         handle e => (print "Parse failed on the string,\n";
	              print s; print "\n";
                      raise e)

  fun strip(A.Pos(_,e))     = strip e
    | strip(A.Constrain(e,t)) = A.Constrain(strip e, t)
    | strip(A.Op(oper,el))  = A.Op(oper, map strip el)
    | strip(A.Tuple(el))    = A.Tuple(map strip el)
    | strip(A.Proj(i,e))    = A.Proj(i,strip e)
    | strip(A.If(e1,e2,e3)) = A.If(strip e1, strip e2, strip e3)
    | strip(A.Call(e1,e2))  = A.Call(strip e1, strip e2)
    | strip(A.While(e1,e2)) = A.While(strip e1, strip e2)
    | strip(A.Let(i,e1,e2)) = A.Let(i,strip e1, strip e2)
    | strip(e)                = e

  fun strip_fundec(_,(f,x,s,t,e)) = ((0,0),(f,x,s,t,strip e))

  fun test(s1,s2) : Absyn.prog * Absyn.prog =
    let val a1 = map strip_fundec (string2absyn s1)
        val a2 = map strip_fundec (string2absyn s2)
     in if a1=a2 then print "OK\n" else 
        (print "*******Parse equivalence failed on the strings,\n";
	              print s1; print "\n";
	              print s2; print "\n");
        (a1,a2)
    end

  fun go() = map test TestCases.all

  fun compile filename = 
    let 
      val absyn = Parse.parse (filename, TextIO.openIn filename)
    in
      if (!ErrorMsg.anyErrors) then print ("\nLexing/Parsing Failed\n\n")
      else (print "Program:\n"; 
	    FunPP.print_prog absyn;
	    print "Running Program...\n";
	    let val v = Eval.eval_prog absyn
             in print "\nProgram Output:\n";
	        FunPP.print_val v;
	        print "\n"
            end)
    end handle ErrorMsg.Error => print ("\nParsing failed\n")
             | Eval.Eval => print ("\nEvaluation failed\n")
end