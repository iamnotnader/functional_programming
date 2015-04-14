structure TestProg = struct

    structure S = SLP

    val prog = 
	S.CompoundStm
           (S.AssignStm ("a", S.OpExp (S.NumExp 5, S.Plus, S.NumExp 3)),
	    S.CompoundStm
	         (S.AssignStm
		    ("b",
		     S.EseqExp
		       (S.PrintStm
			[S.IdExp "a",
			 S.OpExp (S.IdExp "a", S.Minus, S.NumExp 1)],
			S.OpExp (S.NumExp 10, S.Times, S.IdExp "a"))),
		    S.CompoundStm(S.PrintStm([S.NumExp 1, S.NumExp 1, S.NumExp 2,S.NumExp 3]), S.PrintStm [S.IdExp "b"])))


    val prog2 = S.CompoundStm(S.AssignStm("a", S.OpExp(S.NumExp 5, S.Plus, S.NumExp 3)), S.PrintStm[S.IdExp "a", (S.OpExp(S.NumExp 1, S.Plus, S.NumExp 100)),S.NumExp 2,S.NumExp 3]);

    val prog3 = S.CompoundStm(S.CompoundStm(S.AssignStm("a", S.NumExp 5), S.CompoundStm(S.AssignStm("b", S.NumExp 2), S.AssignStm("a", S.IdExp "b"))), S.PrintStm([S.OpExp(S.IdExp "a",S.Plus, S.IdExp "b")]))

end
