structure MaxArg = struct

    structure S = SLP
    (* Use S.CompoundStm, S.Plus, S.IdExp, etc. to refer to
     * items defined in structure SLP (see slp.sml) *)

    exception MaxUnimplemented

    fun maxarg s = 
      case s of
        S.CompoundStm(s1, s2) =>
          let val args1 = maxarg s1
	      val args2 = maxarg s2
	  in Int.max (args1, args2)
	  end
      | S.AssignStm(x, e) => maxExpArg e
      | S.PrintStm nil => 0 
      | S.PrintStm elist => 
      		   Int.max(Int.max(len elist, maxExpArg (hd(elist):SLP.exp)), 
      		   	   maxarg (S.PrintStm (tl(elist):SLP.exp list)))

    and maxExpArg e =
      case e of
        S.IdExp _ => 0
      | S.NumExp _ => 0
      | S.OpExp(el, _, er) => Int.max(maxExpArg el, maxExpArg er)
      | S.EseqExp(s1, e1) => Int.max(maxarg s1, maxExpArg e1)

    and len L =
    	if L=nil 
	   then 0 
	else 
	   1+len(tl(L));
end
