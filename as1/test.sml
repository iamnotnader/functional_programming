structure Test = struct

    fun interp () = Interp.interp TestProg.prog
    fun maxarg () = MaxArg.maxarg TestProg.prog

end
