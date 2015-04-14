functor FunLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Fun_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
structure S = Symbol


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\019\000\010\000\018\000\017\000\031\000\000\000\
\\001\000\001\000\019\000\010\000\018\000\022\000\017\000\000\000\
\\001\000\001\000\019\000\010\000\018\000\022\000\026\000\000\000\
\\001\000\002\000\006\000\000\000\
\\001\000\003\000\086\000\008\000\056\000\013\000\055\000\015\000\054\000\
\\016\000\053\000\017\000\052\000\018\000\051\000\019\000\050\000\
\\020\000\049\000\021\000\048\000\023\000\047\000\024\000\046\000\
\\025\000\045\000\000\000\
\\001\000\004\000\043\000\007\000\042\000\009\000\041\000\012\000\040\000\
\\014\000\039\000\016\000\062\000\018\000\038\000\020\000\037\000\
\\023\000\036\000\027\000\035\000\028\000\034\000\029\000\033\000\000\000\
\\001\000\004\000\043\000\007\000\042\000\009\000\041\000\012\000\040\000\
\\014\000\039\000\018\000\038\000\020\000\037\000\023\000\036\000\
\\027\000\035\000\028\000\034\000\029\000\033\000\000\000\
\\001\000\006\000\078\000\008\000\056\000\013\000\055\000\015\000\054\000\
\\016\000\053\000\017\000\052\000\018\000\051\000\019\000\050\000\
\\020\000\049\000\021\000\048\000\023\000\047\000\024\000\046\000\
\\025\000\045\000\000\000\
\\001\000\008\000\056\000\011\000\077\000\013\000\055\000\015\000\054\000\
\\016\000\053\000\017\000\052\000\018\000\051\000\019\000\050\000\
\\020\000\049\000\021\000\048\000\023\000\047\000\024\000\046\000\
\\025\000\045\000\000\000\
\\001\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\022\000\074\000\023\000\047\000\024\000\046\000\
\\025\000\045\000\000\000\
\\001\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\022\000\080\000\023\000\047\000\024\000\046\000\
\\025\000\045\000\000\000\
\\001\000\016\000\023\000\018\000\016\000\023\000\015\000\027\000\014\000\000\000\
\\001\000\016\000\028\000\026\000\027\000\000\000\
\\001\000\016\000\076\000\026\000\075\000\000\000\
\\001\000\017\000\079\000\000\000\
\\001\000\018\000\016\000\023\000\015\000\027\000\014\000\000\000\
\\001\000\023\000\010\000\000\000\
\\001\000\024\000\012\000\000\000\
\\001\000\024\000\024\000\000\000\
\\001\000\027\000\009\000\000\000\
\\001\000\027\000\011\000\000\000\
\\001\000\027\000\067\000\000\000\
\\001\000\030\000\000\000\000\000\
\\001\000\030\000\007\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\096\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\023\000\047\000\
\\024\000\046\000\025\000\045\000\000\000\
\\097\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\098\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\099\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\100\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\001\000\019\000\010\000\018\000\000\000\
\\104\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\105\000\005\000\085\000\008\000\056\000\013\000\055\000\015\000\054\000\
\\016\000\053\000\017\000\052\000\018\000\051\000\019\000\050\000\
\\020\000\049\000\021\000\048\000\023\000\047\000\024\000\046\000\
\\025\000\045\000\000\000\
\\106\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\107\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\108\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\109\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\110\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\111\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\008\000\056\000\013\000\055\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\018\000\051\000\019\000\050\000\020\000\049\000\
\\021\000\048\000\023\000\047\000\024\000\046\000\025\000\045\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\001\000\019\000\010\000\018\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\001\000\019\000\010\000\018\000\000\000\
\\127\000\001\000\019\000\010\000\018\000\000\000\
\\128\000\002\000\006\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\"
val actionRowNumbers =
"\003\000\023\000\062\000\064\000\
\\019\000\024\000\063\000\016\000\
\\020\000\017\000\015\000\001\000\
\\054\000\015\000\011\000\018\000\
\\057\000\015\000\002\000\012\000\
\\060\000\058\000\015\000\056\000\
\\055\000\015\000\059\000\000\000\
\\061\000\006\000\053\000\028\000\
\\006\000\027\000\006\000\006\000\
\\005\000\006\000\006\000\006\000\
\\006\000\021\000\006\000\006\000\
\\015\000\006\000\046\000\047\000\
\\048\000\050\000\049\000\051\000\
\\006\000\006\000\052\000\033\000\
\\009\000\030\000\013\000\044\000\
\\035\000\031\000\008\000\032\000\
\\007\000\014\000\034\000\029\000\
\\037\000\010\000\041\000\042\000\
\\026\000\006\000\036\000\006\000\
\\006\000\006\000\025\000\045\000\
\\040\000\039\000\004\000\006\000\
\\006\000\038\000\043\000\022\000"
val gotoT =
"\
\\004\000\087\000\007\000\003\000\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\007\000\003\000\008\000\002\000\009\000\006\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\011\000\000\000\
\\000\000\
\\000\000\
\\005\000\018\000\000\000\
\\005\000\020\000\010\000\019\000\000\000\
\\000\000\
\\000\000\
\\005\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\027\000\000\000\
\\000\000\
\\000\000\
\\005\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\030\000\000\000\
\\006\000\042\000\000\000\
\\000\000\
\\001\000\055\000\000\000\
\\000\000\
\\001\000\056\000\000\000\
\\001\000\057\000\000\000\
\\001\000\059\000\002\000\058\000\000\000\
\\001\000\061\000\000\000\
\\001\000\062\000\000\000\
\\001\000\063\000\000\000\
\\001\000\064\000\000\000\
\\000\000\
\\001\000\066\000\000\000\
\\001\000\067\000\000\000\
\\005\000\068\000\000\000\
\\001\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\070\000\000\000\
\\001\000\071\000\000\000\
\\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\000\000\
\\006\000\042\000\000\000\
\\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\000\000\
\\001\000\079\000\000\000\
\\000\000\
\\001\000\080\000\000\000\
\\001\000\081\000\000\000\
\\001\000\082\000\000\000\
\\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\001\000\085\000\000\000\
\\001\000\086\000\000\000\
\\006\000\042\000\000\000\
\\006\000\042\000\000\000\
\\000\000\
\"
val numstates = 88
val numrules = 41
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = ErrorMsg.pos
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | INT of unit ->  (int) | PROJ of unit ->  (int)
 | ID of unit ->  (string) | tplist of unit ->  (A.tp list)
 | funseq of unit ->  (A.fundec list) | fundec of unit ->  (A.fundec)
 | func of unit ->  (A.func) | oper of unit ->  (A.oper)
 | tp of unit ->  (A.tp) | prog of unit ->  (A.prog)
 | explist of unit ->  (A.exp list) | exp of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.prog
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 29) => true | _ => false
val showTerminal =
fn (T 0) => "ARROW"
  | (T 1) => "FUN"
  | (T 2) => "IN"
  | (T 3) => "LET"
  | (T 4) => "ELSE"
  | (T 5) => "THEN"
  | (T 6) => "IF"
  | (T 7) => "ASSIGN"
  | (T 8) => "BANG"
  | (T 9) => "REF"
  | (T 10) => "DO"
  | (T 11) => "WHILE"
  | (T 12) => "OR"
  | (T 13) => "NOT"
  | (T 14) => "AND"
  | (T 15) => "GT"
  | (T 16) => "EQ"
  | (T 17) => "LT"
  | (T 18) => "TIMES"
  | (T 19) => "MINUS"
  | (T 20) => "PLUS"
  | (T 21) => "RPAREN"
  | (T 22) => "LPAREN"
  | (T 23) => "COLON"
  | (T 24) => "SEMICOLON"
  | (T 25) => "COMMA"
  | (T 26) => "ID"
  | (T 27) => "PROJ"
  | (T 28) => "INT"
  | (T 29) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 29) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, EOF1right)) :: ( _, ( MlyValue.funseq funseq1,
 funseq1left, _)) :: rest671)) => let val  result = MlyValue.prog (fn
 _ => let val  (funseq as funseq1) = funseq1 ()
 in (funseq)
end)
 in ( LrTable.NT 3, ( result, funseq1left, EOF1right), rest671)
end
|  ( 1, ( ( _, ( _, _, (RPARENright as RPAREN1right))) :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left,
 _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.Pos((exp1left,RPARENright), 
	                             A.Call (exp1,exp2))
)
end)
 in ( LrTable.NT 0, ( result, exp1left, RPAREN1right), rest671)
end
|  ( 2, ( ( _, ( _, _, (RPARENright as RPAREN1right))) :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (LPARENleft as LPAREN1left), _)
) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
exp1 = exp1 ()
 in (A.Pos((LPARENleft, RPARENright), exp1))
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), (IDright as 
ID1right))) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  ID1 = ID1 ()
 in (A.Pos((IDleft, IDright), A.Id(S.symbol(ID1))))
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.INT INT1, (INTleft as INT1left), (INTright
 as INT1right))) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  INT1 = INT1 ()
 in (A.Pos((INTleft, INTright), A.Int(INT1)))
end)
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
let val it1 = exp1
											  val it2 = exp2
										 in A.Pos((0, 0), A.Id(S.symbol(""))) end
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 in (A.Pos((MINUSleft, exp1right), A.Op(A.Sub,(A.Int(0)::exp1::nil))))

end)
 in ( LrTable.NT 0, ( result, MINUS1left, exp1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
NOTleft as NOT1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (
A.Pos((NOTleft, exp1right), if exp = A.Int(0) then A.Int(1) else A.Int(0))
)
end)
 in ( LrTable.NT 0, ( result, NOT1left, exp1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
BANGleft as BANG1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 in (A.Pos((BANGleft, exp1right), A.Op(A.Get,(exp1::nil))))
end)
 in ( LrTable.NT 0, ( result, BANG1left, exp1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.PROJ PROJ1, (PROJleft as PROJ1left), _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  (PROJ as PROJ1) = PROJ1
 ()
 val  exp1 = exp1 ()
 in (A.Pos((PROJleft, exp1right), A.Proj(PROJ, exp1)))
end)
 in ( LrTable.NT 0, ( result, PROJ1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.oper oper1, _, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  (oper as oper1) = oper1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left, exp2right), A.Op(oper,(exp1::exp2::nil))))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( _, GTleft, GT1right)) :: ( _, ( _, (LTleft as 
LT1left), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 (A.Pos((LTleft, GTleft), A.Tuple(nil))))
 in ( LrTable.NT 0, ( result, LT1left, GT1right), rest671)
end
|  ( 12, ( ( _, ( _, _, (GTright as GT1right))) :: ( _, ( 
MlyValue.explist explist1, _, _)) :: ( _, ( _, (LTleft as LT1left), _)
) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
explist as explist1) = explist1 ()
 in (A.Pos((LTleft, GTright), A.Tuple(explist)))
end)
 in ( LrTable.NT 0, ( result, LT1left, GT1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.tp tp1, _, tp1right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  (tp as tp1) = tp1 ()
 in (A.Pos((exp1left, tp1right), A.Constrain(exp1, tp)))
end)
 in ( LrTable.NT 0, ( result, exp1left, tp1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (A.Pos((IFleft, exp3right), A.If(exp1, exp2, exp3)))
end)
 in ( LrTable.NT 0, ( result, IF1left, exp3right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((IFleft, exp2right), A.If(exp1, exp2, A.Tuple(nil))))
end)
 in ( LrTable.NT 0, ( result, IF1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((WHILEleft, exp2right), A.While(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, WHILE1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
if (exp1 <> A.Int(0)) then 
												(
												if (exp2 <> A.Int(0)) then 
													A.Pos((exp1left, exp2right), A.Int(1))
												else 
													A.Pos((exp1left, exp2right), A.Int(0))
												)
										  else
										  		A.Pos((exp1left, exp2right), A.Int(0)) 
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
if (exp1 <> A.Int(0)) then
												(
												A.Pos((exp1left, exp2right), A.Int(1))
												)
										  else	
										  		(
												if (exp2 <> A.Int(0)) then
													(
														A.Pos((exp1left, exp2right), A.Int(1))
													)
												else
													A.Pos((exp1left, exp2right), A.Int(0))
												)									
										 
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: (
 _, ( _, (LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((LETleft, exp2right), A.Let(S.symbol(ID1), exp1, exp2)))

end)
 in ( LrTable.NT 0, ( result, LET1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.explist (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp::nil)
end)
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.explist explist1, explist1left, _)) :: rest671)) => let val  
result = MlyValue.explist (fn _ => let val  (explist as explist1) = 
explist1 ()
 val  (exp as exp1) = exp1 ()
 in (explist @ (exp::nil))
end)
 in ( LrTable.NT 1, ( result, explist1left, exp1right), rest671)
end
|  ( 22, ( ( _, ( _, PLUS1left, PLUS1right)) :: rest671)) => let val  
result = MlyValue.oper (fn _ => (A.Add))
 in ( LrTable.NT 5, ( result, PLUS1left, PLUS1right), rest671)
end
|  ( 23, ( ( _, ( _, MINUS1left, MINUS1right)) :: rest671)) => let
 val  result = MlyValue.oper (fn _ => (A.Sub))
 in ( LrTable.NT 5, ( result, MINUS1left, MINUS1right), rest671)
end
|  ( 24, ( ( _, ( _, TIMES1left, TIMES1right)) :: rest671)) => let
 val  result = MlyValue.oper (fn _ => (A.Mul))
 in ( LrTable.NT 5, ( result, TIMES1left, TIMES1right), rest671)
end
|  ( 25, ( ( _, ( _, EQ1left, EQ1right)) :: rest671)) => let val  
result = MlyValue.oper (fn _ => (A.Eq))
 in ( LrTable.NT 5, ( result, EQ1left, EQ1right), rest671)
end
|  ( 26, ( ( _, ( _, LT1left, LT1right)) :: rest671)) => let val  
result = MlyValue.oper (fn _ => (A.LT))
 in ( LrTable.NT 5, ( result, LT1left, LT1right), rest671)
end
|  ( 27, ( ( _, ( _, GT1left, GT1right)) :: rest671)) => let val  
result = MlyValue.oper (fn _ => (A.LT))
 in ( LrTable.NT 5, ( result, GT1left, GT1right), rest671)
end
|  ( 28, ( ( _, ( _, ASSIGN1left, ASSIGN1right)) :: rest671)) => let
 val  result = MlyValue.oper (fn _ => (A.Set))
 in ( LrTable.NT 5, ( result, ASSIGN1left, ASSIGN1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.tp tp2, _, _)) :: _ :: _ :: ( _, ( MlyValue.tp tp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _
, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  result = 
MlyValue.func (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  tp1 = tp1 ()
 val  tp2 = tp2 ()
 val  exp1 = exp1 ()
 in ((S.symbol(ID1), S.symbol(ID2), tp1, tp2, exp1))
end)
 in ( LrTable.NT 6, ( result, FUN1left, exp1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), (IDright as 
ID1right))) :: rest671)) => let val  result = MlyValue.tp (fn _ => let
 val  (ID as ID1) = ID1 ()
 in (
if (ID = "Inttp") then
												A.Inttp
										  else 
										  		(ErrorMsg.error((IDleft, IDright), "Invalid type"); A.Inttp)
)
end)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 31, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.tp tp1, _, _
)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  (tp as tp1) = tp1 ()
 in (tp)
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.tp tp2, _, tp2right)) :: _ :: ( _, ( 
MlyValue.tp tp1, tp1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  tp1 = tp1 ()
 val  tp2 = tp2 ()
 in (A.Arrowtp(tp1, tp2))
end)
 in ( LrTable.NT 4, ( result, tp1left, tp2right), rest671)
end
|  ( 33, ( ( _, ( _, _, REF1right)) :: ( _, ( MlyValue.tp tp1, tp1left
, _)) :: rest671)) => let val  result = MlyValue.tp (fn _ => let val  
tp1 = tp1 ()
 in (A.Reftp(tp1))
end)
 in ( LrTable.NT 4, ( result, tp1left, REF1right), rest671)
end
|  ( 34, ( ( _, ( _, _, GT1right)) :: ( _, ( _, LT1left, _)) :: 
rest671)) => let val  result = MlyValue.tp (fn _ => (A.Tupletp(nil)))
 in ( LrTable.NT 4, ( result, LT1left, GT1right), rest671)
end
|  ( 35, ( ( _, ( _, _, GT1right)) :: ( _, ( MlyValue.tplist tplist1,
 _, _)) :: ( _, ( _, LT1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  (tplist as tplist1) = tplist1 ()
 in (A.Tupletp(tplist))
end)
 in ( LrTable.NT 4, ( result, LT1left, GT1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.tp tp1, tp1left, tp1right)) :: rest671)) =>
 let val  result = MlyValue.tplist (fn _ => let val  (tp as tp1) = tp1
 ()
 in (tp::nil)
end)
 in ( LrTable.NT 9, ( result, tp1left, tp1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.tp tp1, _, tp1right)) :: _ :: ( _, ( 
MlyValue.tplist tplist1, tplist1left, _)) :: rest671)) => let val  
result = MlyValue.tplist (fn _ => let val  (tplist as tplist1) = 
tplist1 ()
 val  (tp as tp1) = tp1 ()
 in (tplist @ (tp::nil))
end)
 in ( LrTable.NT 9, ( result, tplist1left, tp1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.funseq (fn _ => let val  (
fundec as fundec1) = fundec1 ()
 in (fundec::nil)
end)
 in ( LrTable.NT 8, ( result, fundec1left, fundec1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.funseq funseq1, _, funseq1right)) :: ( _, (
 MlyValue.fundec fundec1, fundec1left, _)) :: rest671)) => let val  
result = MlyValue.funseq (fn _ => let val  (fundec as fundec1) = 
fundec1 ()
 val  (funseq as funseq1) = funseq1 ()
 in (fundec::funseq)
end)
 in ( LrTable.NT 8, ( result, fundec1left, funseq1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.func func1, (funcleft as func1left), (
funcright as func1right))) :: rest671)) => let val  result = 
MlyValue.fundec (fn _ => let val  (func as func1) = func1 ()
 in (((funcleft, funcright), func))
end)
 in ( LrTable.NT 7, ( result, func1left, func1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Fun_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun BANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun REF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun PROJ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.PROJ (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
end
end
