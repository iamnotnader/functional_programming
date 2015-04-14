type pos = ErrorMsg.pos
type svalue = Tokens.svalue
type ('svalue,'pos) token = ('svalue,'pos) Tokens.token
type lexresult  = (svalue,ErrorMsg.pos) token

val newLine = ErrorMsg.newLine

fun make_pos (yypos,yytext) : ErrorMsg.pos2
    = (yypos, yypos + String.size(yytext) - 1)

(*Declare a reference to an int that holds the number of open comments
  seen so far in the lexing process.*)
val numComments = ref 0

(* Handling EOF.  Note that this function reports the wrong file-position for
   end-of-file.  Because of a design infelicity of ML-Lex, it's possible but
   not easy to get access to the correct file position.  There is a way to 
   do it using the %arg feature of ML-Lex, but you don't need to bother 
   with it for this exercise. 
*)

(*If the number of open comments left after lexing isn't zero, 
  pring "Improper comment" at the end to alert the user.*)
fun eof () = 
		if (not (!numComments = 0))
			then	let val it = ErrorMsg.error((0,0), "Improper comment")
						 val it = (numComments := 0)
			in 
				Tokens.EOF(0,0)
			end
		else Tokens.EOF(0,0)

%%

%s COMMENT;
%header (functor FunLexFun(structure Tokens: Fun_TOKENS));

id = [a-zA-Z][a-zA-Z0-9_]*;
int = [0-9]+;
proj = ("#")(0 | [1-9][0-9]*);
space = " "|\\[rt];

%%
<INITIAL> fun   => (Tokens.FUN(make_pos(yypos,yytext)));
<INITIAL> in => (Tokens.IN(make_pos(yypos,yytext)));
<INITIAL> let => (Tokens.LET(make_pos(yypos,yytext)));
<INITIAL> while => (Tokens.WHILE(make_pos(yypos,yytext)));
<INITIAL> do => (Tokens.DO(make_pos(yypos,yytext)));
<INITIAL> if => (Tokens.IF(make_pos(yypos,yytext)));
<INITIAL> then   => (Tokens.THEN(make_pos(yypos,yytext)));
<INITIAL> else   => (Tokens.ELSE(make_pos(yypos,yytext)));
<INITIAL> ref   => (Tokens.REF(make_pos(yypos,yytext)));
<INITIAL> not   => (Tokens.NOT(make_pos(yypos,yytext)));
<INITIAL> type   => (Tokens.FUN(make_pos(yypos,yytext)));


<INITIAL> "->" => (Tokens.ARROW(make_pos(yypos,yytext)));
<INITIAL> ":=" => (Tokens.ASSIGN(make_pos(yypos,yytext)));
<INITIAL> "(" => (Tokens.LPAREN(make_pos(yypos,yytext)));
<INITIAL> "&" => (Tokens.AND(make_pos(yypos,yytext)));
<INITIAL> "*" => (Tokens.TIMES(make_pos(yypos,yytext)));
<INITIAL> "+" => (Tokens.PLUS(make_pos(yypos,yytext)));
<INITIAL> "-" => (Tokens.MINUS(make_pos(yypos,yytext)));
<INITIAL> "," => (Tokens.COMMA(make_pos(yypos,yytext)));
<INITIAL> "!" => (Tokens.BANG(make_pos(yypos,yytext)));
<INITIAL> ")" => (Tokens.RPAREN(make_pos(yypos,yytext)));
<INITIAL> "||" => (Tokens.OR(make_pos(yypos,yytext)));
<INITIAL> "=" => (Tokens.EQ(make_pos(yypos,yytext)));
<INITIAL> "<" => (Tokens.LT(make_pos(yypos,yytext)));
<INITIAL> ":" => (Tokens.COLON(make_pos(yypos,yytext)));
<INITIAL> ">" => (Tokens.GT(make_pos(yypos,yytext)));
<INITIAL> ";" => (Tokens.SEMICOLON(make_pos(yypos,yytext)));


<INITIAL> {proj} => (Tokens.PROJ(valOf(Int.fromString(implode(tl(explode(yytext))))),yypos, yypos + String.size(yytext) - 1));
<INITIAL> {id} => (Tokens.ID(yytext,yypos, yypos + String.size(yytext) - 1));
<INITIAL> {int} => (Tokens.INT(valOf(Int.fromString(yytext)), yypos, yypos + String.size(yytext) - 1));
<INITIAL> {space} => (continue());


<INITIAL> "/*" => (numComments := !numComments + 1; YYBEGIN COMMENT; continue());
<INITIAL> "*/" => (numComments := 999999999; continue());
<COMMENT> "/*" => (numComments := !numComments + 1; YYBEGIN COMMENT; continue());
<COMMENT> "*/" => (numComments := !numComments - 1; 
							if (!numComments = 0)
								then YYBEGIN INITIAL
							else YYBEGIN COMMENT;
							continue());
<COMMENT> .		  => (continue());
<COMMENT> \n     => (newLine yypos; continue ());


<INITIAL> \n     => (newLine yypos; continue ());

