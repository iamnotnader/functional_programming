structure TestCases = struct

(* NOTE:  Some of these testcases are not semantically correct (they don't type-check)
 * but they are SYNTACTICALLY correct (they parse)
 *)

 val all = [
( "fun f(x:<>):<>=  a;b"
, "fun f(x:<>):<>=  ((a);b)"
),
( "fun f(x:<>):<>=  if a then b else c"
, "fun f(x:<>):<>=  (if a then b else c)"
),
( "fun f(x:<>):<>=  if a then b else c ; d"
, "fun f(x:<>):<>=  (if a then b else c) ; d"
),
( "fun f(x:<>):<>=  if a then b ; d"
, "fun f(x:<>):<>=  (if a then b else <>) ; d"
),
( "fun f(x:<>):<>=  if a then if b then c else d"
, "fun f(x:<>):<>=  if a then (if b then c else d)"
),
( "fun f(x:<>):<>=  if if a then b else c then if b then c else d"
, "fun f(x:<>):<>=  if (if a then b else c) then (if b then c else d)"
),
( "fun f(x:<>):<>=  a*b+c*d"
, "fun f(x:<>):<>=  (a*b)+(c*d)"
),
( "fun f(x:<>):<>=  a+b-c"
, "fun f(x:<>):<>=  (a+b)-c"
),
( "fun f(x:<>):<>=  !a+!b"
, "fun f(x:<>):<>=  (!a)+(!b)"
),
( "fun f(x:<>):<>=  a := b ; c := d"
, "fun f(x:<>):<>=  (a := b) ; (c := d)"
),
( "fun f(x:<>):<>=  (f)(x)"
, "fun f(x:<>):<>=  f(x)"
),
( "fun f(x:<>):<>=  a + b : int ref"
, "fun f(x:<>):<>=  (a + b) : (int ref)"
),
( "fun f(x:<>):<>=  if a = b then c else d : int"
, "fun f(x:<>):<>=  if a = b then c else (d : int)"
),
( "fun f(x:<>):<>=  <if a then b , c>"
, "fun f(x:<>):<>=  <(if a then b else <>), c>"
),
( "fun f(x:<>):<>=  !a*ref b+#8 c=-d &not e||h:int := j:int ref;<>"
, "fun f(x:<>):<>=  ((((((((!a)*(ref b))+(#8 c))=(-d)) &(not e))||h):int) := (j:(int ref)));<>"
),
( "fun f(x:<>):<>=  /* comment */ a"
, "fun f(x:<>):<>=  a"
),
( "fun f(x:<>):<>=  /* comment /* nested comment */ */ a"
, "fun f(x:<>):<>=  a"
)
]

end

