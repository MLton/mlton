structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

open Tokens

val lineNum = ref 0
val eof = fn () => EOF(!lineNum,!lineNum)


structure KeyWord : sig
	     		val find : string ->
				 (int * int -> (svalue,int) token) option
	  	    end =
  struct

	val TableSize = 211
	val HashFactor = 5

	val hash = fn s =>
	   fold (fn (c,v)=>(v*HashFactor+(ord c)) mod TableSize) (explode s) 0


	val HashTable = Array.array(TableSize,nil) :
		 (string * (int * int -> (svalue,int) token)) list Array.array


	val add = fn (s,v) =>
	 let val i = hash s
	 in Array.update(HashTable,i,(s,v) :: (Array.sub(HashTable, i)))
	 end

        val find = fn s =>
	  let val i = hash s
	      fun f ((key,v)::r) = if s=key then SOME v else f r
	        | f nil = NONE
	  in  f (Array.sub(HashTable, i))
	  end
 
	val _ = 
	    (List.app add
	[("and",YAND),
	 ("array",YARRAY),
	 ("begin",YBEGIN),
	 ("case",YCASE),
	 ("const",YCONST),
	 ("div",YDIV),
	 ("do",YDO),
	 ("downto",YDOWNTO),
	 ("else",YELSE),
	 ("end",YEND),
	 ("extern",YEXTERN),
	 ("file",YFILE),
	 ("for",YFOR),
	 ("forward",YFORWARD),
	 ("function",YFUNCTION),
	 ("goto",YGOTO),
	 ("hex",YHEX),
	 ("if",YIF),
	 ("in",YIN),
	 ("label",YLABEL),
	 ("mod",YMOD),
	 ("nil",YNIL),
	 ("not",YNOT),
	 ("oct",YOCT),
	 ("of",YOF),
	 ("or",YOR),
	 ("packed",YPACKED),
	 ("procedure",YPROCEDURE),
	 ("program",YPROG),
	 ("record",YRECORD),
	 ("repeat",YREPEAT),
	 ("set",YSET),
	 ("then",YTHEN),
	 ("to",YTO),
	 ("type",YTYPE),
	 ("until",YUNTIL),
	 ("var",YVAR),
	 ("while",YWHILE),
	 ("with",YWITH)
	])
   end
   open KeyWord

%%

%header (functor PascalLexFun(structure Tokens : Pascal_TOKENS));
%s C B;
alpha=[A-Za-z];
digit=[0-9];
optsign=("+"|"-")?;
integer={digit}+;
frac="."{digit}+;
exp=(e|E){optsign}{digit}+;
octdigit=[0-7];
ws = [\ \t];
%%
<INITIAL>{ws}+	=> (lex());
<INITIAL>\n+	=> (lineNum := (!lineNum) + (String.length yytext); lex());
<INITIAL>{alpha}+ => (case find yytext of SOME v => v(!lineNum,!lineNum)
						  | _ => YID(!lineNum,!lineNum));
<INITIAL>{alpha}({alpha}|{digit})*  => (YID(!lineNum,!lineNum));
<INITIAL>{optsign}{integer}({frac}{exp}?|{frac}?{exp}) => (YNUMB(!lineNum,!lineNum));
<INITIAL>{optsign}{integer} => (YINT(!lineNum,!lineNum));
<INITIAL>{octdigit}+(b|B) => (YBINT(!lineNum,!lineNum));
<INITIAL>"'"([^']|"''")*"'" => (YSTRING(!lineNum,!lineNum));
<INITIAL>"(*" =>   (YYBEGIN C; lex());
<INITIAL>".."	=> (YDOTDOT(!lineNum,!lineNum));
<INITIAL>"."	=> (YDOT(!lineNum,!lineNum));
<INITIAL>"("	=> (YLPAR(!lineNum,!lineNum));
<INITIAL>")"	=> (YRPAR(!lineNum,!lineNum));
<INITIAL>";"	=> (YSEMI(!lineNum,!lineNum));
<INITIAL>","	=> (YCOMMA(!lineNum,!lineNum));
<INITIAL>":"	=> (YCOLON(!lineNum,!lineNum));
<INITIAL>"^"	=> (YCARET(!lineNum,!lineNum));
<INITIAL>"["	=> (YLBRA(!lineNum,!lineNum));
<INITIAL>"]"	=> (YRBRA(!lineNum,!lineNum));
<INITIAL>"~"	=> (YTILDE(!lineNum,!lineNum));
<INITIAL>"<"	=> (YLESS(!lineNum,!lineNum));
<INITIAL>"="	=> (YEQUAL(!lineNum,!lineNum));
<INITIAL>">"	=> (YGREATER(!lineNum,!lineNum));
<INITIAL>"+"	=> (YPLUS(!lineNum,!lineNum));
<INITIAL>"-"	=> (YMINUS(!lineNum,!lineNum));
<INITIAL>"|"	=> (YBAR(!lineNum,!lineNum));
<INITIAL>"*"	=> (YSTAR(!lineNum,!lineNum));
<INITIAL>"/"	=> (YSLASH(!lineNum,!lineNum));
<INITIAL>"{"	=> (YYBEGIN B; lex());
<INITIAL>.	=> (YILLCH(!lineNum,!lineNum));
<C>\n+		=> (lineNum := (!lineNum) + (String.length yytext); lex());
<C>[^()*\n]+	=> (lex());
<C>"(*"		=> (lex());
<C>"*)"		=> (YYBEGIN INITIAL; lex());
<C>[*()]	=> (lex());
<B>\n+		=> (lineNum := (!lineNum) + (String.length yytext); lex());
<B>[^{}\n]+	=> (lex());
<B>"{"		=> (lex());
<B>"}"		=> (YYBEGIN INITIAL; lex());
