type int = Int.t
   
type svalue = Tokens.svalue
type pos = SourcePos.t
type lexresult = (svalue, pos) Tokens.token
type lexarg = {source: Source.t}
type arg = lexarg
type ('a,'b) token = ('a,'b) Tokens.token

val charlist: string list ref = ref []
val colNum: int ref = ref 0
val commentLevel: int ref = ref 0
val commentStart = ref SourcePos.bogus
val lineFile: File.t ref = ref ""
val lineNum: int ref = ref 0

fun lineDirective (source, file, yypos) =
   Source.lineDirective (source, file,
			 {lineNum = !lineNum,
			  lineStart = yypos - !colNum})

fun inc (ri as ref (i: int)) = (ri := i + 1)
fun dec (ri as ref (i: int)) = (ri := i-1)

fun error (source, left, right, msg) = 
   Control.errorStr (Region.make {left = Source.getPos (source, left),
				  right = Source.getPos (source, right)},
		     msg)

val eof: lexarg -> lexresult =
   fn {source, ...} =>
   let
      val pos = Source.lineStart source
      val _ =
	 if !commentLevel > 0
	    then Control.errorStr (Region.make {left = !commentStart,
						right = pos},
				   "unclosed comment")
	 else ()
   in
      Tokens.EOF (pos, pos)
   end

val size = String.size

fun tok (t, s, l, r) =
   let
      val l = Source.getPos (s, l)
      val r = Source.getPos (s, r)
      val _ =
	 if true
	    then ()
	 else
	    print (concat ["tok (",
			   SourcePos.toString l,
			   ", " ,
			   SourcePos.toString r,
			   ")\n"])
   in
      t (l, r)
   end

fun tok' (t, x, s, l) = tok (fn (l, r) => t (x, l, r), s, l, l + size x)

%% 
%reject
%s A L LL LLC LLCQ;
%header (functor MLBLexFun (structure Tokens : MLB_TOKENS));
%arg ({source});
alphanum=[A-Za-z'_0-9]*;
alphanumId=[A-Za-z]{alphanum};
id={alphanumId};
envvar="$("([A-Z_]+)")";
filebase=[-A-Za-z_0-9]+;
fileext=[-A-Za-z_0-9]+;
filename={filebase}("."{fileext})*;
arc=({envvar}|{filename}|"."|"..");
relpath=({arc}"/")*;
abspath="/"{relpath};
path={relpath}|{abspath};
file={path}{filename};
ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
cr="\013";
nl="\010";
eol=({cr}{nl}|{nl}|{cr});

%%
<INITIAL>{ws}	=> (continue ());
<INITIAL>{eol}	=> (Source.newline (source, yypos); continue ());

<INITIAL>","	=> (tok (Tokens.COMMA, source, yypos, yypos + 1));
<INITIAL>";"	=> (tok (Tokens.SEMICOLON, source, yypos, yypos + 1));
<INITIAL>"=" => (tok (Tokens.EQUALOP, source, yypos, yypos + 1));
<INITIAL>"ann" => (tok (Tokens.ANN, source, yypos, yypos + 3));
<INITIAL>"and" => (tok (Tokens.AND, source, yypos, yypos + 3));
<INITIAL>"bas" => (tok (Tokens.BAS, source, yypos, yypos + 3));
<INITIAL>"basis" => (tok (Tokens.BASIS, source, yypos, yypos + 5));
<INITIAL>"end" => (tok (Tokens.END, source, yypos, yypos + 3));
<INITIAL>"functor" => (tok (Tokens.FUNCTOR, source, yypos, yypos + 7));
<INITIAL>"in" => (tok (Tokens.IN, source, yypos, yypos + 2));
<INITIAL>"let" => (tok (Tokens.LET, source, yypos, yypos + 3));
<INITIAL>"local" => (tok (Tokens.LOCAL, source, yypos, yypos + 5));
<INITIAL>"open" => (tok (Tokens.OPEN, source, yypos, yypos + 4));
<INITIAL>"_prim" => (tok (Tokens.PRIM, source, yypos, yypos + 4));
<INITIAL>"signature" => (tok (Tokens.SIGNATURE, source, yypos, yypos + 9));
<INITIAL>"structure" => (tok (Tokens.STRUCTURE, source, yypos, yypos + 9));
<INITIAL>{id} => (tok' (Tokens.ID, yytext, source, yypos));
<INITIAL>{file} => (tok' (Tokens.FILE, yytext, source, yypos));

<INITIAL>"(*#line"{nrws}
                => (YYBEGIN L
		    ; commentStart := Source.getPos (source, yypos)
		    ; commentLevel := 1
		    ; continue ());
<INITIAL>"(*"	=> (YYBEGIN A
                    ; commentLevel := 1
                    ; commentStart := Source.getPos (source, yypos)
                    ; continue ());
<INITIAL>.	=> (error (source, yypos, yypos + 1, "illegal token") ;
		    continue ());

<L>[0-9]+       => (YYBEGIN LL
                    ; (lineNum := valOf (Int.fromString yytext)
                       ; colNum := 1)
                      handle Overflow => YYBEGIN A
                    ; continue ());
<LL>\.          => ((* cheat: take n > 0 dots *) continue ());
<LL>[0-9]+      => (YYBEGIN LLC
		    ; (colNum := valOf (Int.fromString yytext))
		      handle Overflow => YYBEGIN A
	            ; continue ());
<LL>.          => (YYBEGIN LLC; continue ()
		(* note hack, since ml-lex chokes on the empty string for 0* *));
<LLC>"*)"       => (YYBEGIN INITIAL
		    ; lineDirective (source, NONE, yypos + 2)
		    ; commentLevel := 0; charlist := []; continue ());
<LLC>{ws}\"	=> (YYBEGIN LLCQ; continue ());
<LLCQ>[^\"]*    => (lineFile := yytext; continue ());
<LLCQ>\""*)"    => (YYBEGIN INITIAL
                    ; lineDirective (source, SOME (!lineFile), yypos + 3)
                    ; commentLevel := 0; charlist := []; continue ());
<L,LLC,LLCQ>"*)" => (YYBEGIN INITIAL; commentLevel := 0; charlist := []; continue ());
<L,LLC,LLCQ>.   => (YYBEGIN A; continue ());

<A>"(*"		=> (inc commentLevel; continue ());
<A>\n		=> (Source.newline (source, yypos) ; continue ());
<A>"*)"         => (dec commentLevel
		    ; if 0 = !commentLevel then YYBEGIN INITIAL else ()
		    ; continue ());
<A>.		=> (continue ());
