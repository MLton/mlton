(* Heavily modified from the SML/NJ sources by sweeks@acm.org. *)

(* ml.lex
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * $Log: ml.lex,v $
 * Revision 1.3  1997/05/22  20:17:22  jhr
 * Changed lexer to accept "1e1" style floating-point literals.
 *
 * Revision 1.2  1997/01/28  23:20:40  jhr
 * Integer and word literals are now represented by IntInf.int (instead of
 * as strings).
 *
 *)

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
val stringStart = ref SourcePos.bogus
val stringtype = ref false

fun lineDirective (source, file, yypos) =
   Source.lineDirective (source, file,
			 {lineNum = !lineNum,
			  lineStart = yypos - !colNum})
fun addString (s: string) = charlist := s :: (!charlist)
fun addChar (c: char) = addString (String.fromChar c)

fun inc (ri as ref (i: int)) = (ri := i + 1)
fun dec (ri as ref (i: int)) = (ri := i-1)

fun error (source, left, right, msg) = 
   Control.error (Region.make {left = Source.getPos (source, left),
			       right = Source.getPos (source, right)},
		  Layout.str msg)

fun stringError (source, right, msg) =
   Control.error (Region.make {left = !stringStart,
			       right = Source.getPos (source, right)},
		  Layout.str msg)

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
fun tok' (t, x, s, l) = tok (fn (l, r) => t (x, l, r), s, l, l + String.size x)

local 
   fun make (scan, token, default, msg) (radix, str, source, left) =
      let
	 val right = left + String.size str
      in
	 token ((case StringCvt.scanString (scan radix) str of
		    NONE => (error (source, left, right,
				    concat ["invalid ", msg, " constant"])
			     ; default)
		  | SOME n => n)
		handle Overflow =>
		   (error (source, left, right,
			   concat [msg, " constant out of range"])
		    ; default),
		   Source.getPos (source, left),
		   Source.getPos (source, right))
      end
in 
   val makeWord = make (Word32.scan, Tokens.WORD, 0w0, "word")
end

%% 
%reject
%s A S F L LL LLC LLCQ;
%header (functor MLLexFun (structure Tokens : ML_TOKENS));
%arg ({source});
alphanum=[A-Za-z'_0-9]*;
alphanumId=[A-Za-z]{alphanum};
sym=[-!%&$+/:<=>?@~`^|#*]|"\\";
symId={sym}+;
id={alphanumId}|{symId};
longid={id}("."{id})*;
ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
cr="\013";
nl="\010";
eol=({cr}{nl}|{nl}|{cr});
num=[0-9]+;
frac="."{num};
exp=[eE](~?){num};
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
hexDigit=[0-9a-fA-F];
hexnum={hexDigit}+;

%%
<INITIAL>{ws}	=> (continue ());
<INITIAL>{eol}	=> (Source.newline (source, yypos); continue ());
<INITIAL>"_overload" => (tok (Tokens.OVERLOAD, source, yypos, yypos + 9));
<INITIAL>"_prim" => (tok (Tokens.PRIM, source, yypos, yypos + 5));
<INITIAL>"_ffi" => (tok (Tokens.FFI, source, yypos, yypos + 5));
<INITIAL>"_"	=> (tok (Tokens.WILD, source, yypos, yypos + 1));
<INITIAL>","	=> (tok (Tokens.COMMA, source, yypos, yypos + 1));
<INITIAL>"{"	=> (tok (Tokens.LBRACE, source, yypos, yypos + 1));
<INITIAL>"}"	=> (tok (Tokens.RBRACE, source, yypos, yypos + 1));
<INITIAL>"["	=> (tok (Tokens.LBRACKET, source, yypos, yypos + 1));
<INITIAL>"]"	=> (tok (Tokens.RBRACKET, source, yypos, yypos + 1));
<INITIAL>";"	=> (tok (Tokens.SEMICOLON, source, yypos, yypos + 1));
<INITIAL>"("	=> (tok (Tokens.LPAREN, source, yypos, yypos + 1));
<INITIAL>")"	=> (tok (Tokens.RPAREN, source, yypos, yypos + 1));
<INITIAL>"..."  => (tok (Tokens.DOTDOTDOT, source, yypos, yypos + 3));
<INITIAL>"|" => (tok (Tokens.BAR, source, yypos, yypos + 1));
<INITIAL>":" => (tok (Tokens.COLON, source, yypos, yypos + 1));
<INITIAL>":>" => (tok (Tokens.COLONGT, source, yypos, yypos + 1));
<INITIAL>"=" => (tok (Tokens.EQUALOP, source, yypos, yypos + 1));
<INITIAL>"#" => (tok (Tokens.HASH, source, yypos, yypos + 1));
<INITIAL>"->" => (tok (Tokens.ARROW, source, yypos, yypos + 2));
<INITIAL>"=>" => (tok (Tokens.DARROW, source, yypos, yypos + 2));
<INITIAL>"and" => (tok (Tokens.AND, source, yypos, yypos + 3));
<INITIAL>"abstype" => (tok (Tokens.ABSTYPE, source, yypos, yypos + 7));
<INITIAL>"as" => (tok (Tokens.AS, source, yypos, yypos + 2));
<INITIAL>"case" => (tok (Tokens.CASE, source, yypos, yypos + 4));
<INITIAL>"datatype" => (tok (Tokens.DATATYPE, source, yypos, yypos + 8));
<INITIAL>"else" => (tok (Tokens.ELSE, source, yypos, yypos + 4));
<INITIAL>"end" => (tok (Tokens.END, source, yypos, yypos + 3));
<INITIAL>"eqtype" => (tok (Tokens.EQTYPE, source, yypos, yypos + 6));
<INITIAL>"exception" => (tok (Tokens.EXCEPTION, source, yypos, yypos + 9));
<INITIAL>"do" => (tok (Tokens.DO, source, yypos, yypos + 2));
<INITIAL>"fn" => (tok (Tokens.FN, source, yypos, yypos + 2));
<INITIAL>"fun" => (tok (Tokens.FUN, source, yypos, yypos + 3));
<INITIAL>"functor" => (tok (Tokens.FUNCTOR, source, yypos, yypos + 7));
<INITIAL>"funsig" => (tok (Tokens.FUNSIG, source, yypos, yypos + 7));
<INITIAL>"handle" => (tok (Tokens.HANDLE, source, yypos, yypos + 6));
<INITIAL>"if" => (tok (Tokens.IF, source, yypos, yypos + 2));
<INITIAL>"in" => (tok (Tokens.IN, source, yypos, yypos + 2));
<INITIAL>"include" => (tok (Tokens.INCLUDE, source, yypos, yypos + 7));
<INITIAL>"infix" => (tok (Tokens.INFIX, source, yypos, yypos + 5));
<INITIAL>"infixr" => (tok (Tokens.INFIXR, source, yypos, yypos + 6));
<INITIAL>"let" => (tok (Tokens.LET, source, yypos, yypos + 3));
<INITIAL>"local" => (tok (Tokens.LOCAL, source, yypos, yypos + 5));
<INITIAL>"nonfix" => (tok (Tokens.NONFIX, source, yypos, yypos + 6));
<INITIAL>"of" => (tok (Tokens.OF, source, yypos, yypos + 2));
<INITIAL>"op" => (tok (Tokens.OP, source, yypos, yypos + 2));
<INITIAL>"open" => (tok (Tokens.OPEN, source, yypos, yypos + 4));
<INITIAL>"raise" => (tok (Tokens.RAISE, source, yypos, yypos + 5));
<INITIAL>"rec" => (tok (Tokens.REC, source, yypos, yypos + 3));
<INITIAL>"sharing" => (tok (Tokens.SHARING, source, yypos, yypos + 7));
<INITIAL>"sig" => (tok (Tokens.SIG, source, yypos, yypos + 3));
<INITIAL>"signature" => (tok (Tokens.SIGNATURE, source, yypos, yypos + 9));
<INITIAL>"struct" => (tok (Tokens.STRUCT, source, yypos, yypos + 6));
<INITIAL>"structure" => (tok (Tokens.STRUCTURE, source, yypos, yypos + 9));
<INITIAL>"then" => (tok (Tokens.THEN, source, yypos, yypos + 4));
<INITIAL>"type" => (tok (Tokens.TYPE, source, yypos, yypos + 4));
<INITIAL>"val" => (tok (Tokens.VAL, source, yypos, yypos + 3));
<INITIAL>"where" => (tok (Tokens.WHERE, source, yypos, yypos + 5));
<INITIAL>"while" => (tok (Tokens.WHILE, source, yypos, yypos + 5));
<INITIAL>"with" => (tok (Tokens.WITH, source, yypos, yypos + 4));
<INITIAL>"withtype" => (tok (Tokens.WITHTYPE, source, yypos, yypos + 8));
<INITIAL>"orelse" => (tok (Tokens.ORELSE, source, yypos, yypos + 6));
<INITIAL>"andalso" => (tok (Tokens.ANDALSO, source, yypos, yypos + 7));
<INITIAL>"'"{alphanum}? => (tok' (Tokens.TYVAR, yytext, source, yypos));
<INITIAL>{longid} => (case yytext of
			 "*" => tok (Tokens.ASTERISK, source, yypos, yypos + 1)
		       | _ => tok' (Tokens.LONGID, yytext, source, yypos));
<INITIAL>{real}	=> (tok' (Tokens.REAL, yytext, source, yypos));
<INITIAL>{num}	=> (tok' (Tokens.INT, yytext, source, yypos));
<INITIAL>~{num}	=> (tok' (Tokens.INT, yytext, source, yypos));
<INITIAL>"0x"{hexnum} => (tok' (Tokens.INT, yytext, source, yypos));
<INITIAL>"~0x"{hexnum} => (tok' (Tokens.INT, yytext, source, yypos));
<INITIAL>"0w"{num} => (makeWord (StringCvt.DEC, yytext, source, yypos));
<INITIAL>"0wx"{hexnum} => (makeWord (StringCvt.HEX, yytext, source, yypos));
<INITIAL>\"	=> (charlist := [""]
                    ; stringStart := Source.getPos (source, yypos)
                    ; stringtype := true
                    ; YYBEGIN S
                    ; continue ());
<INITIAL>\#\"	=> (charlist := [""]
                    ; stringStart := Source.getPos (source, yypos)
                    ; stringtype := false
                    ; YYBEGIN S
                    ; continue ());
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

<S>\"	        => (let
		       val s = concat (rev (!charlist))
		       val _ = charlist := nil
		       fun make (t, v) =
			  t (v, !stringStart, Source.getPos (source, yypos + 1))
                    in YYBEGIN INITIAL;
		       if !stringtype
			  then make (Tokens.STRING, s)
		       else
			  make (Tokens.CHAR,
				if String.size s <> 1 
				   then (error
					 (source, yypos, yypos + 1,
					  "character constant not length 1")
					 ; #"\000")
				else String.sub (s, 0))
                    end);
<S>\\a		=> (addChar #"\a"; continue ());
<S>\\b		=> (addChar #"\b"; continue ());
<S>\\f		=> (addChar #"\f"; continue ());
<S>\\n		=> (addChar #"\n"; continue ());
<S>\\r		=> (addChar #"\r"; continue ());
<S>\\t		=> (addChar #"\t"; continue ());
<S>\\v		=> (addChar #"\v"; continue ());
<S>\\\^[@-_]	=> (addChar (Char.chr(Char.ord(String.sub(yytext, 2))
				      -Char.ord #"@"));
		    continue ());
<S>\\\^.	=>
	(error (source, yypos, yypos + 2,
		"illegal control escape; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_");
	continue ());
<S>\\[0-9]{3}	=> (let
		       val x =
			  Char.ord(String.sub(yytext, 1)) * 100
			  + Char.ord(String.sub(yytext, 2)) * 10
			  + Char.ord(String.sub(yytext, 3))
			  - (Char.ord #"0") *111
		    in (if x > 255
			   then stringError (source, yypos,
					     "illegal ascii escape")
			else addChar(Char.chr x);
			   continue ())
		    end);
<S>\\u{hexDigit}{4} => (let
			   val x = 
			      StringCvt.scanString
			      (Pervasive.Int.scan StringCvt.HEX)
			      (String.substring (yytext, 2, 4))
			   fun err () =
			      stringError (source, yypos,
					   "illegal unicode escape")
			in (case x of
			       SOME x => if x > 255
					    then err()
					 else addChar(Char.chr x)
			     | _ => err())
			   ; continue ()
			end);
<S>\\\"		=> (addString "\""; continue ());
<S>\\\\		=> (addString "\\"; continue ());
<S>\\{ws}   	=> (YYBEGIN F; continue ());
<S>\\{eol}      => (Source.newline (source, yypos) ; YYBEGIN F ; continue ());   
<S>\\		=> (stringError (source, yypos, "illegal string escape")
		    ; continue ());
<S>{eol}	=> (Source.newline (source, yypos)
		    ; stringError (source, yypos, "unclosed string")
		    ; continue ());
<S>" "|[\033-\126]  => (addString yytext; continue ());
<S>. =>  (stringError (source, yypos + 1, "illegal character in string")
	  ; continue ());

<F>{eol}        => (Source.newline (source, yypos) ; continue ());
<F>{ws}		=> (continue ());
<F>\\		=> (YYBEGIN S
		    ; stringStart := Source.getPos (source, yypos)
		    ; continue ());
<F>.		=> (stringError (source, yypos, "unclosed string")
		    ; continue ());
   
