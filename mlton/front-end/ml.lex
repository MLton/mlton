(* Heavily modified from the SML/NJ sources by sweeks@research.nj.nec.com. *)

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
   
fun error (left, right, msg) = 
   Control.error (Region.T {left = left, right = right},
		  Layout.str msg)

local 
   fun make (scan, token, default, msg) (radix, str, left, right) =
      token((case StringCvt.scanString (scan radix) str of
		NONE => (error(left, right, "invalid " ^ msg ^ " constant") ;
			 default)
	      | SOME n => n)
	    handle Overflow =>
	       (error(left, right, msg ^ " constant out of range") ;
		default),
	    left, right)
in 
   val makeWord = make (Word32.scan, Tokens.WORD, 0w0, "word")
end

type svalue = Tokens.svalue
type pos = int
type lexresult = (svalue, pos) Tokens.token
type lexarg = {comLevel: int ref,
	       source: Source.t,
	       stringtype: bool ref,
	       stringstart: int ref}
type arg = lexarg
type ('a,'b) token = ('a,'b) Tokens.token

val eof: lexarg -> lexresult =
   fn {comLevel, ...} => 
   (if !comLevel > 0
       then Control.errorStr (Region.bogus, "unclosed comment")
    else ()
    ; Tokens.EOF (~1, ~1))

val charlist: string list ref = ref []
fun addString (s: string) = charlist := s :: (!charlist)
fun addChar (c: char) = addString (String.fromChar c)

fun inc (ri as ref (i: int)) = (ri := i + 1)
fun dec (ri as ref (i: int)) = (ri := i-1)
   
%% 
%reject
%s A S F;
%header (functor MLLexFun (structure Tokens : ML_TOKENS));
%arg ({comLevel, source, stringstart, stringtype});
alphanum=[A-Za-z'_0-9]*;
alphanumId=[A-Za-z]{alphanum};
sym=[-!%&$+/:<=>?@~`^|#*]|"\\";
symId={sym}+;
id={alphanumId}|{symId};
longid={id}("."{id})*;
ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
eol=("\013\010"|"\010"|"\013");
num=[0-9]+;
frac="."{num};
exp=[eE](~?){num};
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
hexDigit=[0-9a-fA-F];
hexnum={hexDigit}+;
%%
<INITIAL>{ws}	=> (continue());
<INITIAL>\n	=> (Source.newline (source, yypos); continue());
<INITIAL>"_overload" => (Tokens.OVERLOAD(yypos, yypos + 9));
<INITIAL>"_prim" => (Tokens.PRIM(yypos, yypos + 5));
<INITIAL>"_ffi" => (Tokens.FFI(yypos, yypos + 5));
<INITIAL>"_"	=> (Tokens.WILD(yypos, yypos + 1));
<INITIAL>","	=> (Tokens.COMMA(yypos, yypos + 1));
<INITIAL>"{"	=> (Tokens.LBRACE(yypos, yypos + 1));
<INITIAL>"}"	=> (Tokens.RBRACE(yypos, yypos + 1));
<INITIAL>"["	=> (Tokens.LBRACKET(yypos, yypos + 1));
<INITIAL>"]"	=> (Tokens.RBRACKET(yypos, yypos + 1));
<INITIAL>";"	=> (Tokens.SEMICOLON(yypos, yypos + 1));
<INITIAL>"("	=> (Tokens.LPAREN(yypos, yypos + 1));
<INITIAL>")"	=> (Tokens.RPAREN(yypos, yypos + 1));
<INITIAL>"..."  => (Tokens.DOTDOTDOT(yypos, yypos + 3));
<INITIAL>"|" => (Tokens.BAR(yypos, yypos + 1));
<INITIAL>":" => (Tokens.COLON(yypos, yypos + 1));
<INITIAL>":>" => (Tokens.COLONGT(yypos, yypos + 1));
<INITIAL>"=" => (Tokens.EQUALOP(yypos, yypos + 1));
<INITIAL>"#" => (Tokens.HASH(yypos, yypos + 1));
<INITIAL>"->" => (Tokens.ARROW(yypos, yypos + 2));
<INITIAL>"=>" => (Tokens.DARROW(yypos, yypos + 2));
<INITIAL>"and" => (Tokens.AND(yypos, yypos + 3));
<INITIAL>"abstype" => (Tokens.ABSTYPE(yypos, yypos + 7));
<INITIAL>"as" => (Tokens.AS(yypos, yypos + 2));
<INITIAL>"case" => (Tokens.CASE(yypos, yypos + 4));
<INITIAL>"datatype" => (Tokens.DATATYPE(yypos, yypos + 8));
<INITIAL>"else" => (Tokens.ELSE(yypos, yypos + 4));
<INITIAL>"end" => (Tokens.END(yypos, yypos + 3));
<INITIAL>"eqtype" => (Tokens.EQTYPE(yypos, yypos + 6));
<INITIAL>"exception" => (Tokens.EXCEPTION(yypos, yypos + 9));
<INITIAL>"do" => (Tokens.DO(yypos, yypos + 2));
<INITIAL>"fn" => (Tokens.FN(yypos, yypos + 2));
<INITIAL>"fun" => (Tokens.FUN(yypos, yypos + 3));
<INITIAL>"functor" => (Tokens.FUNCTOR(yypos, yypos + 7));
<INITIAL>"funsig" => (Tokens.FUNSIG(yypos, yypos + 7));
<INITIAL>"handle" => (Tokens.HANDLE(yypos, yypos + 6));
<INITIAL>"if" => (Tokens.IF(yypos, yypos + 2));
<INITIAL>"in" => (Tokens.IN(yypos, yypos + 2));
<INITIAL>"include" => (Tokens.INCLUDE(yypos, yypos + 7));
<INITIAL>"infix" => (Tokens.INFIX(yypos, yypos + 5));
<INITIAL>"infixr" => (Tokens.INFIXR(yypos, yypos + 6));
<INITIAL>"let" => (Tokens.LET(yypos, yypos + 3));
<INITIAL>"local" => (Tokens.LOCAL(yypos, yypos + 5));
<INITIAL>"nonfix" => (Tokens.NONFIX(yypos, yypos + 6));
<INITIAL>"of" => (Tokens.OF(yypos, yypos + 2));
<INITIAL>"op" => (Tokens.OP(yypos, yypos + 2));
<INITIAL>"open" => (Tokens.OPEN(yypos, yypos + 4));
<INITIAL>"raise" => (Tokens.RAISE(yypos, yypos + 5));
<INITIAL>"rec" => (Tokens.REC(yypos, yypos + 3));
<INITIAL>"sharing" => (Tokens.SHARING(yypos, yypos + 7));
<INITIAL>"sig" => (Tokens.SIG(yypos, yypos + 3));
<INITIAL>"signature" => (Tokens.SIGNATURE(yypos, yypos + 9));
<INITIAL>"struct" => (Tokens.STRUCT(yypos, yypos + 6));
<INITIAL>"structure" => (Tokens.STRUCTURE(yypos, yypos + 9));
<INITIAL>"then" => (Tokens.THEN(yypos, yypos + 4));
<INITIAL>"type" => (Tokens.TYPE(yypos, yypos + 4));
<INITIAL>"val" => (Tokens.VAL(yypos, yypos + 3));
<INITIAL>"where" => (Tokens.WHERE(yypos, yypos + 5));
<INITIAL>"while" => (Tokens.WHILE(yypos, yypos + 5));
<INITIAL>"with" => (Tokens.WITH(yypos, yypos + 4));
<INITIAL>"withtype" => (Tokens.WITHTYPE(yypos, yypos + 8));
<INITIAL>"orelse" => (Tokens.ORELSE(yypos, yypos + 6));
<INITIAL>"andalso" => (Tokens.ANDALSO(yypos, yypos + 7));
<INITIAL>"'"{alphanum} => (Tokens.TYVAR(yytext, yypos,
					yypos + String.size yytext));
<INITIAL>{longid} => (case yytext of
			 "*" => Tokens.ASTERISK(yypos, yypos + 1)
		       | _ => Tokens.LONGID(yytext, yypos,
					    yypos + String.size yytext));
<INITIAL>{real}	=> (Tokens.REAL(yytext, yypos, yypos + String.size yytext));
<INITIAL>{num}	=> (Tokens.INT(yytext, yypos, yypos + String.size yytext));
<INITIAL>~{num}	=> (Tokens.INT(yytext, yypos, yypos + String.size yytext));
<INITIAL>"0x"{hexnum} => (Tokens.INT(yytext, yypos, yypos + String.size yytext));
<INITIAL>"~0x"{hexnum} => (Tokens.INT(yytext, yypos, yypos + String.size yytext));
<INITIAL>"0w"{num} => (makeWord(StringCvt.DEC, yytext, yypos,
				yypos + String.size yytext));
<INITIAL>"0wx"{hexnum} => (makeWord(StringCvt.HEX, yytext,
				    yypos, yypos + String.size yytext));
<INITIAL>\"	=> (charlist := [""]; stringstart := yypos;
                    stringtype := true; YYBEGIN S; continue());
<INITIAL>\#\"	=> (charlist := [""]; stringstart := yypos;
                    stringtype := false; YYBEGIN S; continue());
<INITIAL>"(*"	=> (YYBEGIN A; stringstart := yypos; comLevel := 1; continue());
<INITIAL>"*)"	=> (error(yypos, yypos + 2, "unmatched close comment") ;
		    continue());
<INITIAL>.	=> (error(yypos, yypos + 1, "illegal token") ;
		    continue());

<A>"(*"		=> (inc comLevel; continue());
<A>\n		=> (Source.newline (source, yypos) ; continue());
<A>"*)" => (dec comLevel; if !comLevel=0 then YYBEGIN INITIAL else (); continue());
<A>.		=> (continue());

<S>\"	        => (let val s = (concat(rev(!charlist)) before charlist := nil)
                        fun make(tok, v) = tok(v, !stringstart, yypos + 1)
                    in YYBEGIN INITIAL;
		       if !stringtype
			  then make(Tokens.STRING, s)
		       else
			  make(Tokens.CHAR,
			       if String.size s <> 1 
				  then (error(yypos, yypos + 1,
					      "character constant not length 1") ;
					#"\000")
			       else String.sub(s, 0))
                    end);
<S>\\a		=> (addChar #"\a"; continue());
<S>\\b		=> (addChar #"\b"; continue());
<S>\\f		=> (addChar #"\f"; continue());
<S>\\n		=> (addChar #"\n"; continue());
<S>\\r		=> (addChar #"\r"; continue());
<S>\\t		=> (addChar #"\t"; continue());
<S>\\v		=> (addChar #"\v"; continue());
<S>\\\^[@-_]	=> (addChar (Char.chr(Char.ord(String.sub(yytext, 2))
				      -Char.ord #"@"));
		    continue());
<S>\\\^.	=>
	(error(yypos, yypos + 2,
	       "illegal control escape; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_");
	continue());
<S>\\[0-9]{3}	=> (let
		       val x =
			  Char.ord(String.sub(yytext, 1)) * 100
			  + Char.ord(String.sub(yytext, 2)) * 10
			  + Char.ord(String.sub(yytext, 3))
			  - (Char.ord #"0") *111
		    in (if x > 255
			   then error(!stringstart, yypos,
				      "illegal ascii escape")
			else addChar(Char.chr x);
			   continue())
		    end);
<S>\\u{hexDigit}{4} => (let
			   val x = 
			      StringCvt.scanString
			      (Pervasive.Int.scan StringCvt.HEX)
			      (String.substring (yytext, 2, 4))
			   fun err() =
			      error(!stringstart, yypos, "illegal unicode escape")
			in (case x of
			       SOME x => if x > 255
					    then err()
					 else addChar(Char.chr x)
			     | _ => err())
			   ; continue()
			end);
<S>\\\"		=> (addString "\""; continue());
<S>\\\\		=> (addString "\\"; continue());
<S>\\{ws}   	=> (YYBEGIN F; continue());
<S>\\{eol}      => (Source.newline (source, yypos) ; YYBEGIN F ; continue());   
<S>\\		=> (error(!stringstart, yypos, "illegal string escape") ;
		    continue());
<S>{eol}	=> (Source.newline (source, yypos) ;
		    error(!stringstart, yypos, "unclosed string");
		    continue());
<S>" "|[\033-\126]  => (addString yytext; continue());
<S>. =>  (error(yypos, yypos + 1, "illegal character in string");
	  continue());

<F>{eol}        => (Source.newline (source, yypos) ; continue());
<F>{ws}		=> (continue());
<F>\\		=> (YYBEGIN S; stringstart := yypos; continue());
<F>.		=> (error(!stringstart, yypos, "unclosed string");
		    continue());
   
