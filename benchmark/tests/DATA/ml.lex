(* Copyright 1989 by AT&T Bell Laboratories *)
open ErrorMsg
type svalue = Tokens.svalue
type pos = int
type lexresult = (svalue,pos) Tokens.token
type lexarg = {comLevel : int ref, 
	       lineNum : int ref,
	       linePos : int list ref, (* offsets of lines in file *)
		charlist : string list ref,
		stringstart : int ref, (* start of current string or comment*)
	       err : pos*pos -> ErrorMsg.severity -> string->unit}
type arg = lexarg
type ('a,'b) token = ('a,'b) Tokens.token
val eof = fn ({comLevel,err,linePos,stringstart,lineNum,charlist}:lexarg) => 
	   let val pos = Integer.max(!stringstart+2, hd(!linePos))
	    in if !comLevel>0 then err (!stringstart,pos) COMPLAIN
					 "unclosed comment" 
		  	      else ();
	       Tokens.EOF(pos,pos)
	   end	
fun addString (charlist,s:string) = charlist := s :: (!charlist)
fun makeString charlist = (implode(rev(!charlist)) before charlist := nil)
fun makeHexInt sign s = let
      fun digit d = if (d < Ascii.uc_a) then (d - Ascii.zero)
	    else (10 + (if (d < Ascii.lc_a) then (d - Ascii.uc_a) else (d - Ascii.lc_a)))
      in
	revfold (fn (c,a) => sign(a*16, digit(ord c))) (explode s) 0
      end
fun makeInt sign s =
    revfold (fn (c,a) => sign(a*10, ord c - Ascii.zero)) (explode s) 0
%% 
%s A S F;
%header (functor MLLexFun(structure Tokens : ML_TOKENS));
%arg ({comLevel,lineNum,err,linePos,charlist,stringstart});
idchars=[A-Za-z'_0-9];
id=[A-Za-z'_]{idchars}*;
ws=("\012"|[\t\ ])*;
sym=[!%&$+/:<=>?@~|#*`]|\\|\-|\^;
num=[0-9]+;
frac="."{num};
exp="E"(~?){num};
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
hexnum=[0-9a-fA-F]+;
%%
<INITIAL>{ws}	=> (continue());
<INITIAL>\n	=> (inc lineNum; linePos := yypos :: !linePos; continue());
<INITIAL>"*"	=> (Tokens.ASTERISK(yypos,yypos+1));
<INITIAL>"|"	=> (Tokens.BAR(yypos,yypos+1));
<INITIAL>":"	=> (Tokens.COLON(yypos,yypos+1));
<INITIAL>"="	=> (Tokens.EQUAL(yypos,yypos+1));
<INITIAL>"_"	=> (Tokens.WILD(yypos,yypos+1));
<INITIAL>"#"	=> (Tokens.HASH(yypos,yypos+1));
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"{"	=> (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"	=> (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"["	=> (Tokens.LBRACKET(yypos,yypos+1));
<INITIAL>"]"	=> (Tokens.RBRACKET(yypos,yypos+1));
<INITIAL>";"	=> (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("	=> (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"	=> (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"and"	=> (Tokens.AND(yypos,yypos+3));
<INITIAL>"abstraction"	=> (Tokens.ABSTRACTION(yypos,yypos+11));
<INITIAL>"abstype"	=> (Tokens.ABSTYPE(yypos,yypos+7));
<INITIAL>"->"		=> (Tokens.ARROW(yypos,yypos+2));
<INITIAL>"as"		=> (Tokens.AS(yypos,yypos+2));
<INITIAL>"case"		=> (Tokens.CASE(yypos,yypos+4));
<INITIAL>"datatype"	=> (Tokens.DATATYPE(yypos,yypos+8));
<INITIAL>"."		=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>"..."		=> (Tokens.DOTDOTDOT(yypos,yypos+3));
<INITIAL>"else"		=> (Tokens.ELSE(yypos,yypos+4));
<INITIAL>"end"		=> (Tokens.END(yypos,yypos+3));
<INITIAL>"eqtype"	=> (Tokens.EQTYPE(yypos,yypos+6));
<INITIAL>"exception"	=> (Tokens.EXCEPTION(yypos,yypos+9));
<INITIAL>"do"		=> (Tokens.DO(yypos,yypos+2));
<INITIAL>"=>"		=> (Tokens.DARROW(yypos,yypos+2));
<INITIAL>"fn"		=> (Tokens.FN(yypos,yypos+2));
<INITIAL>"fun"		=> (Tokens.FUN(yypos,yypos+3));
<INITIAL>"functor"	=> (Tokens.FUNCTOR(yypos,yypos+7));
<INITIAL>"handle"	=> (Tokens.HANDLE(yypos,yypos+6));
<INITIAL>"if"		=> (Tokens.IF(yypos,yypos+2));
<INITIAL>"in"		=> (Tokens.IN(yypos,yypos+2));
<INITIAL>"include"	=> (Tokens.INCLUDE(yypos,yypos+7));
<INITIAL>"infix"	=> (Tokens.INFIX(yypos,yypos+5));
<INITIAL>"infixr"	=> (Tokens.INFIXR(yypos,yypos+6));
<INITIAL>"let"		=> (Tokens.LET(yypos,yypos+3));
<INITIAL>"local"	=> (Tokens.LOCAL(yypos,yypos+5));
<INITIAL>"nonfix"	=> (Tokens.NONFIX(yypos,yypos+6));
<INITIAL>"of"		=> (Tokens.OF(yypos,yypos+2));
<INITIAL>"op"		=> (Tokens.OP(yypos,yypos+2));
<INITIAL>"open"		=> (Tokens.OPEN(yypos,yypos+4));
<INITIAL>"overload"	=> (Tokens.OVERLOAD(yypos,yypos+8));
<INITIAL>"raise"	=> (Tokens.RAISE(yypos,yypos+5));
<INITIAL>"rec"		=> (Tokens.REC(yypos,yypos+3));
<INITIAL>"sharing"	=> (Tokens.SHARING(yypos,yypos+7));
<INITIAL>"sig"		=> (Tokens.SIG(yypos,yypos+3));
<INITIAL>"signature"	=> (Tokens.SIGNATURE(yypos,yypos+9));
<INITIAL>"struct"	=> (Tokens.STRUCT(yypos,yypos+6));
<INITIAL>"structure"	=> (Tokens.STRUCTURE(yypos,yypos+9));
<INITIAL>"then"		=> (Tokens.THEN(yypos,yypos+4));
<INITIAL>"type"		=> (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"val"		=> (Tokens.VAL(yypos,yypos+3));
<INITIAL>"while"	=> (Tokens.WHILE(yypos,yypos+5));
<INITIAL>"with"		=> (Tokens.WITH(yypos,yypos+4));
<INITIAL>"withtype"	=> (Tokens.WITHTYPE(yypos,yypos+8));
<INITIAL>"orelse"	=> (Tokens.ORELSE(yypos,yypos+6));
<INITIAL>"andalso"	=> (Tokens.ANDALSO(yypos,yypos+7));
<INITIAL>"import"	=> (Tokens.IMPORT(yypos,yypos+6));
<INITIAL>"'"{idchars}*	=> (Tokens.TYVAR(yytext, yypos, yypos+size yytext));
<INITIAL>({sym}+|{id})	=> (Tokens.ID(yytext, yypos, yypos+size yytext));
<INITIAL>{real}	=> (Tokens.REAL(yytext,yypos,yypos+size yytext));
<INITIAL>[1-9][0-9]* => (Tokens.INT(makeInt (op +) yytext
		    handle Overflow => (err (yypos,yypos+size yytext)
					  COMPLAIN "integer too large"; 1),
			yypos,yypos+size yytext));
<INITIAL>{num}	=> (Tokens.INT0(makeInt (op +) yytext
		    handle Overflow => (err (yypos,yypos+size yytext)
					  COMPLAIN "integer too large"; 0),
			yypos,yypos+size yytext));
<INITIAL>~{num}	=> (Tokens.INT0(makeInt (op -)
					(substring(yytext,1,size(yytext)-1))
		    handle Overflow => (err (yypos,yypos+size yytext)
					 COMPLAIN "integer too large"; 0),
			yypos,yypos+size yytext));
<INITIAL>"0x"{hexnum} => (
		    Tokens.INT0(makeHexInt (op +) (substring(yytext, 2, size(yytext)-2))
		        handle Overflow => (err (yypos,yypos+size yytext)
					    COMPLAIN "integer too large"; 0),
		      yypos, yypos+size yytext));
<INITIAL>"~0x"{hexnum} => (
		    Tokens.INT0(makeHexInt (op -) (substring(yytext, 3, size(yytext)-3))
		        handle Overflow => (err (yypos,yypos+size yytext)
					    COMPLAIN "integer too large"; 0),
		      yypos, yypos+size yytext));
<INITIAL>\"	=> (charlist := [""]; stringstart := yypos;
			YYBEGIN S; continue());
<INITIAL>"(*"	=> (YYBEGIN A; stringstart := yypos; comLevel := 1; continue());
<INITIAL>\h	=> (err (yypos,yypos) COMPLAIN "non-Ascii character"; continue());
<INITIAL>.	=> (err (yypos,yypos) COMPLAIN "illegal token"; continue());
<A>"(*"		=> (inc comLevel; continue());
<A>\n		=> (inc lineNum; linePos := yypos :: !linePos; continue());
<A>"*)" => (dec comLevel; if !comLevel=0 then YYBEGIN INITIAL else (); continue());
<A>.		=> (continue());
<S>\"	        => (YYBEGIN INITIAL; Tokens.STRING(makeString charlist,
				!stringstart,yypos+1));
<S>\n		=> (err (!stringstart,yypos) COMPLAIN "unclosed string";
		    inc lineNum; linePos := yypos :: !linePos;
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos));
<S>[^"\\\n]*	=> (addString(charlist,yytext); continue());
<S>\\\n	       	=> (inc lineNum; linePos := yypos :: !linePos;
		    YYBEGIN F; continue());
<S>\\[\ \t]   	=> (YYBEGIN F; continue());
<F>\n		=> (inc lineNum; linePos := yypos :: !linePos; continue());
<F>{ws}		=> (continue());
<F>\\		=> (YYBEGIN S; stringstart := yypos; continue());
<F>.		=> (err (!stringstart,yypos) COMPLAIN "unclosed string"; 
		    YYBEGIN INITIAL; Tokens.STRING(makeString charlist,!stringstart,yypos+1));
<S>\\t		=> (addString(charlist,"\t"); continue());
<S>\\n		=> (addString(charlist,"\n"); continue());
<S>\\\\		=> (addString(charlist,"\\"); continue());
<S>\\\"		=> (addString(charlist,chr(Ascii.dquote)); continue());
<S>\\\^[@-_]	=> (addString(charlist,chr(ordof(yytext,2)-ord("@"))); continue());
<S>\\[0-9]{3}	=>
 (let val x = ordof(yytext,1)*100
	     +ordof(yytext,2)*10
	     +ordof(yytext,3)
	     -(Ascii.zero*111)
  in (if x>255
      then err (yypos,yypos+4) COMPLAIN "illegal ascii escape"
      else addString(charlist,chr x);
      continue())
  end);
<S>\\		=> (err (yypos,yypos+1) COMPLAIN "illegal string escape"; 
		    continue());
