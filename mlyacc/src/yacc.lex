(* Modified by sweeks@acm.org on 2000-8-24.
 * Ported to MLton.
 *)

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi

   yacc.lex: Lexer specification
 *)

structure Tokens = Tokens
type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

type lexarg = Hdr.inputSource
type arg = lexarg

open Tokens
val error = Hdr.error
val lineno = Hdr.lineno
val text = Hdr.text

val pcount: int ref = ref 0
val commentLevel: int ref = ref 0
val actionstart: int ref = ref 0

val eof = fn i => (if (!pcount)>0 then
			error i (!actionstart)
			      " eof encountered in action beginning here !"
		   else (); EOF(!lineno,!lineno))

val Add = fn s => (text := s::(!text))


local val dict = [("%prec",PREC_TAG),("%term",TERM),
	       ("%nonterm",NONTERM), ("%eop",PERCENT_EOP),("%start",START),
	       ("%prefer",PREFER),("%subst",SUBST),("%change",CHANGE),
	       ("%keyword",KEYWORD),("%name",NAME),
	       ("%verbose",VERBOSE), ("%nodefault",NODEFAULT),
	       ("%value",VALUE), ("%noshift",NOSHIFT),
	       ("%header",PERCENT_HEADER),("%pure",PERCENT_PURE),
	       ("%token_sig_info",PERCENT_TOKEN_SIG_INFO),
	       ("%arg",PERCENT_ARG),
	       ("%pos",PERCENT_POS)]
in val lookup =
     fn (s,left,right) =>
	 let fun f ((a,d)::b) = if a=s then d(left,right) else f b
	       | f nil = UNKNOWN(s,left,right)
	 in f dict
	 end
end

fun inc (ri as ref i : int ref) = (ri := i+1)
fun dec (ri as ref i : int ref) = (ri := i-1)

%%
%header (
functor LexMLYACC(structure Tokens : Mlyacc_TOKENS
		  structure Hdr : HEADER (* = Header *)
		    where type prec = Header.prec
		      and type inputSource = Header.inputSource) : ARG_LEXER
);
%arg (inputSource);
%s A CODE F COMMENT STRING EMPTYCOMMENT;
ws = [\t\ ]+;
idchars = [A-Za-z_'0-9];
id=[A-Za-z]{idchars}*;
tyvar="'"{idchars}*;
qualid ={id}".";
%%
<INITIAL>"(*"	=> (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
		    continue() before YYBEGIN INITIAL);
<A>"(*"		=> (YYBEGIN EMPTYCOMMENT; commentLevel := 1; continue());
<CODE>"(*"	=> (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
		    continue() before YYBEGIN CODE);
<INITIAL>[^%\n]+ => (Add yytext; continue());
<INITIAL>"%%"	 => (YYBEGIN A; HEADER (concat (rev (!text)),!lineno,!lineno));
<INITIAL,CODE,COMMENT,F,EMPTYCOMMENT>\n  => (Add yytext; inc lineno; continue());
<INITIAL>.	 => (Add yytext; continue());

<A>\n		=> (inc lineno; continue ());
<A>{ws}+	=> (continue());
<A>of		=> (OF(!lineno,!lineno));
<A>for		=> (FOR(!lineno,!lineno));
<A>"{"		=> (LBRACE(!lineno,!lineno));
<A>"}"		=> (RBRACE(!lineno,!lineno));
<A>","		=> (COMMA(!lineno,!lineno));
<A>"*"		=> (ASTERISK(!lineno,!lineno));
<A>"->"		=> (ARROW(!lineno,!lineno));
<A>"%left"	=> (PREC(Hdr.LEFT,!lineno,!lineno));
<A>"%right"	=> (PREC(Hdr.RIGHT,!lineno,!lineno));
<A>"%nonassoc" 	=> (PREC(Hdr.NONASSOC,!lineno,!lineno));
<A>"%"[a-z_]+	=> (lookup(yytext,!lineno,!lineno));
<A>{tyvar}	=> (TYVAR(yytext,!lineno,!lineno));
<A>{qualid}	=> (IDDOT(yytext,!lineno,!lineno));
<A>[0-9]+	=> (INT (yytext,!lineno,!lineno));
<A>"%%"		=> (DELIMITER(!lineno,!lineno));
<A>":"		=> (COLON(!lineno,!lineno));
<A>"|"		=> (BAR(!lineno,!lineno));
<A>{id}		=> (ID ((yytext,!lineno),!lineno,!lineno));
<A>"("		=> (pcount := 1; actionstart := (!lineno);
		    text := nil; YYBEGIN CODE; continue() before YYBEGIN A);
<A>.		=> (UNKNOWN(yytext,!lineno,!lineno));
<CODE>"("	=> (inc pcount; Add yytext; continue());
<CODE>")"	=> (dec pcount;
		    if !pcount = 0 then
			 PROG (concat (rev (!text)),!lineno,!lineno)
		    else (Add yytext; continue()));
<CODE>"\""	=> (Add yytext; YYBEGIN STRING; continue());
<CODE>[^()"\n]+	=> (Add yytext; continue());

<COMMENT>[(*)]	=> (Add yytext; continue());
<COMMENT>"*)"	=> (Add yytext; dec commentLevel;
		    if !commentLevel=0
			 then BOGUS_VALUE(!lineno,!lineno)
			 else continue()
		   );
<COMMENT>"(*"	=> (Add yytext; inc commentLevel; continue());
<COMMENT>[^*()\n]+ => (Add yytext; continue());

<EMPTYCOMMENT>[(*)]  => (continue());
<EMPTYCOMMENT>"*)"   => (dec commentLevel;
		          if !commentLevel=0 then YYBEGIN A else ();
			  continue ());
<EMPTYCOMMENT>"(*"   => (inc commentLevel; continue());
<EMPTYCOMMENT>[^*()\n]+ => (continue());

<STRING>"\""	=> (Add yytext; YYBEGIN CODE; continue());
<STRING>\\	=> (Add yytext; continue());
<STRING>\n	=> (Add yytext; error inputSource (!lineno) "unclosed string";
 	            inc lineno; YYBEGIN CODE; continue());
<STRING>[^"\\\n]+ => (Add yytext; continue());
<STRING>\\\"	=> (Add yytext; continue());
<STRING>\\[\ \t\n]   => (Add yytext;
			if substring(yytext,1,1)="\n" then inc lineno else ();
		     	YYBEGIN F; continue());

<F>{ws}		=> (Add yytext; continue());
<F>\\		=> (Add yytext; YYBEGIN STRING; continue());
<F>.		=> (Add yytext; error inputSource (!lineno) "unclosed string";
		    YYBEGIN CODE; continue());

