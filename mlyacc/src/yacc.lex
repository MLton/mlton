(* Modified by mfluet@acm.org on 2005-8-01.
 * Update with SML/NJ 110.55+.
 *)
(* Modified by sweeks@acm.org on 2000-8-24.
 * Ported to MLton.
 *)

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi

   yacc.lex: Lexer specification
 *)

structure Tokens = Tokens
type svalue = Tokens.svalue
type pos = Header.pos
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

type lexarg = Hdr.inputSource
type arg = lexarg

open Tokens
val error = Hdr.error
val text = Hdr.text

val pcount: int ref = ref 0
val commentLevel: int ref = ref 0
val actionstart: pos ref = ref {line = 1, col = 0}

fun linePos () = {line = !(#line Hdr.pos), col = 0}
fun pos pos = {line = !(#line Hdr.pos), col = pos - !(#start Hdr.pos)}

val eof = fn i => (if (!pcount)>0 then
                        error i (!actionstart)
                              " eof encountered in action beginning here !"
                   else (); EOF(linePos (), linePos ()))

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
in
fun lookup (s,left,right) = let
       fun f ((a,d)::b) = if a=s then d(left,right) else f b
         | f nil = UNKNOWN(s,left,right)
       in
          f dict
       end
end

fun inc (ri as ref i : int ref) = (ri := i+1)
fun dec (ri as ref i : int ref) = (ri := i-1)

fun incLineNum pos = (inc (#line Hdr.pos) ; #start Hdr.pos := pos)

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
eol=("\n"|"\013\n"|"\013");
idchars = [A-Za-z_'0-9];
id=[A-Za-z]{idchars}*;
tyvar="'"{idchars}*;
qualid ={id}".";
%%
<INITIAL>"(*"   => (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
                    continue() before YYBEGIN INITIAL);
<A>"(*"         => (YYBEGIN EMPTYCOMMENT; commentLevel := 1; continue());
<CODE>"(*"      => (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
                    continue() before YYBEGIN CODE);
<INITIAL>[^%\013\n]+ => (Add yytext; continue());
<INITIAL>"%%"    => (YYBEGIN A; HEADER (concat (rev (!text)),pos yypos,pos yypos));
<INITIAL,CODE,COMMENT,F,EMPTYCOMMENT>{eol}  => (Add yytext; incLineNum yypos; continue());
<INITIAL>.       => (Add yytext; continue());

<A>{eol}        => (incLineNum yypos; continue ());
<A>{ws}+        => (continue());
<A>of           => (OF(pos yypos,pos yypos));
<A>for          => (FOR(pos yypos,pos yypos));
<A>"{"          => (LBRACE(pos yypos,pos yypos));
<A>"}"          => (RBRACE(pos yypos,pos yypos));
<A>","          => (COMMA(pos yypos,pos yypos));
<A>"*"          => (ASTERISK(pos yypos,pos yypos));
<A>"->"         => (ARROW(pos yypos,pos yypos));
<A>"%left"      => (PREC(Hdr.LEFT,pos yypos,pos yypos));
<A>"%right"     => (PREC(Hdr.RIGHT,pos yypos,pos yypos));
<A>"%nonassoc"  => (PREC(Hdr.NONASSOC,pos yypos,pos yypos));
<A>"%"[a-z_]+   => (lookup(yytext,pos yypos,pos yypos));
<A>{tyvar}      => (TYVAR(yytext,pos yypos,pos yypos));
<A>{qualid}     => (IDDOT(yytext,pos yypos,pos yypos));
<A>[0-9]+       => (INT (yytext,pos yypos,pos yypos));
<A>"%%"         => (DELIMITER(pos yypos,pos yypos));
<A>":"          => (COLON(pos yypos,pos yypos));
<A>"|"          => (BAR(pos yypos,pos yypos));
<A>{id}         => (ID ((yytext,pos yypos),pos yypos,pos yypos));
<A>"("          => (pcount := 1; actionstart := pos yypos;
                    text := nil; YYBEGIN CODE; continue() before YYBEGIN A);
<A>.            => (UNKNOWN(yytext,pos yypos,pos yypos));
<CODE>"("       => (inc pcount; Add yytext; continue());
<CODE>")"       => (dec pcount;
                    if !pcount = 0 then
                         PROG (concat (rev (!text)),!actionstart,pos yypos)
                    else (Add yytext; continue()));
<CODE>"\""      => (Add yytext; YYBEGIN STRING; continue());
<CODE>[^()"\n\013]+ => (Add yytext; continue());

<COMMENT>[(*)]  => (Add yytext; continue());
<COMMENT>"*)"   => (Add yytext; dec commentLevel;
                    if !commentLevel=0
                         then BOGUS_VALUE(pos yypos,pos yypos)
                         else continue()
                   );
<COMMENT>"(*"   => (Add yytext; inc commentLevel; continue());
<COMMENT>[^*()\n\013]+ => (Add yytext; continue());

<EMPTYCOMMENT>[(*)]  => (continue());
<EMPTYCOMMENT>"*)"   => (dec commentLevel;
                          if !commentLevel=0 then YYBEGIN A else ();
                          continue ());
<EMPTYCOMMENT>"(*"   => (inc commentLevel; continue());
<EMPTYCOMMENT>[^*()\n\013]+ => (continue());

<STRING>"\""    => (Add yytext; YYBEGIN CODE; continue());
<STRING>\\      => (Add yytext; continue());
<STRING>{eol}   => (Add yytext; error inputSource (pos yypos) "unclosed string";
                    incLineNum yypos; YYBEGIN CODE; continue());
<STRING>[^"\\\n\013]+ => (Add yytext; continue());
<STRING>\\\"    => (Add yytext; continue());
<STRING>\\{eol} => (Add yytext; incLineNum yypos; YYBEGIN F; continue());
<STRING>\\[\ \t] => (Add yytext; YYBEGIN F; continue());

<F>{ws}         => (Add yytext; continue());
<F>\\           => (Add yytext; YYBEGIN STRING; continue());
<F>.            => (Add yytext; error inputSource (pos yypos) "unclosed string";
                    YYBEGIN CODE; continue());

