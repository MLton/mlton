structure Tokens = Tokens
structure Interface = Interface
open Interface

type pos = Interface.pos
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val eof = fn () => Tokens.EOF(!line,!line)
fun makeInt (s : string) = s

%%
%header (functor FolLexFun(structure Tokens: Fol_TOKENS
			   structure Interface: INTERFACE) : LEXER);
lcstart=[a-z!&$+/<=>?@~|#*`]|\-;
ucstart=[A-Z_];
idchars={lcstart}|{ucstart}|[0-9];
lcid={lcstart}{idchars}*;
ucid={ucstart}{idchars}*;
ws=[\t\ ]*;
num=[0-9]+;
%%
<INITIAL>{ws}	=> (lex());
<INITIAL>\n	=> (next_line(); lex());
<INITIAL>":-"	=> (Tokens.BACKARROW(!line,!line));
<INITIAL>","	=> (Tokens.COMMA(!line,!line));
<INITIAL>";"	=> (Tokens.SEMICOLON(!line,!line));
<INITIAL>"."    => (Tokens.DOT(!line,!line));
<INITIAL>"("	=> (Tokens.LPAREN(!line,!line));
<INITIAL>")"	=> (Tokens.RPAREN(!line,!line));
<INITIAL>"->"	=> (Tokens.ARROW(!line,!line));
<INITIAL>"=>"	=> (Tokens.DOUBLEARROW(!line,!line));
<INITIAL>"|"	=> (Tokens.BAR(!line,!line));
<INITIAL>"true" => (Tokens.TRUE(!line,!line));
<INITIAL>"forall" => (Tokens.FORALL(!line,!line));
<INITIAL>"exists" => (Tokens.EXISTS(!line,!line));
<INITIAL>{lcid} => (Tokens.LCID (yytext,!line,!line));
<INITIAL>{ucid} => (Tokens.UCID (yytext,!line,!line));
<INITIAL>{num}	=> (Tokens.INT (makeInt yytext,!line,!line));
<INITIAL>.	=> (error ("ignoring illegal character" ^ yytext,
			   !line,!line); lex());
