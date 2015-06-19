(* Heavily modified from the SML/NJ sources by sweeks@sweeks.com. *)

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

type svalue = Tokens.svalue
type pos = SourcePos.t
type lexresult = (svalue, pos) Tokens.token
type lexarg = {source: Source.t}
type arg = lexarg
type ('a,'b) token = ('a,'b) Tokens.token

val charlist: IntInf.t list ref = ref []
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

fun addString (s: string) =
   charlist :=
   String.fold (s, !charlist, fn (c, ac) => Int.toIntInf (Char.ord c) :: ac)

fun addChar (c: char) = addString (String.fromChar c)

fun inc (ri as ref (i: int)) = ri := i + 1

fun dec (ri as ref (i: int)) = ri := i - 1

fun error (source, left, right, msg) = 
   Control.errorStr (Region.make {left = Source.getPos (source, left),
                                  right = Source.getPos (source, right)},
                     msg)

fun stringError (source, right, msg) =
   Control.errorStr (Region.make {left = !stringStart,
                                  right = Source.getPos (source, right)},
                     msg)

local
   open Control.Elaborate
in
   val allowLineComments = fn () => current allowLineComments
   val allowExtendedLiterals = fn () => current allowExtendedLiterals
end

fun addOrd (i: IntInf.t): unit = List.push (charlist, i)

fun addHexEscape (s: string, source, yypos): unit =
   case StringCvt.scanString (Pervasive.IntInf.scan StringCvt.HEX) s of
      NONE => stringError (source, yypos, "illegal unicode escape")
    | SOME i => addOrd i

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

fun stripUscores (yytext) =
   String.keepAll (yytext, (fn c => if c = #"_" then false else true))

fun hasUscores (yytext) =
   String.exists (yytext, (fn c => if c = #"_" then true else false))

fun extLiteral (yytext, source, l, r) =
   if (not (allowExtendedLiterals ())) andalso (hasUscores yytext)
      then (error (source, l, r, "Extended literals disallowed, compile with -default-ann 'allowExtendedLiterals true'"))
      else ()

fun binLiteral (yytext, source, l, r) =
   if not (allowExtendedLiterals ())
      then (error (source, l, r, "Binary literals disallowed, compile with -default-ann 'allowExtendedLiterals true'"))
      else ()

fun int (yytext, drop, source, yypos, {negate: bool}, radix) =
   Tokens.INT ({digits = String.dropPrefix (stripUscores yytext, drop),
                negate = negate,
                radix = radix},
               Source.getPos (source, yypos),
               Source.getPos (source, yypos + size yytext))

fun word (yytext, drop, source, yypos, radix) =
   Tokens.WORD ({digits = String.dropPrefix (stripUscores yytext, drop),
                 radix = radix},
                Source.getPos (source, yypos),
                Source.getPos (source, yypos + size yytext))

%% 
%reject
%s A B C S F L LL LLC LLCQ;
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
num=([0-9]([0-9]|"_")*[0-9])|([0-9]+);
frac="."{num};
exp=[eE](~?){num};
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
hexDigit=[0-9a-fA-F];
hexnum=({hexDigit}({hexDigit}|"_")*{hexDigit})|({hexDigit}+);
binDigit=[0-1];
binnum=({binDigit}({binDigit}|"_")*{binDigit})|({binDigit}+);

%%
<INITIAL>{ws}   => (continue ());
<INITIAL>{eol}  => (Source.newline (source, yypos); continue ());
<INITIAL>"_address" =>
   (tok (Tokens.ADDRESS, source, yypos, yypos + size yytext));
<INITIAL>"_build_const" =>
   (tok (Tokens.BUILD_CONST, source, yypos, yypos + size yytext));
<INITIAL>"_command_line_const" =>
   (tok (Tokens.COMMAND_LINE_CONST, source, yypos, yypos + size yytext));
<INITIAL>"_const" =>
   (tok (Tokens.CONST, source, yypos, yypos + size yytext));
<INITIAL>"_export" =>
   (tok (Tokens.EXPORT, source, yypos, yypos + size yytext));
<INITIAL>"_import" =>
   (tok (Tokens.IMPORT, source, yypos, yypos + size yytext));
<INITIAL>"_overload" =>
   (tok (Tokens.OVERLOAD, source, yypos, yypos + size yytext));
<INITIAL>"_symbol" =>
   (tok (Tokens.SYMBOL, source, yypos, yypos + size yytext));
<INITIAL>"_prim" =>
   (tok (Tokens.PRIM, source, yypos, yypos + size yytext));
<INITIAL>"_"    => (tok (Tokens.WILD, source, yypos, yypos + 1));
<INITIAL>","    => (tok (Tokens.COMMA, source, yypos, yypos + 1));
<INITIAL>"{"    => (tok (Tokens.LBRACE, source, yypos, yypos + 1));
<INITIAL>"}"    => (tok (Tokens.RBRACE, source, yypos, yypos + 1));
<INITIAL>"["    => (tok (Tokens.LBRACKET, source, yypos, yypos + 1));
<INITIAL>"]"    => (tok (Tokens.RBRACKET, source, yypos, yypos + 1));
<INITIAL>";"    => (tok (Tokens.SEMICOLON, source, yypos, yypos + 1));
<INITIAL>"("    => (tok (Tokens.LPAREN, source, yypos, yypos + 1));
<INITIAL>")"    => (tok (Tokens.RPAREN, source, yypos, yypos + 1));
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
<INITIAL>{longid} =>
   (case yytext of
       "*" => tok (Tokens.ASTERISK, source, yypos, yypos + 1)
     | _ => tok' (Tokens.LONGID, yytext, source, yypos));
<INITIAL>{real} =>
   ((extLiteral (yytext, source, yypos, yypos + size yytext));
   (tok' (Tokens.REAL, stripUscores yytext, source, yypos)));
<INITIAL>{num} =>
   ((extLiteral (yytext, source, yypos, yypos + size yytext));
   (int (yytext, 0, source, yypos, {negate = false}, StringCvt.DEC)));
<INITIAL>"~"{num} =>
   ((extLiteral (yytext, source, yypos, yypos + 1 + size yytext));
   (int (yytext, 1, source, yypos, {negate = true}, StringCvt.DEC)));
<INITIAL>"0x"{hexnum} =>
   ((extLiteral (yytext, source, yypos, yypos + 2 + size yytext));
   (int (yytext, 2, source, yypos, {negate = false}, StringCvt.HEX)));
<INITIAL>"~0x"{hexnum} =>
   ((extLiteral (yytext, source, yypos, yypos + 3 + size yytext));
   (int (yytext, 3, source, yypos, {negate = true}, StringCvt.HEX)));
<INITIAL>"0b"{binnum} =>
   ((binLiteral (yytext, source, yypos, yypos + 2 + size yytext));
   (int (yytext, 2, source, yypos, {negate = false}, StringCvt.BIN)));
<INITIAL>"~0b"{binnum} =>
   ((binLiteral (yytext, source, yypos, yypos + 3 + size yytext));
   (int (yytext, 3, source, yypos, {negate = true}, StringCvt.BIN)));
<INITIAL>"0w"{num} =>
   ((extLiteral (yytext, source, yypos, yypos + 2 + size yytext));
   (word (yytext, 2, source, yypos, StringCvt.DEC)));
<INITIAL>("0wx"|"0xw"){hexnum} =>
   ((extLiteral (yytext, source, yypos, yypos + 3 + size yytext));
   (word (yytext, 3, source, yypos, StringCvt.HEX)));
<INITIAL>("0wb"|"0bw"){binnum} =>
   ((binLiteral (yytext, source, yypos, yypos + 3 + size yytext));
   (word (yytext, 3, source, yypos, StringCvt.BIN)));
<INITIAL>\"     => (charlist := []
                    ; stringStart := Source.getPos (source, yypos)
                    ; stringtype := true
                    ; YYBEGIN S
                    ; continue ());
<INITIAL>\#\"   => (charlist := []
                    ; stringStart := Source.getPos (source, yypos)
                    ; stringtype := false
                    ; YYBEGIN S
                    ; continue ());
<INITIAL>"(*)"	=> (if allowLineComments ()
                       then (YYBEGIN B
                            ; commentStart := Source.getPos (source, yypos)
                            ; continue ())
                       else (YYBEGIN A
                            ; commentLevel := 1
                            ; commentStart := Source.getPos (source, yypos)
                            ; continue ())
                    );
<INITIAL>"(*#line"{nrws}
                => (YYBEGIN L
                    ; commentStart := Source.getPos (source, yypos)
                    ; commentLevel := 1
                    ; continue ());
<INITIAL>"(*"   => (YYBEGIN A
                    ; commentLevel := 1
                    ; commentStart := Source.getPos (source, yypos)
                    ; continue ());
<INITIAL>.      => (error (source, yypos, yypos + 1, "illegal token") ;
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
<LLC>{ws}\"     => (YYBEGIN LLCQ; continue ());
<LLCQ>[^\"]*    => (lineFile := yytext; continue ());
<LLCQ>\""*)"    => (YYBEGIN INITIAL
                    ; lineDirective (source, SOME (!lineFile), yypos + 3)
                    ; commentLevel := 0; charlist := []; continue ());
<L,LLC,LLCQ>"*)" => (YYBEGIN INITIAL; commentLevel := 0; charlist := []; continue ());
<L,LLC,LLCQ>.   => (YYBEGIN A; continue ());

<A>"(*)"        => (if allowLineComments ()
                       then (YYBEGIN C
                            ; continue ())
                       else (inc commentLevel; continue ())
                   );
<A>"(*"         => (inc commentLevel; continue ());
<A>\n           => (Source.newline (source, yypos) ; continue ());
<A>"*)"         => (dec commentLevel
                    ; if 0 = !commentLevel then YYBEGIN INITIAL else ()
                    ; continue ());
<A>.            => (continue ());
<B>{eol}        => (YYBEGIN INITIAL
                    ; Source.newline (source, yypos) ; continue ());
<B>.            => (continue ());
<C>{eol}        => (YYBEGIN A; continue ());
<C>.            => (continue ());
<S>\"           => (let
                       val s = Vector.fromListRev (!charlist)
                       val _ = charlist := nil
                       fun make (t, v) =
                          t (v, !stringStart, Source.getPos (source, yypos + 1))
                       val () = YYBEGIN INITIAL
                    in
                       if !stringtype
                          then make (Tokens.STRING, s)
                       else
                          make (Tokens.CHAR,
                                if 1 <> Vector.length s
                                   then (error
                                         (source, yypos, yypos + 1,
                                          "character constant not length 1")
                                         ; 0)
                                else Vector.sub (s, 0))
                    end);
<S>\\a          => (addChar #"\a"; continue ());
<S>\\b          => (addChar #"\b"; continue ());
<S>\\f          => (addChar #"\f"; continue ());
<S>\\n          => (addChar #"\n"; continue ());
<S>\\r          => (addChar #"\r"; continue ());
<S>\\t          => (addChar #"\t"; continue ());
<S>\\v          => (addChar #"\v"; continue ());
<S>\\\^[@-_]    => (addChar (Char.chr(Char.ord(String.sub(yytext, 2))
                                      -Char.ord #"@"));
                    continue ());
<S>\\\^.        =>
        (error (source, yypos, yypos + 2,
                "illegal control escape; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_");
        continue ());
<S>\\[0-9]{3}   => (let
                       fun c (i, scale) =
                          scale * (Char.ord (String.sub (yytext, i))
                                   - Char.ord #"0")
                       val () = addOrd (IntInf.fromInt
                                        (c (1, 100) + c (2, 10) + c (3, 1)))
                    in
                       continue ()
                    end);
<S>\\u{hexDigit}{4} => (addHexEscape (String.substring (yytext, 2, 4),
                                      source, yypos)
                        ; continue ());
<S>\\U{hexDigit}{8} => (addHexEscape (String.substring (yytext, 2, 8),
                                      source, yypos)
                        ; continue ());
<S>\\\"         => (addString "\""; continue ());
<S>\\\\         => (addString "\\"; continue ());
<S>\\{nrws}     => (YYBEGIN F; continue ());
<S>\\{eol}      => (Source.newline (source, yypos + 1) ; YYBEGIN F ; continue ());
<S>\\           => (stringError (source, yypos, "illegal string escape")
                    ; continue ());
<S>{eol}        => (Source.newline (source, yypos)
                    ; stringError (source, yypos, "unclosed string")
                    ; continue ());
<S>" "|[\033-\126]  => (addString yytext; continue ());
<S>. =>  (stringError (source, yypos + 1, "illegal character in string")
          ; continue ());

<F>{eol}        => (Source.newline (source, yypos) ; continue ());
<F>{ws}         => (continue ());
<F>\\           => (YYBEGIN S
                    ; stringStart := Source.getPos (source, yypos)
                    ; continue ());
<F>.            => (stringError (source, yypos, "unclosed string")
                    ; continue ());

