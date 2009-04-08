(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

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

fun lineDirective (source, file, yypos) =
   Source.lineDirective (source, file,
                         {lineNum = !lineNum,
                          lineStart = yypos - !colNum})
fun addString (s: string) = charlist := s :: (!charlist)
fun addChar (c: char) = addString (String.fromChar c)

fun inc (ri as ref (i: int)) = (ri := i + 1)
fun dec (ri as ref (i: int)) = (ri := i-1)

fun error (source, left, right, msg) = 
   Control.errorStr (Region.make {left = Source.getPos (source, left),
                                  right = Source.getPos (source, right)},
                     msg)

fun stringError (source, right, msg) =
   Control.errorStr (Region.make {left = !stringStart,
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
%s A S F L LL LLC LLCQ;
%header (functor MLBLexFun (structure Tokens : MLB_TOKENS));
%arg ({source});
alphanum=[A-Za-z'_0-9]*;
alphanumId=[A-Za-z]{alphanum};
id={alphanumId};

pathvar="$("([A-Z_][A-Z0-9_]*)")";
filename=({pathvar}|[A-Za-z0-9_.])({pathvar}|[-A-Za-z0-9_.])*;
arc=({pathvar}|{filename}|"."|"..");
relpath=({arc}"/")*;
abspath="/"{relpath};
path={relpath}|{abspath};
file={path}{filename};

ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
cr="\013";
nl="\010";
eol=({cr}{nl}|{nl}|{cr});

hexDigit=[0-9a-fA-F];

%%
<INITIAL>{ws}   => (continue ());
<INITIAL>{eol}  => (Source.newline (source, yypos); continue ());
<INITIAL>"_prim" 
                => (tok (Tokens.PRIM, source, yypos, yypos + 4));
<INITIAL>","    => (tok (Tokens.COMMA, source, yypos, yypos + 1));
<INITIAL>";"    => (tok (Tokens.SEMICOLON, source, yypos, yypos + 1));
<INITIAL>"="    => (tok (Tokens.EQUALOP, source, yypos, yypos + 1));
<INITIAL>"ann"  => (tok (Tokens.ANN, source, yypos, yypos + 3));
<INITIAL>"and"  => (tok (Tokens.AND, source, yypos, yypos + 3));
<INITIAL>"bas"  => (tok (Tokens.BAS, source, yypos, yypos + 3));
<INITIAL>"basis" 
                => (tok (Tokens.BASIS, source, yypos, yypos + 5));
<INITIAL>"end"  => (tok (Tokens.END, source, yypos, yypos + 3));
<INITIAL>"functor" 
                => (tok (Tokens.FUNCTOR, source, yypos, yypos + 7));
<INITIAL>"in"   => (tok (Tokens.IN, source, yypos, yypos + 2));
<INITIAL>"let"  => (tok (Tokens.LET, source, yypos, yypos + 3));
<INITIAL>"local" 
                => (tok (Tokens.LOCAL, source, yypos, yypos + 5));
<INITIAL>"open" => (tok (Tokens.OPEN, source, yypos, yypos + 4));
<INITIAL>"signature" 
                => (tok (Tokens.SIGNATURE, source, yypos, yypos + 9));
<INITIAL>"structure" 
                => (tok (Tokens.STRUCTURE, source, yypos, yypos + 9));
<INITIAL>{id}   => (tok' (Tokens.ID, yytext, source, yypos));
<INITIAL>{file} => (tok' (Tokens.FILE, yytext, source, yypos));

<INITIAL>\"     => (charlist := [""]
                    ; stringStart := Source.getPos (source, yypos)
                    ; YYBEGIN S
                    ; continue ());
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
<LL>.           => (YYBEGIN LLC; continue ()
                (* note hack, since ml-lex chokes on the empty string for 0* *));
<LLC>"*)"       => (YYBEGIN INITIAL
                    ; lineDirective (source, NONE, yypos + 2)
                    ; commentLevel := 0; charlist := []; continue ());
<LLC>{ws}\"     => (YYBEGIN LLCQ; continue ());
<LLCQ>[^\"]*    => (lineFile := yytext; continue ());
<LLCQ>\""*)"    => (YYBEGIN INITIAL
                    ; lineDirective (source, SOME (!lineFile), yypos + 3)
                    ; commentLevel := 0; charlist := []; continue ());
<L,LLC,LLCQ>"*)" 
                => (YYBEGIN INITIAL; commentLevel := 0; charlist := []; continue ());
<L,LLC,LLCQ>.   => (YYBEGIN A; continue ());

<A>"(*"         => (inc commentLevel; continue ());
<A>\n           => (Source.newline (source, yypos) ; continue ());
<A>"*)"         => (dec commentLevel
                    ; if 0 = !commentLevel then YYBEGIN INITIAL else ()
                    ; continue ());
<A>.            => (continue ());

<S>\"           => (let
                       val s = concat (rev (!charlist))
                       val _ = charlist := nil
                       fun make (t, v) =
                          t (v, !stringStart, Source.getPos (source, yypos + 1))
                    in YYBEGIN INITIAL
                       ; make (Tokens.STRING, s)
                    end);
<S>\\a          => (addChar #"\a"; continue ());
<S>\\b          => (addChar #"\b"; continue ());
<S>\\f          => (addChar #"\f"; continue ());
<S>\\n          => (addChar #"\n"; continue ());
<S>\\r          => (addChar #"\r"; continue ());
<S>\\t          => (addChar #"\t"; continue ());
<S>\\v          => (addChar #"\v"; continue ());
<S>\\\^[@-_]    => (addChar (Char.chr(Char.ord(String.sub(yytext, 2))
                                      -Char.ord #"@"))
                    ; continue ());
<S>\\\^.        => (error (source, yypos, yypos + 2,
                           "illegal control escape; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_")
                    ; continue ());
<S>\\[0-9]{3}   => (let
                       val x =
                          Char.ord(String.sub(yytext, 1)) * 100
                          + Char.ord(String.sub(yytext, 2)) * 10
                          + Char.ord(String.sub(yytext, 3))
                          - (Char.ord #"0") * 111
                    in (if x > 255
                           then stringError (source, yypos,
                                             "illegal ascii escape")
                        else addChar(Char.chr x);
                           continue ())
                    end);
<S>\\u{hexDigit}{4} 
                => (let
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
<S>\\\"         => (addString "\""; continue ());
<S>\\\\         => (addString "\\"; continue ());
<S>\\{nrws}     => (YYBEGIN F; continue ());
<S>\\{eol}      => (Source.newline (source, yypos) ; YYBEGIN F ; continue ());   
<S>\\           => (stringError (source, yypos, "illegal string escape")
                    ; continue ());
<S>{eol}        => (Source.newline (source, yypos)
                    ; stringError (source, yypos, "unclosed string")
                    ; continue ());
<S>" "|[\033-\126]  
                => (addString yytext; continue ());
<S>.            => (stringError (source, yypos + 1, "illegal character in string")
                    ; continue ());

<F>{eol}        => (Source.newline (source, yypos) ; continue ());
<F>{ws}         => (continue ());
<F>\\           => (YYBEGIN S
                    ; stringStart := Source.getPos (source, yypos)
                    ; continue ());
<F>.            => (stringError (source, yypos, "unclosed string")
                    ; continue ());
