(* Heavily modified from SML/NJ sources. *)

(* ml.lex
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * SML/NJ is released under a BSD-style license.
 * See the file NJ-LICENSE for details.
 *)

(* Copyright (C) 2009,2016-2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
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

local
   open Control.Elaborate
in
   val allowLineComments = fn () => current allowLineComments
   val allowExtendedNumConsts = fn () => current allowExtendedNumConsts
   val allowExtendedTextConsts = fn () => current allowExtendedTextConsts
end

fun tok (t, s, l, r) =
   let
      val l = Source.getPos (s, l)
      val r = Source.getPos (s, r)
   in
      t (l, r)
   end

fun tok' (t, x, s, l) = tok (fn (l, r) => t (x, l, r), s, l, l + size x)

fun error (source, left, right, msg) =
   Control.errorStr (Region.make {left = Source.getPos (source, left),
                                  right = Source.getPos (source, right)},
                     msg)


(* Windows line endings are two characters.  We want to record the position of
 * the last end-of-line character, for accurate column number reporting.  *)
fun newLinePos yypos yytext = yypos + size yytext - 1

(* Comments *)
local
   val commentLeft = ref SourcePos.bogus
   val commentStack: (unit -> unit) list ref = ref []
in
   fun commentError (right, msg) =
      Control.errorStr (Region.make {left = !commentLeft,
                                     right = right},
                        msg)
   val inComment = fn () => not (List.isEmpty (!commentStack))
   fun startComment (source, yypos, th) =
      let
         val _ =
            if inComment ()
               then ()
               else commentLeft := Source.getPos (source, yypos)
         val _ = List.push (commentStack, th)
      in
         ()
      end
   fun finishComment () =
      (List.pop commentStack) ()
end


(* Line Directives *)
local
   val lineDirCol: int ref = ref ~1
   val lineDirFile: File.t option ref = ref NONE
   val lineDirLine: int ref = ref ~1
in
   fun startLineDir (source, yypos, th) =
      let
         val _ = lineDirCol := ~1
         val _ = lineDirFile := NONE
         val _ = lineDirLine := ~1
      in
         startComment (source, yypos, th)
      end
   fun addLineDirLineCol (line, col) =
      let
         val _ = lineDirLine := line
         val _ = lineDirCol := col
      in
         ()
      end
   fun addLineDirFile file =
      let
         val _ = lineDirFile := SOME file
      in
         ()
      end
   fun finishLineDir (source, yypos) =
      let
         val col = !lineDirCol
         val file = !lineDirFile
         val line = !lineDirLine
         val _ = lineDirCol := ~1
         val _ = lineDirFile := NONE
         val _ = lineDirLine := ~1
      in
         Source.lineDirective (source, file,
                               {lineNum = line,
                                lineStart = yypos - col})
         ; finishComment ()
      end
end


(* Numeric Constants *)
local
fun doit (source, yypos, yytext, drop, {extended: string option}, mkTok) =
   let
      val left = yypos
      val right = yypos + size yytext
      val extended =
         if String.contains (yytext, #"_")
            then SOME (Option.fold
                       (extended, "'_' separators", fn (msg1, msg2) =>
                        msg1 ^ " and " ^ msg2))
            else extended
      val _ =
         case extended of
            NONE => ()
          | SOME msg =>
               if allowExtendedNumConsts ()
                  then ()
                  else error (source, left, right,
                              concat ["Extended numeric constants (using ", msg,
                                      ") disallowed, compile with -default-ann 'allowExtendedNumConsts true'"])
   in
      mkTok (String.keepAll (String.dropPrefix (yytext, drop), fn c => not (c = #"_")),
             {extended = Option.isSome extended},
             Source.getPos (source, left), Source.getPos (source, right))
   end
in
fun real (source, yypos, yytext) =
   doit (source, yypos, yytext, 0, {extended = NONE}, fn (digits, {extended: bool}, l, r) =>
         Tokens.REAL (digits, l, r))
fun int (source, yypos, yytext, drop, {extended: string option}, {negate: bool}, radix) =
   doit (source, yypos, yytext, drop, {extended = extended}, fn (digits, {extended: bool}, l, r) =>
         Tokens.INT ({digits = digits,
                      extended = extended,
                      negate = negate,
                      radix = radix},
                     l, r))
fun word (source, yypos, yytext, drop, {extended: string option}, radix) =
   doit (source, yypos, yytext, drop, {extended = extended}, fn (digits, {extended: bool}, l, r) =>
         Tokens.WORD ({digits = digits,
                       radix = radix},
                      l, r))
end


(* Text Constants *)
local
   val chars: IntInf.t list ref = ref []
   val inText = ref false
   val textLeft = ref SourcePos.bogus
   val textFinishFn: (IntInf.t vector * SourcePos.t * SourcePos.t -> unit -> lexresult) ref = ref (fn _ => raise Fail "textFinish")
in
   fun textError (right, msg) =
      Control.errorStr (Region.make {left = !textLeft,
                                     right = right},
                        msg)
   fun startText (tl, tf) =
      let
         val _ = chars := []
         val _ = inText := true
         val _ = textLeft := tl
         val _ = textFinishFn := tf
      in
         ()
      end
   fun finishText (textRight) =
      let
         val cs = Vector.fromListRev (!chars)
         val tf = (!textFinishFn) (cs, !textLeft, textRight)
         val _ = chars := []
         val _ = inText := false
         val _ = textLeft := SourcePos.bogus
         val _ = textFinishFn := (fn _ => raise Fail "textFinish")
      in
         tf ()
      end
   val inText = fn () => !inText
   fun addTextString (s: string) =
      chars := String.fold (s, !chars, fn (c, ac) => Int.toIntInf (Char.ord c) :: ac)
   fun addTextCharCode (i: IntInf.int) = List.push (chars, i)
end
fun addTextChar (c: char) = addTextString (String.fromChar c)
fun addTextNumEsc (source, yypos, yytext, drop, {extended: string option}, radix): unit =
   let
      val left = yypos
      val right = yypos + size yytext
      val _ =
         case extended of
            NONE => ()
          | SOME msg =>
               if allowExtendedTextConsts ()
                  then ()
                  else error (source, left, right,
                              concat ["Extended text constants (using ", msg,
                                      ") disallowed, compile with -default-ann 'allowExtendedTextConsts true'"])
   in
      case StringCvt.scanString (fn r => IntInf.scan (radix, r)) (String.dropPrefix (yytext, drop)) of
         NONE => error (source, left, right, "Illegal numeric escape in text constant")
       | SOME i => addTextCharCode i
   end
fun addTextUTF8 (source, yypos, yytext): unit =
   let
      val left = yypos
      val right = yypos + size yytext
   in
      if not (allowExtendedTextConsts ())
         then error (source, left, right,
                     "Extended text constants (using UTF-8 byte sequences) disallowed, compile with -default-ann 'allowExtendedTextConsts true'")
         else addTextString yytext
   end


(* EOF *)
val eof: lexarg -> lexresult =
   fn {source, ...} =>
   let
      val pos = Source.lineStart source
      val _ =
         if inComment ()
            then commentError (SourcePos.bogus, "Unclosed comment at end of file")
            else ()
      val _ =
         if inText ()
            then textError (SourcePos.bogus, "Unclosed text constant at end of file")
            else ()
   in
      Tokens.EOF (pos, pos)
   end


%% 
%full

%s TEXT TEXT_FMT  BLOCK_COMMENT LINE_COMMENT  LINE_DIR1 LINE_DIR2 LINE_DIR3 LINE_DIR4;

%header (functor MLLexFun (structure Tokens : ML_TOKENS));
%arg ({source});

ws=\t|"\011"|"\012"|" ";
cr="\013";
nl="\010";
eol=({cr}{nl}|{nl}|{cr});

alphanum=[A-Za-z0-9'_];
alphanumId=[A-Za-z]{alphanum}*;
tyvarId="'"{alphanum}*;
sym="!"|"%"|"&"|"$"|"#"|"+"|"-"|"/"|":"|"<"|"="|">"|"?"|"@"|"\\"|"~"|"`"|"^"|"|"|"*";
symId={sym}+;
id={alphanumId}|{symId};
longId={id}("."{id})*;

decDigit=[0-9];
decnum={decDigit}("_"*{decDigit})*;
hexDigit=[0-9a-fA-F];
hexnum={hexDigit}("_"*{hexDigit})*;
binDigit=[0-1];
binnum={binDigit}("_"*{binDigit})*;
frac="."{decnum};
exp=[eE](~?){decnum};
real=(~?)(({decnum}{frac}?{exp})|({decnum}{frac}{exp}?));

%%
<INITIAL>{ws}+  => (continue ());
<INITIAL>{eol}  => (Source.newline (source, newLinePos yypos yytext); continue ());


<INITIAL>"_address" => (tok (Tokens.ADDRESS, source, yypos, yypos + size yytext));
<INITIAL>"_build_const" => (tok (Tokens.BUILD_CONST, source, yypos, yypos + size yytext));
<INITIAL>"_command_line_const" => (tok (Tokens.COMMAND_LINE_CONST, source, yypos, yypos + size yytext));
<INITIAL>"_const" => (tok (Tokens.CONST, source, yypos, yypos + size yytext));
<INITIAL>"_export" => (tok (Tokens.EXPORT, source, yypos, yypos + size yytext));
<INITIAL>"_import" => (tok (Tokens.IMPORT, source, yypos, yypos + size yytext));
<INITIAL>"_overload" => (tok (Tokens.OVERLOAD, source, yypos, yypos + size yytext));
<INITIAL>"_prim" => (tok (Tokens.PRIM, source, yypos, yypos + size yytext));
<INITIAL>"_symbol" => (tok (Tokens.SYMBOL, source, yypos, yypos + size yytext));

<INITIAL>"#" => (tok (Tokens.HASH, source, yypos, yypos + size yytext));
<INITIAL>"(" => (tok (Tokens.LPAREN, source, yypos, yypos + size yytext));
<INITIAL>")" => (tok (Tokens.RPAREN, source, yypos, yypos + size yytext));
<INITIAL>"," => (tok (Tokens.COMMA, source, yypos, yypos + size yytext));
<INITIAL>"->" => (tok (Tokens.ARROW, source, yypos, yypos + size yytext));
<INITIAL>"..." => (tok (Tokens.DOTDOTDOT, source, yypos, yypos + size yytext));
<INITIAL>":" => (tok (Tokens.COLON, source, yypos, yypos + size yytext));
<INITIAL>":>" => (tok (Tokens.COLONGT, source, yypos, yypos + size yytext));
<INITIAL>";" => (tok (Tokens.SEMICOLON, source, yypos, yypos + size yytext));
<INITIAL>"=" => (tok (Tokens.EQUALOP, source, yypos, yypos + size yytext));
<INITIAL>"=>" => (tok (Tokens.DARROW, source, yypos, yypos + size yytext));
<INITIAL>"[" => (tok (Tokens.LBRACKET, source, yypos, yypos + size yytext));
<INITIAL>"]" => (tok (Tokens.RBRACKET, source, yypos, yypos + size yytext));
<INITIAL>"_" => (tok (Tokens.WILD, source, yypos, yypos + size yytext));
<INITIAL>"{" => (tok (Tokens.LBRACE, source, yypos, yypos + size yytext));
<INITIAL>"|" => (tok (Tokens.BAR, source, yypos, yypos + size yytext));
<INITIAL>"}" => (tok (Tokens.RBRACE, source, yypos, yypos + size yytext));

<INITIAL>"abstype" => (tok (Tokens.ABSTYPE, source, yypos, yypos + size yytext));
<INITIAL>"and" => (tok (Tokens.AND, source, yypos, yypos + size yytext));
<INITIAL>"andalso" => (tok (Tokens.ANDALSO, source, yypos, yypos + size yytext));
<INITIAL>"as" => (tok (Tokens.AS, source, yypos, yypos + size yytext));
<INITIAL>"case" => (tok (Tokens.CASE, source, yypos, yypos + size yytext));
<INITIAL>"datatype" => (tok (Tokens.DATATYPE, source, yypos, yypos + size yytext));
<INITIAL>"do" => (tok (Tokens.DO, source, yypos, yypos + size yytext));
<INITIAL>"else" => (tok (Tokens.ELSE, source, yypos, yypos + size yytext));
<INITIAL>"end" => (tok (Tokens.END, source, yypos, yypos + size yytext));
<INITIAL>"eqtype" => (tok (Tokens.EQTYPE, source, yypos, yypos + size yytext));
<INITIAL>"exception" => (tok (Tokens.EXCEPTION, source, yypos, yypos + size yytext));
<INITIAL>"fn" => (tok (Tokens.FN, source, yypos, yypos + size yytext));
<INITIAL>"fun" => (tok (Tokens.FUN, source, yypos, yypos + size yytext));
<INITIAL>"functor" => (tok (Tokens.FUNCTOR, source, yypos, yypos + size yytext));
<INITIAL>"handle" => (tok (Tokens.HANDLE, source, yypos, yypos + size yytext));
<INITIAL>"if" => (tok (Tokens.IF, source, yypos, yypos + size yytext));
<INITIAL>"in" => (tok (Tokens.IN, source, yypos, yypos + size yytext));
<INITIAL>"include" => (tok (Tokens.INCLUDE, source, yypos, yypos + size yytext));
<INITIAL>"infix" => (tok (Tokens.INFIX, source, yypos, yypos + size yytext));
<INITIAL>"infixr" => (tok (Tokens.INFIXR, source, yypos, yypos + size yytext));
<INITIAL>"let" => (tok (Tokens.LET, source, yypos, yypos + size yytext));
<INITIAL>"local" => (tok (Tokens.LOCAL, source, yypos, yypos + size yytext));
<INITIAL>"nonfix" => (tok (Tokens.NONFIX, source, yypos, yypos + size yytext));
<INITIAL>"of" => (tok (Tokens.OF, source, yypos, yypos + size yytext));
<INITIAL>"op" => (tok (Tokens.OP, source, yypos, yypos + size yytext));
<INITIAL>"open" => (tok (Tokens.OPEN, source, yypos, yypos + size yytext));
<INITIAL>"orelse" => (tok (Tokens.ORELSE, source, yypos, yypos + size yytext));
<INITIAL>"raise" => (tok (Tokens.RAISE, source, yypos, yypos + size yytext));
<INITIAL>"rec" => (tok (Tokens.REC, source, yypos, yypos + size yytext));
<INITIAL>"sharing" => (tok (Tokens.SHARING, source, yypos, yypos + size yytext));
<INITIAL>"sig" => (tok (Tokens.SIG, source, yypos, yypos + size yytext));
<INITIAL>"signature" => (tok (Tokens.SIGNATURE, source, yypos, yypos + size yytext));
<INITIAL>"struct" => (tok (Tokens.STRUCT, source, yypos, yypos + size yytext));
<INITIAL>"structure" => (tok (Tokens.STRUCTURE, source, yypos, yypos + size yytext));
<INITIAL>"then" => (tok (Tokens.THEN, source, yypos, yypos + size yytext));
<INITIAL>"type" => (tok (Tokens.TYPE, source, yypos, yypos + size yytext));
<INITIAL>"val" => (tok (Tokens.VAL, source, yypos, yypos + size yytext));
<INITIAL>"where" => (tok (Tokens.WHERE, source, yypos, yypos + size yytext));
<INITIAL>"while" => (tok (Tokens.WHILE, source, yypos, yypos + size yytext));
<INITIAL>"with" => (tok (Tokens.WITH, source, yypos, yypos + size yytext));
<INITIAL>"withtype" => (tok (Tokens.WITHTYPE, source, yypos, yypos + size yytext));


<INITIAL>{tyvarId} => (tok' (Tokens.TYVAR, yytext, source, yypos));
<INITIAL>{longId} =>
   (case yytext of
       "*" => tok (Tokens.ASTERISK, source, yypos, yypos + size yytext)
     | _ => tok' (Tokens.LONGID, yytext, source, yypos));


<INITIAL>{real} =>
   (real (source, yypos, yytext));
<INITIAL>{decnum} =>
   (int (source, yypos, yytext, 0, {extended = NONE}, {negate = false}, StringCvt.DEC));
<INITIAL>"~"{decnum} =>
   (int (source, yypos, yytext, 1, {extended = NONE}, {negate = true}, StringCvt.DEC));
<INITIAL>"0x"{hexnum} =>
   (int (source, yypos, yytext, 2, {extended = NONE}, {negate = false}, StringCvt.HEX));
<INITIAL>"~0x"{hexnum} =>
   (int (source, yypos, yytext, 3, {extended = NONE}, {negate = true}, StringCvt.HEX));
<INITIAL>"0b"{binnum} =>
   (int (source, yypos, yytext, 2, {extended = SOME "binary notation"}, {negate = false}, StringCvt.BIN));
<INITIAL>"~0b"{binnum} =>
   (int (source, yypos, yytext, 3, {extended = SOME "binary notation"}, {negate = true}, StringCvt.BIN));
<INITIAL>"0w"{decnum} =>
   (word (source, yypos, yytext, 2, {extended = NONE}, StringCvt.DEC));
<INITIAL>"0wx"{hexnum} =>
   (word (source, yypos, yytext, 3, {extended = NONE}, StringCvt.HEX));
<INITIAL>"0wb"{binnum} =>
   (word (source, yypos, yytext, 3, {extended = SOME "binary notation"}, StringCvt.BIN));

<INITIAL>"\"" =>
   (startText (Source.getPos (source, yypos), fn (cs, l, r) => fn () =>
               (YYBEGIN INITIAL;
                Tokens.STRING (cs, l, r)))
    ; YYBEGIN TEXT
    ; continue ());
<INITIAL>"#\"" =>
   (startText (Source.getPos (source, yypos), fn (cs, l, r) =>
               (let
                   fun err () =
                      textError (r, "character constant not of size 1")
                   val c =
                      case Int.compare (Vector.length cs, 1) of
                         LESS => (err (); 0)
                       | EQUAL => Vector.sub (cs, 0)
                       | GREATER => (err (); Vector.sub (cs, 0))
                in
                   fn () =>
                   (YYBEGIN INITIAL;
                    Tokens.CHAR (c, l, r))
                end))
    ; YYBEGIN TEXT
    ; continue ());

<TEXT>"\""       => (finishText (Source.getPos (source, yypos + 1)));
<TEXT>" "|[\033-\126] =>
                    (addTextString yytext; continue ());
<TEXT>[\192-\223][\128-\191] =>
                    (addTextUTF8 (source, yypos, yytext); continue());
<TEXT>[\224-\239][\128-\191][\128-\191] =>
                    (addTextUTF8 (source, yypos, yytext); continue());
<TEXT>[\240-\247][\128-\191][\128-\191][\128-\191] =>
                    (addTextUTF8 (source, yypos, yytext); continue());
<TEXT>\\a        => (addTextChar #"\a"; continue ());
<TEXT>\\b        => (addTextChar #"\b"; continue ());
<TEXT>\\t        => (addTextChar #"\t"; continue ());
<TEXT>\\n        => (addTextChar #"\n"; continue ());
<TEXT>\\v        => (addTextChar #"\v"; continue ());
<TEXT>\\f        => (addTextChar #"\f"; continue ());
<TEXT>\\r        => (addTextChar #"\r"; continue ());
<TEXT>\\\^[@-_]  => (addTextChar (Char.chr(Char.ord(String.sub(yytext, 2)) - Char.ord #"@"));
                     continue ());
<TEXT>\\\^.      => (error (source, yypos, yypos + 2,
                            "Illegal control escape in text constant; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_");
                     continue ());
<TEXT>\\[0-9]{3} => (addTextNumEsc (source, yypos, yytext, 1,
                                    {extended = NONE}, StringCvt.DEC)
                     ; continue ());
<TEXT>\\u{hexDigit}{4} =>
                    (addTextNumEsc (source, yypos, yytext, 2,
                                    {extended = NONE}, StringCvt.HEX)
                     ; continue ());
<TEXT>\\U{hexDigit}{8} =>
                    (addTextNumEsc (source, yypos, yytext, 2,
                                    {extended = SOME "\\Uxxxxxxxx numeric escapes"},
                                    StringCvt.HEX)
                     ; continue ());
<TEXT>"\\\""     => (addTextString "\""; continue ());
<TEXT>\\\\       => (addTextString "\\"; continue ());
<TEXT>\\{ws}+    => (YYBEGIN TEXT_FMT; continue ());
<TEXT>\\{eol}    => (Source.newline (source, newLinePos (yypos + 1) yytext); YYBEGIN TEXT_FMT; continue ());
<TEXT>\\         => (error (source, yypos, yypos, "Illegal escape in text constant")
                     ; continue ());
<TEXT>{eol}      => (Source.newline (source, newLinePos yypos yytext)
                     ; textError (Source.getPos (source, yypos), "Unclosed text constant at end of line")
                     ; continue ());
<TEXT>.          => (error (source, yypos, yypos, "Illegal character in text constant")
                     ; continue ());

<TEXT_FMT>{ws}+  => (continue ());
<TEXT_FMT>{eol}  => (Source.newline (source, newLinePos yypos yytext); continue ());
<TEXT_FMT>\\     => (YYBEGIN TEXT; continue ());
<TEXT_FMT>.      => (error (source, yypos, yypos, "Illegal formatting character in text continuation")
                     ; continue ());


<INITIAL>"(*)" =>
   (if allowLineComments ()
       then ()
       else error (source, yypos, yypos,
                   "Line comments disallowed, compile with -default-ann 'allowLineConsts true'")
    ; startComment (source, yypos, fn () =>
                    YYBEGIN INITIAL)
    ; YYBEGIN LINE_COMMENT
    ; continue ());
<INITIAL>"(*" =>
   (startComment (source, yypos, fn () =>
                  YYBEGIN INITIAL)
    ; YYBEGIN BLOCK_COMMENT
    ; continue ());

<LINE_COMMENT>{eol} =>
   (Source.newline (source, newLinePos yypos yytext)
    ; finishComment ()
    ; continue ());
<LINE_COMMENT>. =>
   (continue ());

<BLOCK_COMMENT>"(*)" =>
   (if allowLineComments ()
       then ()
       else error (source, yypos, yypos,
                   "Line comments disallowed, compile with -default-ann 'allowLineConsts true'")
    ; startComment (source, yypos, fn () =>
                    YYBEGIN BLOCK_COMMENT)
    ; YYBEGIN LINE_COMMENT
    ; continue ());
<BLOCK_COMMENT>"(*" =>
   (startComment (source, yypos, fn () =>
                  YYBEGIN BLOCK_COMMENT)
    ; YYBEGIN BLOCK_COMMENT
    ; continue ());
<BLOCK_COMMENT>"*)" =>
   (finishComment ()
    ; continue ());
<BLOCK_COMMENT>{eol} =>
   (Source.newline (source, newLinePos yypos yytext)
    ; continue ());
<BLOCK_COMMENT>. =>
   (continue ());


<INITIAL>"(*#line"{ws}+ =>
   (startLineDir (source, yypos, fn () =>
                  YYBEGIN INITIAL)
    ; YYBEGIN LINE_DIR1
    ; continue ());

<LINE_DIR1>{decDigit}+"."{decDigit}+ =>
   (let
       fun err () =
          (commentError (Source.getPos (source, yypos), "Illegal line directive")
           ; YYBEGIN BLOCK_COMMENT)
     in
        case String.split (yytext, #".") of
           [line, col] =>
              (YYBEGIN LINE_DIR2
               ; addLineDirLineCol (valOf (Int.fromString line), valOf (Int.fromString col))
                 handle Overflow => err () | Option => err ()
               ; continue ())
         | _ => (err (); continue ())
     end);
<LINE_DIR2>{ws}+"\"" =>
   (YYBEGIN LINE_DIR3
    ; continue ());
<LINE_DIR3>[^"]*"\"" =>
   (addLineDirFile (String.dropLast yytext)
    ; YYBEGIN LINE_DIR4
    ; continue ());
<LINE_DIR2,LINE_DIR4>{ws}*"*)" =>
   (finishLineDir (source, yypos + size yytext)
    ; continue ());
<LINE_DIR1,LINE_DIR2,LINE_DIR3,LINE_DIR4>. =>
   (commentError (Source.getPos (source, yypos), "Illegal line directive")
    ; YYBEGIN BLOCK_COMMENT
    ; continue ());


<INITIAL>. =>
   (error (source, yypos, yypos + 1, "Illegal token")
    ; continue ());
