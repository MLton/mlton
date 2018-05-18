(* Heavily modified from SML/NJ sources. *)

(* ml.lex
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * SML/NJ is released under a HPND-style license.
 * See the file NJ-LICENSE for details.
 *)

(* Copyright (C) 2009,2016-2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
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

fun lastPos (yypos, yytext) = yypos + size yytext - 1

fun tok (t, x, s, l) =
   let
      val left = Source.getPos (s, l)
      val right = Source.getPos (s, lastPos (l, x))
   in
      t (left, right)
   end

fun tok' (t, x, s, l) = tok (fn (l, r) => t (x, l, r), x, s, l)

fun error' (left, right, msg) =
   Control.errorStr (Region.make {left = left, right = right}, msg)
fun error (source, left, right, msg) =
   error' (Source.getPos (source, left), Source.getPos (source, right), msg)


(* Comments *)
local
   val commentErrors: string list ref = ref []
   val commentLeft = ref SourcePos.bogus
   val commentStack: (int -> unit) list ref = ref []
in
   fun addCommentError msg =
      List.push (commentErrors, msg)
   val inComment = fn () => not (List.isEmpty (!commentStack))
   fun startComment (source, yypos, th) =
      if inComment ()
         then List.push (commentStack, fn _ => th ())
         else (commentErrors := []
               ; commentLeft := Source.getPos (source, yypos)
               ; List.push (commentStack, fn yypos =>
                            (List.foreach (!commentErrors, fn msg =>
                                           error' (!commentLeft,
                                                   Source.getPos (source, yypos),
                                                   msg))
                             ; th ())))
   fun finishComment yypos =
      (List.pop commentStack) yypos
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
         finishComment yypos
         ; Source.lineDirective (source, file,
                                 {lineNum = line,
                                  lineStart = yypos + 1 - col})
      end
end


(* Numeric Constants *)
local
fun doit (source, yypos, yytext, drop, {extended: string option}, mkTok) =
   let
      val left = yypos
      val right = lastPos (yypos, yytext)
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
   val textFinishFn: (IntInf.t vector * SourcePos.t * SourcePos.t -> lexresult) ref = ref (fn _ => raise Fail "textFinish")
in
   fun startText (tl, tf) =
      let
         val _ = chars := []
         val _ = inText := true
         val _ = textLeft := tl
         val _ = textFinishFn := tf
      in
         ()
      end
   fun finishText textRight =
      let
         val cs = Vector.fromListRev (!chars)
         val tl = !textLeft
         val tr = textRight
         val tf = !textFinishFn
         val _ = chars := []
         val _ = inText := false
         val _ = textLeft := SourcePos.bogus
         val _ = textFinishFn := (fn _ => raise Fail "textFinish")
      in
         tf (cs, tl, tr)
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
      val right = lastPos (yypos, yytext)
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
      val right = lastPos (yypos, yytext)
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
      val _ = Source.newline (source, ~1)
      val pos = Source.getPos (source, ~1)
      val _ =
         if inComment ()
            then error' (pos, SourcePos.bogus, "Unclosed comment at end of file")
            else ()
      val _ =
         if inText ()
            then error' (pos, SourcePos.bogus, "Unclosed text constant at end of file")
            else ()
   in
      Tokens.EOF (pos, SourcePos.bogus)
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
sym="!"|"%"|"&"|"$"|"#"|"+"|"-"|"/"|":"|"<"|"="|">"|"?"|"@"|"\\"|"~"|"`"|"^"|"|"|"*";
symId={sym}+;

tyvarId="'"{alphanum}*;
longSymId=({alphanumId}".")+{symId};
longAlphanumId=({alphanumId}".")+{alphanumId};

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
<INITIAL>{eol}  => (Source.newline (source, lastPos (yypos, yytext)); continue ());


<INITIAL>"_address" => (tok (Tokens.ADDRESS, yytext, source, yypos));
<INITIAL>"_build_const" => (tok (Tokens.BUILD_CONST, yytext, source, yypos));
<INITIAL>"_command_line_const" => (tok (Tokens.COMMAND_LINE_CONST, yytext, source, yypos));
<INITIAL>"_const" => (tok (Tokens.CONST, yytext, source, yypos));
<INITIAL>"_export" => (tok (Tokens.EXPORT, yytext, source, yypos));
<INITIAL>"_import" => (tok (Tokens.IMPORT, yytext, source, yypos));
<INITIAL>"_overload" => (tok (Tokens.OVERLOAD, yytext, source, yypos));
<INITIAL>"_prim" => (tok (Tokens.PRIM, yytext, source, yypos));
<INITIAL>"_symbol" => (tok (Tokens.SYMBOL, yytext, source, yypos));

<INITIAL>"#" => (tok (Tokens.HASH, yytext, source, yypos));
<INITIAL>"#[" => (tok (Tokens.HASHLBRACKET, yytext, source, yypos));
<INITIAL>"(" => (tok (Tokens.LPAREN, yytext, source, yypos));
<INITIAL>")" => (tok (Tokens.RPAREN, yytext, source, yypos));
<INITIAL>"," => (tok (Tokens.COMMA, yytext, source, yypos));
<INITIAL>"->" => (tok (Tokens.ARROW, yytext, source, yypos));
<INITIAL>"..." => (tok (Tokens.DOTDOTDOT, yytext, source, yypos));
<INITIAL>":" => (tok (Tokens.COLON, yytext, source, yypos));
<INITIAL>":>" => (tok (Tokens.COLONGT, yytext, source, yypos));
<INITIAL>";" => (tok (Tokens.SEMICOLON, yytext, source, yypos));
<INITIAL>"=" => (tok (Tokens.EQUALOP, yytext, source, yypos));
<INITIAL>"=>" => (tok (Tokens.DARROW, yytext, source, yypos));
<INITIAL>"[" => (tok (Tokens.LBRACKET, yytext, source, yypos));
<INITIAL>"]" => (tok (Tokens.RBRACKET, yytext, source, yypos));
<INITIAL>"_" => (tok (Tokens.WILD, yytext, source, yypos));
<INITIAL>"{" => (tok (Tokens.LBRACE, yytext, source, yypos));
<INITIAL>"|" => (tok (Tokens.BAR, yytext, source, yypos));
<INITIAL>"}" => (tok (Tokens.RBRACE, yytext, source, yypos));

<INITIAL>"abstype" => (tok (Tokens.ABSTYPE, yytext, source, yypos));
<INITIAL>"and" => (tok (Tokens.AND, yytext, source, yypos));
<INITIAL>"andalso" => (tok (Tokens.ANDALSO, yytext, source, yypos));
<INITIAL>"as" => (tok (Tokens.AS, yytext, source, yypos));
<INITIAL>"case" => (tok (Tokens.CASE, yytext, source, yypos));
<INITIAL>"datatype" => (tok (Tokens.DATATYPE, yytext, source, yypos));
<INITIAL>"do" => (tok (Tokens.DO, yytext, source, yypos));
<INITIAL>"else" => (tok (Tokens.ELSE, yytext, source, yypos));
<INITIAL>"end" => (tok (Tokens.END, yytext, source, yypos));
<INITIAL>"eqtype" => (tok (Tokens.EQTYPE, yytext, source, yypos));
<INITIAL>"exception" => (tok (Tokens.EXCEPTION, yytext, source, yypos));
<INITIAL>"fn" => (tok (Tokens.FN, yytext, source, yypos));
<INITIAL>"fun" => (tok (Tokens.FUN, yytext, source, yypos));
<INITIAL>"functor" => (tok (Tokens.FUNCTOR, yytext, source, yypos));
<INITIAL>"handle" => (tok (Tokens.HANDLE, yytext, source, yypos));
<INITIAL>"if" => (tok (Tokens.IF, yytext, source, yypos));
<INITIAL>"in" => (tok (Tokens.IN, yytext, source, yypos));
<INITIAL>"include" => (tok (Tokens.INCLUDE, yytext, source, yypos));
<INITIAL>"infix" => (tok (Tokens.INFIX, yytext, source, yypos));
<INITIAL>"infixr" => (tok (Tokens.INFIXR, yytext, source, yypos));
<INITIAL>"let" => (tok (Tokens.LET, yytext, source, yypos));
<INITIAL>"local" => (tok (Tokens.LOCAL, yytext, source, yypos));
<INITIAL>"nonfix" => (tok (Tokens.NONFIX, yytext, source, yypos));
<INITIAL>"of" => (tok (Tokens.OF, yytext, source, yypos));
<INITIAL>"op" => (tok (Tokens.OP, yytext, source, yypos));
<INITIAL>"open" => (tok (Tokens.OPEN, yytext, source, yypos));
<INITIAL>"orelse" => (tok (Tokens.ORELSE, yytext, source, yypos));
<INITIAL>"raise" => (tok (Tokens.RAISE, yytext, source, yypos));
<INITIAL>"rec" => (tok (Tokens.REC, yytext, source, yypos));
<INITIAL>"sharing" => (tok (Tokens.SHARING, yytext, source, yypos));
<INITIAL>"sig" => (tok (Tokens.SIG, yytext, source, yypos));
<INITIAL>"signature" => (tok (Tokens.SIGNATURE, yytext, source, yypos));
<INITIAL>"struct" => (tok (Tokens.STRUCT, yytext, source, yypos));
<INITIAL>"structure" => (tok (Tokens.STRUCTURE, yytext, source, yypos));
<INITIAL>"then" => (tok (Tokens.THEN, yytext, source, yypos));
<INITIAL>"type" => (tok (Tokens.TYPE, yytext, source, yypos));
<INITIAL>"val" => (tok (Tokens.VAL, yytext, source, yypos));
<INITIAL>"where" => (tok (Tokens.WHERE, yytext, source, yypos));
<INITIAL>"while" => (tok (Tokens.WHILE, yytext, source, yypos));
<INITIAL>"with" => (tok (Tokens.WITH, yytext, source, yypos));
<INITIAL>"withtype" => (tok (Tokens.WITHTYPE, yytext, source, yypos));


<INITIAL>{alphanumId} => (tok' (Tokens.SHORTALPHANUMID, yytext, source, yypos));
<INITIAL>{symId} =>
   (case yytext of
       "*" => tok (Tokens.ASTERISK, yytext, source, yypos)
     | _ => tok' (Tokens.SHORTSYMID, yytext, source, yypos));
<INITIAL>{tyvarId} => (tok' (Tokens.TYVAR, yytext, source, yypos));
<INITIAL>{longAlphanumId} => (tok' (Tokens.LONGALPHANUMID, yytext, source, yypos));
<INITIAL>{longSymId} => (tok' (Tokens.LONGSYMID, yytext, source, yypos));


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
   (startText (Source.getPos (source, yypos), fn (cs, l, r) =>
               (YYBEGIN INITIAL;
                Tokens.STRING (cs, l, r)))
    ; YYBEGIN TEXT
    ; continue ());
<INITIAL>"#\"" =>
   (startText (Source.getPos (source, yypos), fn (cs, l, r) =>
               let
                  fun err () =
                     error' (l, r, "character constant not of size 1")
                  val c =
                     case Int.compare (Vector.length cs, 1) of
                        LESS => (err (); 0)
                      | EQUAL => Vector.sub (cs, 0)
                      | GREATER => (err (); Vector.sub (cs, 0))
               in
                  YYBEGIN INITIAL;
                  Tokens.CHAR (c, l, r)
               end)
    ; YYBEGIN TEXT
    ; continue ());

<TEXT>"\""       => (finishText (Source.getPos (source, lastPos (yypos, yytext))));
<TEXT>" "|!|[\035-\091]|[\093-\126] =>
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
<TEXT>\\\^.      => (error (source, yypos, yypos + 2, "Illegal control escape in text constant; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_");
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
<TEXT>\\{eol}    => (Source.newline (source, lastPos (yypos, yytext)); YYBEGIN TEXT_FMT; continue ());
<TEXT>\\         => (error (source, yypos, yypos + 1, "Illegal escape in text constant")
                     ; continue ());
<TEXT>{eol}      => (error (source, yypos, lastPos (yypos, yytext), "Unclosed text constant at end of line")
                     ; Source.newline (source, lastPos (yypos, yytext))
                     ; continue ());
<TEXT>.          => (error (source, yypos, yypos, "Illegal character in text constant")
                     ; continue ());

<TEXT_FMT>{ws}+  => (continue ());
<TEXT_FMT>{eol}  => (Source.newline (source, lastPos (yypos, yytext)); continue ());
<TEXT_FMT>\\     => (YYBEGIN TEXT; continue ());
<TEXT_FMT>.      => (error (source, yypos, yypos, "Illegal formatting character in text continuation")
                     ; continue ());


<INITIAL>"(*)" =>
   (if allowLineComments ()
       then ()
       else error (source, yypos, lastPos (yypos, yytext),
                   "Line comments disallowed, compile with -default-ann 'allowLineComments true'")
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
   (finishComment (lastPos (yypos, yytext))
    ; Source.newline (source, lastPos (yypos, yytext))
    ; continue ());
<LINE_COMMENT>. =>
   (continue ());

<BLOCK_COMMENT>"(*)" =>
   (if allowLineComments ()
       then ()
       else error (source, yypos, lastPos (yypos, yytext),
                   "Line comments disallowed, compile with -default-ann 'allowLineComments true'")
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
   (finishComment (lastPos (yypos,yytext))
    ; continue ());
<BLOCK_COMMENT>{eol} =>
   (Source.newline (source, lastPos (yypos, yytext))
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
          (addCommentError "Illegal line directive"
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
   (finishLineDir (source, lastPos (yypos, yytext))
    ; continue ());
<LINE_DIR1,LINE_DIR2,LINE_DIR3,LINE_DIR4>. =>
   (addCommentError "Illegal line directive"
    ; YYBEGIN BLOCK_COMMENT
    ; continue ());


<INITIAL>"(*#showBasis"{ws}+"\""[^"]*"\""{ws}*"*)" =>
   (let
       val file = List.nth (String.split (yytext, #"\""), 1)
       val file =
         if OS.Path.isAbsolute file
            then file
            else OS.Path.mkCanonical (OS.Path.concat (OS.Path.dir (Source.name source), file))
   in
       tok' (fn (_, l, r) => Tokens.SHOW_BASIS (file, l, r), yytext, source, yypos)
   end);


<INITIAL>. =>
   (error (source, yypos, yypos, "Illegal token")
    ; continue ());
