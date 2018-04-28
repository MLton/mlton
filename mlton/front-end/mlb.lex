(* Copyright (C) 2009,2016,2017 Matthew Fluet.
 * Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
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


(* Text Constants *)
local
   val chars: char list ref = ref []
   val inText = ref false
   val textLeft = ref SourcePos.bogus
   val textFinishFn: (string * SourcePos.t * SourcePos.t -> lexresult) ref = ref (fn _ => raise Fail "textFinish")
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
         val cs = String.fromListRev (!chars)
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
   fun addTextString (s: string) =
      chars := String.fold (s, !chars, fn (c, ac) => c :: ac)
   val inText = fn () => !inText
end
fun addTextChar (c: char) = addTextString (String.fromChar c)
fun addTextNumEsc (source, yypos, yytext, drop, radix): unit =
   let
      val left = yypos
      val right = lastPos (left, yytext)
      fun err () =
         error (source, left, right, "Illegal numeric escape in text constant")
   in
      case StringCvt.scanString (fn r => IntInf.scan (radix, r)) (String.dropPrefix (yytext, drop)) of
         NONE => err ()
       | SOME i => if i > 255
                      then err ()
                      else addTextChar (Char.chr (IntInf.toInt i))
   end
fun addTextUTF8 (source, yypos, yytext): unit =
   addTextString yytext


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

%header (functor MLBLexFun (structure Tokens : MLB_TOKENS));
%arg ({source});

ws=\t|"\011"|"\012"|" ";
cr="\013";
nl="\010";
eol=({cr}{nl}|{nl}|{cr});

alphanum=[A-Za-z0-9'_];
alphanumId=[A-Za-z]{alphanum}*;
id={alphanumId};

pathvar="$("([A-Z_][A-Z0-9_]*)")";
filename=({pathvar}|[A-Za-z0-9_.])({pathvar}|[-A-Za-z0-9_.])*;
arc=({pathvar}|{filename}|"."|"..");
relpath=({arc}"/")*;
abspath="/"{relpath};
path={relpath}|{abspath};
file={path}{filename};

decDigit=[0-9];
hexDigit=[0-9a-fA-F];

%%
<INITIAL>{ws}+  => (continue ());
<INITIAL>{eol}  => (Source.newline (source, lastPos (yypos, yytext)); continue ());

<INITIAL>"_prim" => (tok (Tokens.PRIM, yytext, source, yypos));

<INITIAL>"," => (tok (Tokens.COMMA, yytext, source, yypos));
<INITIAL>";" => (tok (Tokens.SEMICOLON, yytext, source, yypos));
<INITIAL>"=" => (tok (Tokens.EQUALOP, yytext, source, yypos));

<INITIAL>"and" => (tok (Tokens.AND, yytext, source, yypos));
<INITIAL>"ann" => (tok (Tokens.ANN, yytext, source, yypos));
<INITIAL>"bas" => (tok (Tokens.BAS, yytext, source, yypos));
<INITIAL>"basis" => (tok (Tokens.BASIS, yytext, source, yypos));
<INITIAL>"end" => (tok (Tokens.END, yytext, source, yypos));
<INITIAL>"functor" => (tok (Tokens.FUNCTOR, yytext, source, yypos));
<INITIAL>"in" => (tok (Tokens.IN, yytext, source, yypos));
<INITIAL>"let" => (tok (Tokens.LET, yytext, source, yypos));
<INITIAL>"local" => (tok (Tokens.LOCAL, yytext, source, yypos));
<INITIAL>"open" => (tok (Tokens.OPEN, yytext, source, yypos));
<INITIAL>"signature" => (tok (Tokens.SIGNATURE, yytext, source, yypos));
<INITIAL>"structure" => (tok (Tokens.STRUCTURE, yytext, source, yypos));

<INITIAL>{id} => (tok' (Tokens.ID, yytext, source, yypos));
<INITIAL>{file} => (tok' (Tokens.FILE, yytext, source, yypos));

<INITIAL>"\"" =>
   (startText (Source.getPos (source, yypos), fn (s, l, r) =>
               (YYBEGIN INITIAL;
                Tokens.STRING (s, l, r)))
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
                                    StringCvt.DEC)
                     ; continue ());
<TEXT>\\u{hexDigit}{4} =>
                    (addTextNumEsc (source, yypos, yytext, 2,
                                    StringCvt.HEX)
                     ; continue ());
<TEXT>\\U{hexDigit}{8} =>
                    (addTextNumEsc (source, yypos, yytext, 2,
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
   (startComment (source, yypos, fn () =>
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
   (startComment (source, yypos, fn () =>
                  YYBEGIN BLOCK_COMMENT)
    ; YYBEGIN LINE_COMMENT
    ; continue ());
<BLOCK_COMMENT>"(*" =>
   (startComment (source, yypos, fn () =>
                  YYBEGIN BLOCK_COMMENT)
    ; YYBEGIN BLOCK_COMMENT
    ; continue ());
<BLOCK_COMMENT>"*)" =>
   (finishComment (lastPos (yypos, yytext))
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


<INITIAL>. =>
   (error (source, yypos, yypos, "Illegal character")
    ; continue ());
