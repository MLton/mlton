(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHAR0 =
   sig
      eqtype char
      eqtype string
      
      val minChar: char
      val maxOrd: int
      val maxChar: char
      
      (* raw chr,ord methods *)
      val chr: int -> char
      val ord: char -> int
      val < : char * char -> bool
      
      val toUnicode: char -> Word32.word
      val fromUnicode: Word32.word -> char
      
      structure CharVector : 
         MONO_VECTOR 
         where type elem = char 
         where type vector = string
   end
   
functor CharFn(Char0 : CHAR0) : CHAR_EXTRA =
   struct
      open Char0
      
      (* required operators *)
      fun a >= b = not (a < b)
      fun a >  b = b < a
      fun a <= b = not (b < a)
      
      (* for convenience: *)
      val op + = Int.+
      val op - = Int.-
      
      fun succ c =
         if Primitive.safe andalso c = maxChar
            then raise Chr
         else chr (ord c + 1)
      
      fun pred c =
         if Primitive.safe andalso c = minChar
            then raise Chr
         else chr (ord c - 1)
      
      fun chrOpt c =
         if Primitive.safe andalso Primitive.Int.gtu (c, maxOrd)
            then NONE
         else SOME (chr c)

      fun chr c =
         case chrOpt c of
            NONE => raise Chr
          | SOME c => c

      val {compare, ...} = Util.makeCompare (op <)
      
      val isUpper = Charset.isUpper o toUnicode
      val isLower = Charset.isLower o toUnicode
      val isDigit = Charset.isDigit o toUnicode
      val isAlpha = Charset.isAlpha o toUnicode
      val isGraph = Charset.isGraph o toUnicode
      val isPrint = Charset.isPrint o toUnicode
      val isPunct = Charset.isPunct o toUnicode
      val isCntrl = Charset.isCntrl o toUnicode
      val isSpace = Charset.isSpace o toUnicode
      val isAscii = Charset.isAscii o toUnicode
      val isAlphaNum = Charset.isAlphaNum o toUnicode
      val isHexDigit = Charset.isHexDigit o toUnicode
      
      val toUpper = fromUnicode o Charset.toUpper o toUnicode
      val toLower = fromUnicode o Charset.toLower o toUnicode
      
      (* !!! fixme, use a table in SOME situations *)
      fun contains s c = CharVector.exists (fn d => c = d) s
      fun notContains s c = not (contains s c)

(*      
      fun control reader state =
         case reader state of
            NONE => NONE
          | SOME (c, state) =>
               if #"@" <= c andalso c <= #"_"
                  then SOME (chr (ord c -? ord #"@"), state)
               else NONE

      fun formatChar reader state =
         case reader state of
            NONE => NONE
          | SOME (c, state) =>
               if Ascii.isSpace c
                  then SOME ((), state)
               else NONE

      fun formatChars reader =
         let
            fun loop state =
               case formatChar reader state of
                  NONE => state
                | SOME ((), state) => loop state
         in
            loop
         end
                  
      val 'a formatSequences: (char, 'a) StringCvt.reader -> 'a -> 'a =
         fn reader =>
         let
            fun loop state =
               case reader state of
                  SOME (#"\\", state1) =>
                     (case formatChar reader state1 of
                         NONE => state
                       | SOME ((), state2) =>
                            let
                               val state3 = formatChars reader state2
                            in
                               case reader state3 of
                                  SOME (#"\\", state4) => loop state4
                                | _ => state
                            end)
                | _ => state
         in
            loop
         end

      fun 'a scan (reader: (Char.char, 'a) StringCvt.reader)
        : (char, 'a) StringCvt.reader =
         let
            val escape: (char, 'a) StringCvt.reader =
               fn state =>
               case reader state of
                  NONE => NONE
                | SOME (c, state') =>
                     let
                        fun yes c = SOME (c, state')
                     in
                        case c of
                           #"a" => yes #"\a"
                         | #"b" => yes #"\b"
                         | #"t" => yes #"\t"
                         | #"n" => yes #"\n"
                         | #"v" => yes #"\v"
                         | #"f" => yes #"\f"
                         | #"r" => yes #"\r"
                         | #"\\" => yes #"\\"
                         | #"\"" => yes #"\""
                         | #"^" => control reader state'
                         | #"u" =>
                              Reader.mapOpt chrOpt
                              (StringCvt.digitsExact (StringCvt.HEX, 4) reader)
                              state'
                         | _ => (* 3 decimal digits *)
                              Reader.mapOpt chrOpt
                              (StringCvt.digitsExact (StringCvt.DEC, 3)
                               reader)
                              state
                     end
            val main: (char, 'a) StringCvt.reader =
               fn state =>
               let
                  val state = formatSequences reader state
               in
                  case reader state of
                     NONE => NONE
                   | SOME (c, state) =>
                        if isPrint c
                           then
                              case c of
                                 #"\\" => escape state
                               | #"\"" => NONE
                               | _ => SOME (c, formatSequences reader state)
                        else NONE
               end
         in
            main
         end

      val fromString = StringCvt.scanString scan

      fun 'a scanC (reader: (char, 'a) StringCvt.reader)
        : (char, 'a) StringCvt.reader =
         let
            val rec escape =
               fn state =>
               case reader state of
                  NONE => NONE
                | SOME (c, state') =>
                     let fun yes c = SOME (c, state')
                     in case c of
                        #"a" => yes #"\a"
                      | #"b" => yes #"\b"
                      | #"t" => yes #"\t"
                      | #"n" => yes #"\n"
                      | #"v" => yes #"\v"
                      | #"f" => yes #"\f"
                      | #"r" => yes #"\r"
                      | #"?" => yes #"?"
                      | #"\\" => yes #"\\"
                      | #"\"" => yes #"\""
                      | #"'" => yes #"'"
                      | #"^" => control reader state'
                      | #"x" =>
                           Reader.mapOpt chrOpt
                           (StringCvt.digits StringCvt.HEX reader)
                           state'
                      | _ =>
                           Reader.mapOpt chrOpt
                           (StringCvt.digitsPlus (StringCvt.OCT, 3) reader)
                           state
                     end
            and main =
               fn NONE => NONE
                | SOME (c, state) =>
                     if isPrint c
                        then
                           case c of
                              #"\\" => escape state
                            | _ => SOME (c, state)
                     else NONE
         in
            main o reader
         end

      val fromCString = StringCvt.scanString scanC

*)
      fun padLeft (s: String.string, n: int): String.string =
         let
            val m = Char1Vector.length s
            val diff = n -? m
         in if Int.> (diff, 0)
               then Char1Vector.concat [Char1Vector.tabulate (diff, fn _ => #"0"), s]
            else if diff = 0
                    then s
                 else raise Fail "padLeft"
         end
      
      fun memoize (f: char -> 'a): char -> 'a =
         let val a = Array.tabulate (numChars, f o Char.chr)
         in fn c => Array.sub (a, Char.ord c)
         end
      
      val toString =
         memoize
         (fn c =>
          if isPrint c
             then
                (case c of
                    #"\\" => "\\\\"
                  | #"\"" => "\\\""
                  | _ => CharVector.new (1, c))
          else
             case c of
                #"\a" => "\\a"
              | #"\b" => "\\b"
              | #"\t" => "\\t"
              | #"\n" => "\\n"
              | #"\v" => "\\v"
              | #"\f" => "\\f"
              | #"\r" => "\\r"
              | _ =>
                   if c < #" "
                      then (String.concat
                            ["\\^", CharVector.new (1, chr (ord c +? ord #"@"))])
                   else String.concat 
                        ["\\", padLeft (Int.fmt StringCvt.DEC (ord c), 3)])
      
      val toCString =
         memoize
         (fn c =>
          if isPrint c
             then
                (case c of
                    #"\\" => "\\\\"
                  | #"\"" => "\\\""
                  | #"?" => "\\?"
                  | #"'" => "\\'"
                  | _ => String0.str c)
          else
             case c of
                #"\a" => "\\a"
              | #"\b" => "\\b"
              | #"\t" => "\\t"
              | #"\n" => "\\n"
              | #"\v" => "\\v"
              | #"\f" => "\\f"
              | #"\r" => "\\r"
              | _ =>
                   String.concat
                   ["\\", padLeft (Int.fmt StringCvt.OCT (ord c), 3)])
   end

structure CharGlobal: CHAR_GLOBAL = Char
open CharGlobal
