(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHAR_ARG =
   sig
      structure PreChar : PRE_CHAR
      structure CharVector: EQTYPE_MONO_VECTOR_EXTRA
      structure CharArray: MONO_ARRAY_EXTRA
      sharing type PreChar.char   = CharVector.elem   = CharArray.elem
      sharing type PreChar.string = CharVector.vector = CharArray.vector
   end

functor CharFn(Arg : CHAR_ARG) 
        :> CHAR_EXTRA 
            where type char   = Arg.PreChar.char 
            where type string = Arg.PreChar.string =
   struct
      open Arg.PreChar
      
      type string = Arg.CharVector.vector
      val maxOrd: int = numChars - 1
      
      val fromString = Arg.CharVector.fromPoly o 
                       Vector.map (fn x => fromChar x) o
                       String.toPoly

      fun succ c =
         if Primitive.Controls.safe 
            andalso c = maxChar
            then raise Chr
         else chrUnsafe (Int.+ (ord c, 1))

      fun pred c =
         if Primitive.Controls.safe 
            andalso c = minChar
            then raise Chr
         else chrUnsafe (Int.- (ord c, 1))

      fun chrOpt c =
         if Primitive.Controls.safe 
            andalso Int.gtu (c, maxOrd)
            then NONE
         else SOME (chrUnsafe c)

      fun chr c =
         case chrOpt c of
            NONE => raise Chr
          | SOME c => c
      
      (* To implement character classes, we cannot use lookup tables on the
       * order of the number of characters. We don't want to scan the string
       * each time, so instead we'll sort it and use binary search.
       *)
      fun contains s =
         let
            val a = Array.tabulate (Arg.CharVector.length s, 
                                    fn i => Arg.CharVector.sub (s, i))
            val () = Heap.heapSort (a, op <)
         in
            fn c =>
               let
                  val x = Heap.binarySearch (a, fn d => d < c)
               in
                  if x = Array.length a then false else
                  Array.sub (a, x) = c
               end
         end
      
      fun notContains s = not o contains s
      
      val c = fromChar
      val (  la,    lA,    lf,    lF,    lz,    lZ,    l0,    l9,  lSPACE,lBANG, lTIL,  lDEL) =
          (c#"a", c#"A", c#"f", c#"F", c#"z", c#"Z", c#"0", c#"9", c#" ", c#"!", c#"~", c#"\127")
      
      (* Range comparisons don't need tables! It's faster to just compare. *)
      fun isLower c = c >= la andalso c <= lz
      fun isUpper c = c >= lA andalso c <= lZ
      fun isDigit c = c >= l0 andalso c <= l9
      fun isGraph c = c >= lBANG  andalso c <= lTIL
      fun isPrint c = c >= lSPACE andalso c <= lTIL
      fun isCntrl c = c <  lSPACE orelse  c  = lDEL
      fun isAscii c = c <= lDEL
      
      local
         (* We can use a table for small ranges *)
         val limit = 128
         fun memoize (f: char -> 'a, g: char -> 'a): char -> 'a =
            let
               val v = Vector.tabulate (limit, f o chrUnsafe)
               val limit = chr limit
            in
               fn c => if c >= limit then g c else 
                       Vector.sub (v, ord c)
            end
         
         fun make (test, diff) =
            memoize (fn c => if test c then chrUnsafe (Int.+? (ord c, diff)) 
                                       else c,
                     fn c => c)
         val diff = Int.- (ord lA, ord la)
      
         infix || &&
         fun f || g = memoize (fn c => f c orelse  g c, fn _ => false)
         fun f && g = memoize (fn c => f c andalso g c, fn _ => false)
         
         val WS = fromString " \t\r\n\v\f"
         
         fun laf c = (c >= la andalso c <= lf) orelse
                     (c >= lA andalso c <= lF)
      in
         val isAlpha = isUpper || isLower
         val isHexDigit = isDigit || laf
         val isAlphaNum = isAlpha || isDigit
         val isSpace = memoize (contains WS, fn _ => false)
         val isPunct = isGraph && (not o isAlphaNum)
         
         val toLower = make (isUpper, Int.~ diff)
         val toUpper = make (isLower, diff)
      end
      
      fun control reader state =
         case reader state of
            NONE => NONE
          | SOME (c, state) =>
               if Char.<= (#"@", c) andalso Char.<= (c, #"_")
                  then SOME (chr (Int.-? (Char.ord c, Char.ord #"@")), state)
               else NONE

      fun formatChar reader state =
         case reader state of
            NONE => NONE
          | SOME (c, state) =>
               if StringCvt.isSpace c
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

      val 'a formatSequences: (Char.char, 'a) StringCvt.reader -> 'a -> 'a =
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
            val escape : (char, 'a) StringCvt.reader =
               fn state =>
               case reader state of
                  NONE => NONE
                | SOME (c, state') =>
                     let
                        fun yes c = SOME (fromChar c, state')
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
                         | #"U" =>
                              Reader.mapOpt chrOpt
                              (StringCvt.digitsExact (StringCvt.HEX, 8) reader)
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
                        (* isPrint doesn't exist. yuck: *)
                        if Char.>= (c, #" ") andalso Char.<= (c, #"~")
                           then
                              case c of
                                 #"\\" => escape state
                               | #"\"" => NONE
                               | _ => SOME (fromChar c, formatSequences reader state)
                        else NONE
               end
         in
            main
         end
      
      val fromString = StringCvt.scanString scan
      
      fun 'a scanC (reader: (Char.char, 'a) StringCvt.reader)
        : (char, 'a) StringCvt.reader =
         let
            val rec escape =
               fn state =>
               case reader state of
                  NONE => NONE
                | SOME (c, state') =>
                     let fun yes c = SOME (fromChar c, state')
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
                      | #"u" =>
                           Reader.mapOpt chrOpt
                           (StringCvt.digitsExact (StringCvt.HEX, 4) reader)
                           state'
                      | #"U" =>
                           Reader.mapOpt chrOpt
                           (StringCvt.digitsExact (StringCvt.HEX, 8) reader)
                           state'
                      | _ =>
                           Reader.mapOpt chrOpt
                           (StringCvt.digitsPlus (StringCvt.OCT, 3) reader)
                           state
                     end
            and main =
               fn NONE => NONE
                | SOME (c, state) =>
                     (* yuck. isPrint is not defined yet: *)
                     if Char.>= (c, #" ") andalso Char.<= (c, #"~")
                        then
                           case c of
                              #"\\" => escape state
                            | _ => SOME (fromChar c, state)
                     else NONE
         in
            main o reader
         end

      val fromCString = StringCvt.scanString scanC

      fun padLeft (s: String.string, n: int): String.string =
         let
            val m = String.size s
            val diff = Int.-? (n, m)
         in if Int.> (diff, 0)
               then String.concat [String.new (diff, #"0"), s]
            else if diff = 0
                    then s
                 else raise Fail "padLeft"
         end
      
      fun unicodeEscape ord =
          if Int.< (ord, 65536)
             then String.concat
                  ["\\u", padLeft (Int.fmt StringCvt.HEX ord, 4)]
          else String.concat
               ["\\U", padLeft (Int.fmt StringCvt.HEX ord, 8)]
      
      fun toString c =
         let
            val ord = ord c
         in
            if isPrint c
               then
                  case ord of
                     92 (* #"\\" *) => "\\\\"
                   | 34 (* #"\"" *) => "\\\""
                   | _ => String.new (1, Char.chrUnsafe ord)
                                             (* ^^^^ safe b/c isPrint < 128 *)
            else
               case ord of
                  7  (* #"\a" *) => "\\a"
                | 8  (* #"\b" *) => "\\b"
                | 9  (* #"\t" *) => "\\t"
                | 10 (* #"\n" *) => "\\n"
                | 11 (* #"\v" *) => "\\v"
                | 12 (* #"\f" *) => "\\f"
                | 13 (* #"\r" *) => "\\r"
                | _ =>
                   if Int.< (ord, 32)
                      then String.concat
                           ["\\^", String.new 
                                   (1, Char.chrUnsafe 
                                       (Int.+? (ord, 64 (* #"@" *) )))]
                   else if Int.< (ord, 256)
                      then String.concat
                           ["\\", padLeft (Int.fmt StringCvt.DEC ord, 3)]
                   else unicodeEscape ord
         end
      
      fun toCString c =
         let
            val ord = ord c
         in
            if isPrint c
               then
                  case ord of
                     92 (* #"\\" *) => "\\\\"
                   | 34 (* #"\"" *) => "\\\""
                   | 63 (* #"?"  *) => "\\?"
                   | 39 (* #"'"  *) => "\\'"
                   | _ => String.new (1, Char.chrUnsafe ord)
            else
               case ord of
                   7 (* #"\a" *) => "\\a"
                |  8 (* #"\b" *) => "\\b"
                |  9 (* #"\t" *) => "\\t"
                | 10 (* #"\n" *) => "\\n"
                | 11 (* #"\v" *) => "\\v"
                | 12 (* #"\f" *) => "\\f"
                | 13 (* #"\r" *) => "\\r"
                | _ => 
                   if Int.< (ord, 256)
                      then String.concat
                           ["\\", padLeft (Int.fmt StringCvt.OCT ord, 3)]
                   else unicodeEscape ord
         end
   end

structure CharArg : CHAR_ARG =
   struct
      structure PreChar = Char
      structure CharVector = CharVector
      structure CharArray = CharArray
   end

structure WideCharArg : CHAR_ARG =
   struct
      structure PreChar = WideChar
      structure CharVector = WideCharVector
      structure CharArray = WideCharArray
   end

structure Char : CHAR_EXTRA = CharFn(CharArg)
structure WideChar : CHAR_EXTRA = CharFn(WideCharArg)
