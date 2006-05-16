(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure StringCvt: STRING_CVT_EXTRA =
   struct
      open Reader

      val wordFromInt = Word.sextdFromInt

      datatype radix = BIN | OCT | DEC | HEX

      val radixToInt: radix -> int =
         fn BIN => 2
          | OCT => 8
          | DEC => 10
          | HEX => 16
      val radixToWord: radix -> word = wordFromInt o radixToInt

      datatype realfmt =
         SCI of int option 
       | FIX of int option 
       | GEN of int option 
       | EXACT
         
      type ('a, 'b) reader = 'b -> ('a * 'b) option

      open Int

      local
         fun pad f (c: char) i s =
            let
               val n = PreString.size s
            in
               if n >= i
                  then s
               else f (s, PreString.vector (i -? n, c))
            end
      in
         val padLeft = pad (fn (s, pad) => PreString.^ (pad, s))
         val padRight = pad PreString.^
      end

      fun splitl p f src =
         let fun done chars = PreString.implode (rev chars)
            fun loop (src, chars) =
               case f src of
                  NONE => (done chars, src)
                | SOME (c, src') =>
                     if p c
                        then loop (src', c :: chars)
                     else (done chars, src)
         in loop (src, [])
         end

      fun takel p f s = #1 (splitl p f s)
      fun dropl p f s = #2 (splitl p f s)

      fun skipWS x = dropl PreChar.isSpace x

      type cs = int

      fun stringReader (s: string): (char, cs) reader =
         fn i => if i >= PreString.size s
                    then NONE
                 else SOME (PreString.sub (s, i), i + 1)
                    
      fun 'a scanString (f: ((char, cs) reader -> ('a, cs) reader)) (s: string)
        : 'a option =
         case f (stringReader s) 0 of
            NONE => NONE
          | SOME (a, _) => SOME a

      local
         fun range (add: int, cmin: char, cmax: char): char -> int option =
            let val min = PreChar.ord cmin
            in fn c => if PreChar.<= (cmin, c) andalso PreChar.<= (c, cmax)
                          then SOME (add +? PreChar.ord c -? min)
                       else NONE
            end

         fun 'a combine (ds: (char -> 'a option) list): char -> 'a option =
            PreChar.memoize
            (fn c =>
             let
                val rec loop =
                   fn [] => NONE
                    | d :: ds =>
                         case d c of
                            NONE => loop ds
                          | z => z
             in loop ds
             end)
            
         val bin = PreChar.memoize (range (0, #"0", #"1"))
         val oct = PreChar.memoize (range (0, #"0", #"7"))
         val dec = PreChar.memoize (range (0, #"0", #"9"))
         val hex = combine [range (0, #"0", #"9"),
                            range (10, #"a", #"f"),
                            range (10, #"A", #"F")]
      in
         fun charToDigit (radix: radix): char -> int option =
            case radix of
               BIN => bin
             | OCT => oct
             | DEC => dec
             | HEX => hex
      end
   
      fun charToWDigit radix = (Option.map wordFromInt) o (charToDigit radix)

      fun digits (radix, max, accum) reader state =
         let
            val r = radixToInt radix
            fun loop (max, accum, state) =
               let fun done () = SOME (accum, state)
               in if max <= 0
                     then done ()
                  else
                     case reader state of
                        NONE => done ()
                      | SOME (c, state) =>
                           case charToDigit radix c of
                              NONE => done ()
                            | SOME n => loop (max - 1, n + accum * r, state)
               end
         in loop (max, accum, state)
         end

      fun digitsPlus (radix, max) reader state =
         case reader state of
            NONE => NONE
          | SOME (c, state) =>
               case charToDigit radix c of
                  NONE => NONE
                | SOME n => digits (radix, max -? 1, n) reader state

      fun digitsExact (radix, num) reader state =
         let val r = radixToInt radix
            fun loop (num, accum, state) =
               if num <= 0
                  then SOME (accum, state)
               else
                  case reader state of
                     NONE => NONE
                   | SOME (c, state) =>
                        case charToDigit radix c of
                           NONE => NONE
                         | SOME n => loop (num - 1, n + accum * r, state)
         in loop (num, 0, state)
         end

      fun digits radix reader state =
         let 
            val r = radixToInt radix
            fun loop (accum, state) =
               case reader state of
                  NONE => SOME (accum, state)
                | SOME (c, state') =>
                     case charToDigit radix c of
                        NONE => SOME (accum, state)
                      | SOME n => loop (n + accum * r, state')
         in case reader state of
            NONE => NONE
          | SOME (c, state) =>
               case charToDigit radix c of
                  NONE => NONE
                | SOME n => loop (n, state)
         end

      fun wdigits radix reader state =
         let 
            val op + = Word.+
            val op * = Word.*
            val r = radixToWord radix
            fun loop (accum, state) =
               case reader state of
                  NONE => SOME (accum, state)
                | SOME (c, state') =>
                     case charToWDigit radix c of
                        NONE => SOME (accum, state)
                      | SOME n => loop (n + accum * r, state')
         in case reader state of
            NONE => NONE
          | SOME (c, state) =>
               case charToWDigit radix c of
                  NONE => NONE
                | SOME n => loop (n, state)
         end

      fun digitToChar (n: int): char = PreString.sub ("0123456789ABCDEF", n)
   end
