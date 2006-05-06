(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Word (W: PRE_WORD_EXTRA): WORD_EXTRA =
struct

open W
type t = word

val wordSize: Int.int = Primitive.Int32.toInt wordSize
val wordSizeWord: Word.word = Primitive.Word32.toWord wordSizeWord

fun << (w, n) = 
   if Word.>= (n, wordSizeWord)
      then zero
      else W.<< (w, Primitive.Word32.fromWord n)
fun >> (w, n) = 
   if Word.>= (n, wordSizeWord)
      then zero
      else W.>> (w, Primitive.Word32.fromWord n)
fun ~>> (w, n) =
   if Word.< (n, wordSizeWord)
      then W.~>> (w, Primitive.Word32.fromWord n)
      else W.~>> (w, Primitive.Word32.- (W.wordSizeWord, 0w1))
fun rol (w, n) = W.rol (w, Primitive.Word32.fromWord n)
fun ror (w, n) = W.ror (w, Primitive.Word32.fromWord n)

local
   (* Allocate a buffer large enough to hold any formatted word in any radix.
    * The most that will be required is for maxWord in binary.
    *)
   val maxNumDigits = wordSize
   val oneBuf = One.make (fn () => CharArray.array (maxNumDigits, #"\000"))
in
   fun fmt radix (w: word): string =
      One.use
      (oneBuf, fn buf =>
      let
         val radix = fromInt (StringCvt.radixToInt radix)
         fun loop (q, i: Int.int) =
            let
               val _ =
                  CharArray.update
                  (buf, i, StringCvt.digitToChar (toInt (q mod radix)))
               val q = q div radix
            in
               if q = zero
                  then CharArraySlice.vector
                       (CharArraySlice.slice (buf, i, NONE))
                  else loop (q, Int.- (i, 1))
            end
      in
         loop (w, Int.- (maxNumDigits, 1))
      end)
end

fun fmt radix (w: word): string =
   let val radix = fromInt (StringCvt.radixToInt radix)
      fun loop (q, chars) =
         let val chars = StringCvt.digitToChar (toInt (q mod radix)) :: chars
            val q = q div radix
         in if q = zero
               then PreString.implode chars
            else loop (q, chars)
         end
   in loop (w, [])
   end

val toString = fmt StringCvt.HEX

fun scan radix reader state =
   let
      val state = StringCvt.skipWS reader state
      val charToDigit = StringCvt.charToDigit radix
      val radixWord = fromInt (StringCvt.radixToInt radix)
      fun finishNum (state, n) =
         case reader state of
            NONE => SOME (n, state)
          | SOME (c, state') =>
               case charToDigit c of
                  NONE => SOME (n, state)
                | SOME n' =>
                     let val n'' = n * radixWord
                     in if n'' div radixWord = n
                           then let val n' = fromInt n'
                                   val n''' = n'' + n'
                                in if n''' >= n''
                                      then finishNum (state', n''')
                                   else raise Overflow
                                end
                        else raise Overflow
                     end
      fun num state = finishNum (state, zero)
   in
      case reader state of
         NONE => NONE
       | SOME (c, state) =>
            case c of
               #"0" =>
               (case reader state of
                   NONE => SOME (zero, state)
                 | SOME (c, state') =>
                      case c of
                         #"w" => (case radix of
                                     StringCvt.HEX =>
                                        (case reader state' of
                                            NONE =>
                                               (* the #"w" was not followed by
                                                * an #"X" or #"x", therefore we
                                                * return 0 *)
                                               SOME (zero, state)
                                          | SOME (c, state) =>
                                               (case c of
                                                   #"x" => num state
                                                 | #"X" => num state
                                                 | _ =>
                                                 (* the #"w" was not followed by
                                                  * an #"X" or #"x", therefore we
                                                  * return 0 *)
                                                      SOME (zero, state)))
                                   | _ => num state')
                       | #"x" => (case radix of
                                     StringCvt.HEX => num state'
                                   | _ => NONE)
                       | #"X" => (case radix of
                                     StringCvt.HEX => num state'
                                   | _ => NONE)
                       | _ => num state)
             | _ => (case charToDigit c of
                        NONE => NONE
                      | SOME n => finishNum (state, fromInt n))
   end

val fromString = StringCvt.scanString (scan StringCvt.HEX)

end

structure Word8 = Word (Primitive.Word8)
structure Word16 = Word (Primitive.Word16)
structure Word32 = Word (Primitive.Word32)
structure Word64 = Word (Primitive.Word64)
