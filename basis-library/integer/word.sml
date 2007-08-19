(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Word (W: PRIM_WORD) : WORD_EXTRA =
struct

open W
type t = word

val wordSize: Int.int = Primitive.Int32.zextdToInt sizeInBits
val sizeInBitsWord = Primitive.Word32.zextdToWord sizeInBitsWord

fun << (i, n) = 
   if Word.>= (n, sizeInBitsWord)
      then zero
      else W.<<? (i, Primitive.Word32.zextdFromWord n)
fun >> (i, n) = 
   if Word.>= (n, sizeInBitsWord)
      then zero
      else W.>>? (i, Primitive.Word32.zextdFromWord n)
fun ~>> (i, n) =
   if Word.< (n, sizeInBitsWord)
      then W.~>>? (i, Primitive.Word32.zextdFromWord n)
      else W.~>>? (i, Primitive.Word32.- (W.sizeInBitsWord, 0w1))
fun rol (i, n) = W.rolUnsafe (i, Primitive.Word32.zextdFromWord n)
fun ror (i, n) = W.rorUnsafe (i, Primitive.Word32.zextdFromWord n)

local
   fun st (w, msk, sft) =
      let
         val odd = andb (w, msk)
         val evn = xorb (w, odd)
      in
         (xorb (W.<<? (odd, sft), W.>>? (evn, sft)),
          xorb (msk, W.<<? (msk, Primitive.Word32.>>? (sft, 0w1))),
          Primitive.Word32.>>? (sft, 0w1))
      end
   val (f, sft) =
      case W.sizeInBitsWord of
         0w8 => (fn x => x, 0w4)
       | 0w16 => (st, 0w8)
       | 0w32 => (st o st, 0w16)
       | 0w64 => (st o st o st, 0w32)
       | _ => raise (Fail "Word.bswap")
in
   fun bswap w = #1 (f (w, W.<<? (one, sft) - one, sft))
end

val fromInt = W.sextdFromInt
val toIntX = W.schckToInt
fun toInt w =
   let
      val i = W.zchckToInt w
   in
      if Primitive.Controls.detectOverflow
         andalso (case Int.precision of
                     NONE => false
                   | SOME precision => 
                        Int32.<= (precision, W.sizeInBits))
         andalso Int.< (i, 0)
         then raise Overflow
         else i
   end
val fromLargeInt = W.sextdFromLargeInt
val toLargeIntX = W.schckToLargeInt
fun toLargeInt w =
   let
      val i = W.zchckToLargeInt w
   in
      if Primitive.Controls.detectOverflow
         andalso (case LargeInt.precision of
                     NONE => false
                   | SOME precision => 
                        Int32.<= (precision, W.sizeInBits))
         andalso LargeInt.< (i, 0)
         then raise Overflow
         else i
   end

val fromLargeWord = W.zextdFromLargeWord
val fromLarge = fromLargeWord
val toLargeWordX = W.sextdToLargeWord
val toLargeX = toLargeWordX
val toLargeWord = W.zextdToLargeWord
val toLarge = toLargeWord

val fromWord = W.zextdFromWord
val toWordX = W.sextdToWord
val toWord = W.zextdToWord

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

(*
fun fmt radix (w: word): string =
   let 
      val radix = fromInt (StringCvt.radixToInt radix)
      fun loop (q, chars) =
         let 
            val chars = StringCvt.digitToChar (toInt (q mod radix)) :: chars
            val q = q div radix
         in 
            if q = zero
               then String.implode chars
            else loop (q, chars)
         end
   in 
      loop (w, [])
   end
*)

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
