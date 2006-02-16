(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Const (S: CONST_STRUCTS): CONST = 
struct

open S

structure ConstType = ConstType ()

structure SmallIntInf =
   struct
      structure Word = Pervasive.Word

      val minSmall: IntInf.t = ~0x40000000
      val maxSmall: IntInf.t = 0x3FFFFFFF

      fun isSmall (i: IntInf.t): bool =
         minSmall <= i andalso i <= maxSmall

      fun toWord (i: IntInf.t): word option =
         if isSmall i
            then SOME (Word.orb (0w1,
                                 Word.<< (Word.fromInt (IntInf.toInt i),
                                          0w1)))
         else NONE

      fun fromWord (w: word): IntInf.t =
         IntInf.fromInt (Word.toIntX (Word.~>> (w, 0w1)))
   end

datatype t =
   IntInf of IntInf.t
 | Real of RealX.t
 | Word of WordX.t
 | WordVector of WordXVector.t

val real = Real
val intInf = IntInf
val word = Word
val wordVector = WordVector

val word8 = word o WordX.fromWord8 
val string = wordVector o WordXVector.fromString
   
local
   open Layout
   fun wrap (pre, post, s) = seq [str pre, String.layout s, str post]
in
   val layout =
      fn IntInf i => IntInf.layout i
       | Real r => RealX.layout r
       | Word w => WordX.layout w
       | WordVector v => wrap ("\"", "\"", WordXVector.toString v)
end      

val toString = Layout.toString o layout

fun hash (c: t): word =
   case c of
      IntInf i => String.hash (IntInf.toString i)
    | Real r => RealX.hash r
    | Word w => Word.fromIntInf (WordX.toIntInf w)
    | WordVector v => String.hash (WordXVector.toString v)
   
fun equals (c, c') =
   case (c, c') of
      (IntInf i, IntInf i') => IntInf.equals (i, i')
    | (Real r, Real r') => RealX.equals (r, r')
    | (Word w, Word w') => WordX.equals (w, w')
    | (WordVector v, WordVector v') => WordXVector.equals (v, v')
    | _ => false

val equals = Trace.trace2 ("Const.equals", layout, layout, Bool.layout) equals

val lookup: ({default: string option, name: string} * ConstType.t -> t) ref =
   ref (fn _ => Error.bug "Const.lookup: not set")

end
