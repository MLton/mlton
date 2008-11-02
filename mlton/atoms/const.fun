(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Const (S: CONST_STRUCTS): CONST =
struct

open S

structure ConstType = ConstType (struct
                                    structure RealSize = RealX.RealSize
                                    structure WordSize = WordX.WordSize
                                 end)

structure SmallIntInf =
   struct
      structure WordSize = WordX.WordSize

      fun toWord (i: IntInf.t): WordX.t option =
         let
            val ws = WordSize.smallIntInfWord ()
            val ws' = WordSize.fromBits (Bits.- (WordSize.bits ws, Bits.one))
         in
            if WordSize.isInRange (ws', i, {signed = true})
               then SOME (WordX.orb (WordX.one ws,
                                     WordX.lshift (WordX.fromIntInf (i, ws),
                                                   WordX.one ws)))
               else NONE
         end

      val isSmall = isSome o toWord

      fun fromWord (w: WordX.t): IntInf.t =
         WordX.toIntInfX (WordX.rshift (w, WordX.one (WordX.size w), {signed = true}))
   end

datatype t =
   IntInf of IntInf.t
 | Null
 | Real of RealX.t
 | Word of WordX.t
 | WordVector of WordXVector.t

val intInf = IntInf
val null = Null
val real = Real
val word = Word
val wordVector = WordVector

val string = wordVector o WordXVector.fromString

local
   open Layout
   fun wrap (pre, post, s) = seq [str pre, String.layout s, str post]
in
   val layout =
      fn IntInf i => IntInf.layout i
       | Null => str "NULL"
       | Real r => RealX.layout r
       | Word w => WordX.layout w
       | WordVector v => wrap ("\"", "\"", WordXVector.toString v)
end

val toString = Layout.toString o layout

fun hash (c: t): word =
   case c of
      IntInf i => IntInf.hash i
    | Null => 0wx0
    | Real r => RealX.hash r
    | Word w => WordX.hash w
    | WordVector v => WordXVector.hash v

fun equals (c, c') =
   case (c, c') of
      (IntInf i, IntInf i') => IntInf.equals (i, i')
    | (Null, Null) => true
    | (Real r, Real r') => RealX.equals (r, r')
    | (Word w, Word w') => WordX.equals (w, w')
    | (WordVector v, WordVector v') => WordXVector.equals (v, v')
    | _ => false

val equals = Trace.trace2 ("Const.equals", layout, layout, Bool.layout) equals

val lookup: ({default: string option, name: string} * ConstType.t -> t) ref =
   ref (fn _ => Error.bug "Const.lookup: not set")

end
