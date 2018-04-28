(* Copyright (C) 2014,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Const (S: CONST_STRUCTS): CONST =
struct

open S

structure ConstType = ConstType (struct
                                    structure RealSize = RealX.RealSize
                                    structure WordSize = WordX.WordSize
                                 end)

structure IntInfRep =
   struct
      structure WordSize = WordX.WordSize
      datatype t = Big of WordXVector.t | Small of WordX.t
      fun fromIntInf (i: IntInf.t) : t =
         let
            val sws = WordSize.smallIntInfWord ()
            val sws' = WordSize.fromBits (Bits.- (WordSize.bits sws, Bits.one))
         in
            if WordSize.isInRange (sws', i, {signed = true})
               then Small (WordX.orb (WordX.one sws,
                                      WordX.lshift (WordX.fromIntInf (i, sws), WordX.one sws)))
            else let
                    val bws = WordSize.bigIntInfWord ()
                    val bbws = Bits.toWord (WordSize.bits bws)
                    val mask = IntInf.- (WordSize.cardinality bws, IntInf.one)
                    fun loop (i, acc) =
                       if IntInf.isZero i
                          then Big (WordXVector.fromListRev ({elementSize = bws}, acc))
                       else let
                               val quot = IntInf.~>> (i, bbws)
                               val rem = IntInf.andb (i, mask)
                            in
                               loop (quot, (WordX.fromIntInf (rem, bws)) :: acc)
                            end
                 in
                    loop (if IntInf.>= (i, IntInf.zero)
                             then (i, [WordX.zero bws])
                          else (IntInf.~ i, [WordX.one bws]))
                 end
         end
      fun smallToIntInf (w: WordX.t): IntInf.t option =
         let
            val sws = WordSize.smallIntInfWord ()
            val one = WordX.one sws
         in
            if WordSize.equals (WordX.size w, sws)
               andalso WordX.isOne (WordX.andb (w, one))
               then SOME (WordX.toIntInfX (WordX.rshift (w, one, {signed = true})))
            else NONE
         end
      fun bigToIntInf (v: WordXVector.t): IntInf.t option =
         let
            val bws = WordSize.bigIntInfWord ()
            val bbws = Bits.toWord (WordSize.bits bws)
         in
            if WordSize.equals (WordXVector.elementSize v, bws)
               andalso WordXVector.length v >= 2
               then let
                       val v0 = WordXVector.sub (v, 0)
                       fun mag () =
                          WordXVector.foldFrom
                          (v, 1, IntInf.zero, fn (w, i) =>
                           IntInf.andb (IntInf.<< (i, bbws), WordX.toIntInf w))
                    in
                       if WordX.isZero v0
                          then SOME (mag ())
                       else if WordX.isOne v0
                          then SOME (IntInf.~ (mag ()))
                       else NONE
                    end
            else NONE
         end
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

local
   fun make (s, deOpt : t -> 'a option) =
      let
         fun de (t: t): 'a =
            case deOpt t of
               SOME z => z
             | NONE => Error.bug ("Const.de" ^ s)
         val is: t -> bool = isSome o deOpt
      in
         (deOpt, de, is)
      end
in
   val (deWordOpt,deWord,_) = make ("Word", fn Word ws => SOME ws | _ => NONE)
end

val string = wordVector o WordXVector.fromString

fun layout c =
   case c of
      IntInf i => IntInf.layout i
    | Null => Layout.str "NULL"
    | Real r => RealX.layout r
    | Word w => WordX.layout w
    | WordVector v => WordXVector.layout v

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
