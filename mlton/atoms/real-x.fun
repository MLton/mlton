(* Copyright (C) 2009,2011-2012 Matthew Fluet.
 * Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RealX (S: REAL_X_STRUCTS): REAL_X = 
struct

open S

structure P = Pervasive
structure PR32 = P.Real32
structure PR64 = P.Real64
structure PIR = P.IEEEReal

datatype z = datatype RealSize.t

datatype t =
   Real32 of Real32.t
 | Real64 of Real64.t

fun zero s =
   case s of
      R32 => Real32 0.0
    | R64 => Real64 0.0

fun size r =
   case r of
      Real32 _ => R32
    | Real64 _ => R64

fun make (r: string, s: RealSize.t): t option =
   let
      fun doit (fromString, isFinite, con): t option =
         case fromString r of
            NONE => Error.bug "RealX.make: unexpected real constant"
          | SOME r =>
               if isFinite r
                  then SOME (con r)
               else NONE
   in
      case s of
         R32 => doit (Real32.fromString, Real32.isFinite, Real32)
       | R64 => doit (Real64.fromString, Real64.isFinite, Real64)
   end

(* RealX.equals determines if two floating-point constants are equal.
 * Must check the sign bit, since Real{32,64}.== ignores the sign of
 * zeros; the difference between 0.0 and ~0.0 is observable by
 * programs that examine the sign bit.
 * Should check for nan, since Real{32,64}.== returns false for any
 * comparison with nan values.  Ideally, should use bit-wise equality
 * since there are multiple representations for nan.  However, SML/NJ
 * doesn't support the PackReal structures that would be required to
 * compare real values as bit patterns.  Conservatively return
 * 'false'; constant-propagation and common-subexpression elimination
 * will not combine nan values.
 *)
fun equals (r, r') =
   case (r, r') of
      (Real32 r, Real32 r') =>
         let
            open Real32
         in
            equals (r, r') andalso signBit r = signBit r'
         end
    | (Real64 r, Real64 r') =>
         let
            open Real64
         in
            equals (r, r') andalso signBit r = signBit r'
         end
    | _ => false

fun toString r =
   case r of
      Real32 r => Real32.format (r, Real32.Format.exact)
    | Real64 r => Real64.format (r, Real64.Format.exact)

val layout = Layout.str o toString

val hash = String.hash o toString

(* Disable constant folding when it might change the results. *)
fun disableCF () =
   PR32.precision = PR64.precision
   orelse !Control.target <> Control.Self

local
   fun make (o32, o64) arg =
       if disableCF ()
          then NONE
       else SOME (case arg of
                     Real32 x => Real32 (o32 x)
                   | Real64 x => Real64 (o64 x))
in
   val neg = make (Real32.~, Real64.~)
   val abs = make (Real32.abs, Real64.abs)
end

datatype 'r r =
   R of {zero: 'r, half: 'r, one: 'r, inf: 'r, abs: 'r -> 'r,
         signBit: 'r -> bool, isNan: 'r -> bool,
         toManExp: 'r -> {exp: int, man: 'r},
         compareReal: 'r * 'r -> PIR.real_order,
         bits: Bits.t,
         subVec: P.Word8Vector.vector * int -> P.LargeWord.word,
         update: P.Word8Array.array * int * P.LargeWord.word -> unit,
         toBytes: 'r -> P.Word8Vector.vector,
         subArr: P.Word8Array.array * int -> 'r,
         tag: 'r -> t}

val r32 =
    R {zero = 0.0, half = 0.5, one = 1.0, inf = PR32.posInf,
       abs = PR32.abs, signBit = PR32.signBit, isNan = PR32.isNan,
       toManExp = PR32.toManExp, compareReal = PR32.compareReal,
       bits = Bits.inWord32,
       subVec = P.PackWord32Little.subVec,
       update = P.PackWord32Little.update,
       toBytes = P.PackReal32Little.toBytes,
       subArr = P.PackReal32Little.subArr,
       tag = Real32}
val r64 =
    R {zero = 0.0, half = 0.5, one = 1.0, inf = PR64.posInf,
       abs = PR64.abs, signBit = PR64.signBit, isNan = PR64.isNan,
       toManExp = PR64.toManExp, compareReal = PR64.compareReal,
       bits = Bits.inWord64,
       subVec = P.PackWord64Little.subVec,
       update = P.PackWord64Little.update,
       toBytes = P.PackReal64Little.toBytes,
       subArr = P.PackReal64Little.subArr,
       tag = Real64}

local
   fun doit (R {compareReal, signBit, tag, ...}) (f, arg) =
       if disableCF ()
          then NONE
       else
          let
             val old = PIR.getRoundingMode ()
          in
             (* According to the Basis Library specification,
              * setRoundingMode can fail (raise an exception).
              *)
             let
                val () = PIR.setRoundingMode PIR.TO_NEGINF
                val min = f arg
                val () = PIR.setRoundingMode PIR.TO_POSINF
                val max = f arg
                val () = PIR.setRoundingMode old
             in
                if (PIR.EQUAL = compareReal (min, max)
                    andalso signBit min = signBit max)
                   then SOME (tag min)
                else NONE
             end
             handle _ =>
                (if PIR.getRoundingMode () = old
                    then ()
                 else PIR.setRoundingMode old
                 ; NONE)
          end

   fun make1 (o32, o64) =
    fn Real32 x => doit r32 (o32, x)
     | Real64 x => doit r64 (o64, x)

   fun make2 (o32, o64) =
    fn (Real32 x, Real32 y) => doit r32 (o32, (x, y))
     | (Real64 x, Real64 y) => doit r64 (o64, (x, y))
     | _ => Error.bug "impossible"

   fun make3 (o32, o64) =
    fn (Real32 x, Real32 y, Real32 z) => doit r32 (o32, (x, y, z))
     | (Real64 x, Real64 y, Real64 z) => doit r64 (o64, (x, y, z))
     | _ => Error.bug "impossible"
in
   val acos = make1 (PR32.Math.acos, PR64.Math.acos)
   val asin = make1 (PR32.Math.asin, PR64.Math.asin)
   val atan = make1 (PR32.Math.atan, PR64.Math.atan)
   val atan2 = make2 (PR32.Math.atan2, PR64.Math.atan2)
   val cos = make1 (PR32.Math.cos, PR64.Math.cos)
   val exp = make1 (PR32.Math.exp, PR64.Math.exp)
   val ln = make1 (PR32.Math.ln, PR64.Math.ln)
   val log10 = make1 (PR32.Math.log10, PR64.Math.log10)
   val sin = make1 (PR32.Math.sin, PR64.Math.sin)
   val sqrt = make1 (PR32.Math.sqrt, PR64.Math.sqrt)
   val tan = make1 (PR32.Math.tan, PR64.Math.tan)

   val add = make2 (PR32.+, PR64.+)
   val op div = make2 (PR32./, PR64./)
   val mul = make2 (PR32.*, PR64.* )
   val sub = make2 (PR32.-, PR64.-)

   val muladd = make3 (PR32.*+, PR64.*+)
   val mulsub = make3 (PR32.*-, PR64.*-)

   fun fromIntInf (i, s) =
       case s of
          R32 => doit r32 (Real32.fromIntInf, i)
        | R64 => doit r64 (Real64.fromIntInf, i)
end

local
   fun make (o32, o64) args =
       if disableCF ()
          then NONE
       else
          SOME (case args of
                   (Real32 r1, Real32 r2) => o32 (r1, r2)
                 | (Real64 r1, Real64 r2) => o64 (r1, r2)
                 | _ => Error.bug "impossible")
in
   val equal = make (PR32.==, PR64.==)
   val le = make (PR32.<=, PR64.<=)
   val lt = make (PR32.<, PR64.<)
   val qequal = make (PR32.?=, PR64.?=)
end

datatype decon =
   NAN
 | ZERO of {signBit: bool}
 | ONE of {signBit: bool}
 | POW2 of {signBit: bool, exp: Int.t} (* man = 0.5 *)
 | FIN of {signBit: bool, exp: Int.t, man: t}
 | INF of {signBit: bool}

local
   fun doit (R {zero, half, one, inf, abs, signBit, isNan, toManExp,
                compareReal, tag, ...})
            value =
       if isNan value
          then NAN
       else let
             val signBit = signBit value
             val absValue = abs value
          in
             if PIR.EQUAL = compareReal (zero, absValue)
                then ZERO {signBit = signBit}
             else if PIR.EQUAL = compareReal (one, absValue)
                then ONE {signBit = signBit}
             else if PIR.EQUAL = compareReal (inf, absValue)
                then INF {signBit = signBit}
             else let
                   val {man, exp} = toManExp absValue
                in
                   if PIR.EQUAL = compareReal (half, man)
                      then POW2 {signBit = signBit, exp = exp}
                   else FIN {signBit = signBit, exp = exp, man = tag man}
                end
          end
in
   fun decon x =
       if disableCF ()
          then NONE
       else SOME (case x of
                     Real32 x => doit r32 x
                   | Real64 x => doit r64 x)
end

local
   fun doit (R {bits, toBytes, subVec, ...}) x = let
   in
       (SOME o WordX.fromIntInf)
          (P.LargeWord.toLargeInt (subVec (toBytes x, 0)),
           WordX.WordSize.fromBits bits)
   end handle _ => NONE
in
   fun castToWord x =
       if disableCF ()
          then NONE
       else
          (case x of
              Real32 x => doit r32 x
            | Real64 x => doit r64 x)
end

local
   fun doit (R {bits, update, subArr, tag, isNan, ...}) w = let
      val a = P.Word8Array.array (Bytes.toInt (Bits.toBytes bits), 0w0)
      val () = update (a, 0, P.LargeWord.fromLargeInt (WordX.toIntInf w))
      val r = subArr (a, 0)
   in
      if isNan r
         then NONE
      else SOME (tag r)
   end handle _ => NONE
in
   fun castFromWord w =
      if disableCF () then
         NONE
      else if WordX.WordSize.bits (WordX.size w) = Bits.inWord32 then
         doit r32 w
      else if WordX.WordSize.bits (WordX.size w) = Bits.inWord64 then
         doit r64 w
      else
         Error.bug "Invalid word size"
end

end
