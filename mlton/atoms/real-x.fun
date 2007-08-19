(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RealX (S: REAL_X_STRUCTS): REAL_X = 
struct

open S

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

(* We need to check the sign bit when comparing reals so that we don't treat
 * 0.0 and ~0.0 identically.  The difference between the two is detectable by
 * user programs that look at the sign bit.
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

end
