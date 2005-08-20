(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PackReal (S: sig
                        type real
                        val bytesPerElem: int
                        val isBigEndian: bool
                        val subVec: word8 vector * int -> real
                        val subVecRev: word8 vector * int -> real
                        val update: word8 array * int * real -> unit
                        val updateRev: word8 array * int * real -> unit
                     end): PACK_REAL =
struct

open S

val (sub, up) =
   if isBigEndian = Primitive.MLton.Platform.Arch.hostIsBigEndian
      then (subVec, update)
   else (subVecRev, updateRev)

fun update (a, i, r) =
   let
      val a = Word8Array.toPoly a
      val _ = Array.checkSlice (a, i, SOME bytesPerElem)
   in
      up (a, i, r)
   end
   
local
   val a = Word8Array.array (bytesPerElem, 0w0)
in
   fun toBytes (r: real): Word8Vector.vector =
      (up (Word8Array.toPoly a, 0, r)
       ; Byte.stringToBytes (Byte.unpackString (Word8ArraySlice.full a)))
end

fun subVec (v, i) =
   let
      val v = Word8Vector.toPoly v
      val _ = Vector.checkSlice (v, i, SOME bytesPerElem)
   in
      sub (v, i)
   end

fun fromBytes v = subVec (v, 0)

fun subArr (a, i) =
   subVec (Word8Vector.fromPoly
           (Primitive.Vector.fromArray (Word8Array.toPoly a)),
           i)
   
end

structure PackReal32Big: PACK_REAL =
   PackReal (val bytesPerElem: int = 4
             val isBigEndian = true
             open Primitive.PackReal32)
structure PackReal32Little: PACK_REAL =
   PackReal (val bytesPerElem: int = 4
             val isBigEndian = false
             open Primitive.PackReal32)
structure PackReal64Big: PACK_REAL =
   PackReal (val bytesPerElem: int = 8
             val isBigEndian = true
             open Primitive.PackReal64)
structure PackReal64Little: PACK_REAL =
   PackReal (val bytesPerElem: int = 8
             val isBigEndian = false
             open Primitive.PackReal64)

structure PackRealBig = PackReal64Big
structure PackRealLittle = PackReal64Little
