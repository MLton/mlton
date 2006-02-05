(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Net : NET =
   struct
      structure Prim = PrimitiveFFI.Net

      val htonl = Primitive.Word32.toInt32 o Prim.htonl o Primitive.Word32.fromInt32
      val ntohl = Primitive.Word32.toInt32 o Prim.ntohl o Primitive.Word32.fromInt32
      val htons = Primitive.Word16.toInt16 o Prim.htons o Primitive.Word16.fromInt16
      val ntohs = Primitive.Word16.toInt16 o Prim.ntohs o Primitive.Word16.fromInt16
   end
