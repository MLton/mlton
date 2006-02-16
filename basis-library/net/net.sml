(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Net : NET =
   struct
      structure Prim = Primitive.Net

(*      val htonl = Prim.htonl *)
(*      val ntohl = Prim.ntohl *)
      val htons = Prim.htons
      val ntohs = Prim.ntohs
   end
