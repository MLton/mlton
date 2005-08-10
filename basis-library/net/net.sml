(* Copyright (C) 2002-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure Net : NET =
   struct
      structure Prim = Primitive.Net

(*      val htonl = Prim.htonl *)
(*      val ntohl = Prim.ntohl *)
      val htons = Prim.htons
      val ntohs = Prim.ntohs
   end
