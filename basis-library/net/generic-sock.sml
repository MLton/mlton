(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure GenericSock : GENERIC_SOCK =
   struct
      structure Prim = PrimitiveFFI.Socket.GenericSock
      structure PE = Posix.Error
      structure PESC = PE.SysCall

      fun socket' (af, st, p) =
         PESC.simpleResult
         (fn () => Prim.socket (af, st, C_Int.fromInt p))

      fun socketPair' (af, st, p) =
         let
            val a = Array.array (2, 0)
         in
            PESC.syscall
            (fn () => (Prim.socketPair (af, st, C_Int.fromInt p, a), fn _ => 
                       (Array.sub (a, 0), Array.sub (a, 1))))
         end

      fun socket (af, st) = socket' (af, st, 0)

      fun socketPair (af, st) = socketPair' (af, st, 0)
   end
