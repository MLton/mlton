(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure GenericSock : GENERIC_SOCK =
   struct
      structure Prim = Primitive.Socket.GenericSock
      structure PE = Posix.Error
      structure PESC = PE.SysCall

      fun intToSock i = Socket.wordToSock (SysWord.fromInt i)

      fun socket' (af, st, p) =
         PESC.syscall
         (fn () =>
          let val n = Prim.socket (NetHostDB.addrFamilyToInt af, st, p)
          in (n, fn () => intToSock n)
          end)

      fun socketPair' (af, st, p) =
         let
            val s1 = ref 0
            val s2 = ref 0
         in
            PESC.syscall
            (fn () =>
             let val n = Prim.socketPair (NetHostDB.addrFamilyToInt af, st, p, s1, s2)
             in (n, fn () => (intToSock (!s1), intToSock (!s2)))
             end)
         end
      
      fun socket (af, st) = socket' (af, st, 0)
         
      fun socketPair (af, st) = socketPair' (af, st, 0)
   end
