(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
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

      fun intToSock i = Socket.wordToSock (SysWord.fromInt i)

      fun socket' (af, st, p) =
         PESC.syscall
         (fn () =>
          let val n = Prim.socket (NetHostDB.addrFamilyToInt af, st, p)
          in (n, fn () => intToSock n)
          end)

      fun socketPair' (af, st, p) =
         let
            val a = Array.array (2, 0)
         in
            PESC.syscall
            (fn () =>
             let val n = Prim.socketPair (NetHostDB.addrFamilyToInt af, st, p, a)
             in (n, fn () => (intToSock (Array.sub (a, 0)), 
                              intToSock (Array.sub (a, 1))))
             end)
         end
      
      fun socket (af, st) = socket' (af, st, 0)
         
      fun socketPair (af, st) = socketPair' (af, st, 0)
   end
