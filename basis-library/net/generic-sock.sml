(* Copyright (C) 2002-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure GenericSock : GENERIC_SOCK =
   struct
      structure Prim = PrimitiveFFI.Socket.GenericSock
      structure PE = Posix.Error
      structure PESC = PE.SysCall

      fun socket' (af, st, p) =
         (Socket.fromRep o PESC.simpleResult)
         (fn () => Prim.socket (Net.AddrFamily.toRep af, 
                                Socket.SOCKExtra.toRep st, 
                                C_Int.fromInt p))

      fun socketPair' (af, st, p) =
         let
            val a : C_Sock.t array = Array.array (2, C_Sock.fromInt 0)
            val get = fn i => Socket.fromRep (Array.sub (a, i))
         in
            PESC.syscall
            (fn () => (Prim.socketPair (Net.AddrFamily.toRep af, 
                                        Socket.SOCKExtra.toRep st, 
                                        C_Int.fromInt p, 
                                        a), 
                       fn _ => (get 0, get 1)))
         end

      fun socket (af, st) = socket' (af, st, 0)

      fun socketPair (af, st) = socketPair' (af, st, 0)
   end
