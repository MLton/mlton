(* Copyright (C) 2002-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature NET =
   sig
      structure AddrFamily : ABS_REP_EQ where type Rep.t = C_Int.t
      structure Sock : ABS_REP where type Rep.t = C_Sock.t
      structure SockType : ABS_REP_EQ where type Rep.t = C_Sock.t

      structure Word16 :
         sig
            val hton: Word16.word -> Word16.word
            val ntoh: Word16.word -> Word16.word
         end
      structure C_Int :
         sig
            val hton: C_Int.t -> C_Int.t
            val ntoh: C_Int.t -> C_Int.t
         end
   end
