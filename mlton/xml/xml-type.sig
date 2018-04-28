(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature XML_TYPE =
   sig
      include HASH_TYPE

      datatype dest =
         Var of Tyvar.t
       | Con of Tycon.t * t vector

      val dest: t -> dest
   end
