(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SMALL_INT_INF_STRUCTS = 
   sig
   end

signature SMALL_INT_INF = 
   sig
      include SMALL_INT_INF_STRUCTS

      type t

      val toCstring: t -> string
      val toMLstring: t -> string
      val toWord: t -> Word.t
   end
