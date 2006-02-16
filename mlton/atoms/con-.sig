(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CON_STRUCTS = 
   sig
   end

signature CON = 
   sig
      include ID
      include PRIM_CONS where type con = t

      val fromBool: bool -> t
      val stats: unit -> Layout.t
   end
