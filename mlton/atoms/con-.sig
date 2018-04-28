(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CON_STRUCTS = 
   sig
   end

signature CON = 
   sig
      include ID
      include PRIM_CONS
      sharing type t = con

      val fromBool: bool -> t
      val stats: unit -> Layout.t
   end
