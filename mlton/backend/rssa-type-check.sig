(* Copyright (C) 2009,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RSSA_TYPE_CHECK_STRUCTS =
   sig
      include RSSA_TREE
   end

signature RSSA_TYPE_CHECK =
   sig
      include RSSA_TYPE_CHECK_STRUCTS

      val typeCheck: Program.t -> unit
   end
