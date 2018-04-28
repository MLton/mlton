(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Compute strongly connected components on fun decs to make them
 * as small as possible.
 *)

signature SCC_FUNS_STRUCTS = 
   sig
      include TYPE_CHECK
   end

signature SCC_FUNS = 
   sig
      include SCC_FUNS_STRUCTS

      val sccFuns: Program.t -> Program.t
   end
