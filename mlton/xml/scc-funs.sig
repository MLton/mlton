(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(* Compute strongly connected components on fun decs to make them
 * as small as possible.
 *)

signature SCC_FUNS_STRUCTS = 
   sig
      include XML_TREE
   end

signature SCC_FUNS = 
   sig
      include SCC_FUNS_STRUCTS
      
      val sccFuns: Program.t -> Program.t
   end
