(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
