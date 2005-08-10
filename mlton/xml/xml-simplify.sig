(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature XML_SIMPLIFY_STRUCTS = 
   sig
      include XML_TREE
      val shrink: Program.t -> Program.t
      val typeCheck: Program.t -> unit
   end

signature XML_SIMPLIFY = 
   sig
      include XML_SIMPLIFY_STRUCTS
      
      val simplify: Program.t -> Program.t
   end
