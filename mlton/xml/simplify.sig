(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature SIMPLIFY_STRUCTS = 
   sig
      structure XmlTree: XML_TREE
   end

signature SIMPLIFY = 
   sig
      include SIMPLIFY_STRUCTS
      
      val simplify: XmlTree.Program.t -> XmlTree.Program.t
   end
