(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
