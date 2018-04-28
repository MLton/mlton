(* Copyright (C) 2017 Jason Carr.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PARSE_SXML_STRUCTS =
   sig
      structure XmlTree: XML_TREE
   end

signature PARSE_SXML =
   sig
      include PARSE_SXML_STRUCTS

      val program: XmlTree.Program.t Parse.t 
   end
