(* Copyright (C) 2017 Jason Carr.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PARSE_SXML_STRUCTS =
   sig
      structure XmlTree: XML_TREE
      structure StreamParser: STREAM_PARSER
   end

signature PARSE_SXML = 
   sig
      include PARSE_SXML_STRUCTS

      val parse: char Stream.t -> XmlTree.Program.t
   end
