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
