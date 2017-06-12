signature PARSE_SXML_STRUCTS =
   sig
      structure XmlTree: XML_TREE
      structure Stream: STREAM
      structure StreamParser: STREAM_PARSER
      sharing Stream = StreamParser.Stream 
   end

signature PARSE_SXML = 
   sig
      include PARSE_SXML_STRUCTS

      val parse: char Stream.t -> XmlTree.Program.t
   end
