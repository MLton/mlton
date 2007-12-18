(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor ParseGenParserFun(structure Header : HEADER
                          structure Parser : ARG_PARSER
                            where type pos = Header.pos
                          sharing type Parser.result = Header.parseResult
                          sharing type Parser.arg = Header.inputSource =
                                          Parser.lexarg
                         ) : PARSE_GEN_PARSER =

 struct
      structure Header = Header
      val parse = fn file =>
          let
              val in_str = TextIO.openIn file
              val source = Header.newSource(file,in_str,TextIO.stdOut)
              val error = fn (s : string,p:Header.pos,_) =>
                              Header.error source p s
              val stream =  Parser.makeLexer (fn i => (TextIO.inputN(in_str,i)))
                            source
              val (result,_) = (#line Header.pos := 1;
                                #start Header.pos := 0;
                                Header.text := nil;
                                Parser.parse(15,stream,error,source))
           in (TextIO.closeIn in_str; (result,source))
           end
  end;
