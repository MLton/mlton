(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log: parse.sml,v $
 * Revision 1.2  1997/05/23 16:21:10  dbm
 *   SML '97 sharing, where clauses.
 *
# Revision 1.1.1.1  1997/01/14  01:38:06  george
#   Version 109.24
#
 * Revision 1.2  1996/02/26  15:02:38  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:46  george
 * Version 109
 * 
 *)

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
	      val error = fn (s : string,i:int,_) =>
		              Header.error source i s
	      val stream =  Parser.makeLexer (fn i => (TextIO.inputN(in_str,i)))
		            source
	      val (result,_) = (Header.lineno := 1; 
				Header.text := nil;
		                Parser.parse(15,stream,error,source))
	   in (TextIO.closeIn in_str; (result,source))
	   end
  end;
