(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor FrontEnd (S: FRONT_END_STRUCTS): FRONT_END = 
struct

open S

structure LrVals = MLLrValsFun (structure Token = LrParser.Token
				structure Ast = Ast)
structure Lex = MLLexFun (structure Tokens = LrVals.Tokens)
structure Parse = JoinWithArg (structure ParserData = LrVals.ParserData
			       structure Lex = Lex
			       structure LrParser = LrParser)
   
fun lexAndParse (source: Source.t, ins: In.t) =
   let
      val stream =
	 Parse.makeLexer (fn n => In.inputN (ins, n))
	 {source = source}
      val lookahead = 30
      val result =
	 (#1 (Parse.parse (lookahead, stream, fn (s, left, right) =>
			   Control.errorStr (Region.make {left = left,
							  right = right},
					     s),
			   ())))
	 handle _ =>
	    let
	       val i = Source.lineStart source
	       val _ = 
		  Control.errorStr (Region.make {left = i, right = i},
				    "parse error")
	    in
	       Ast.Program.T []
	    end
   in result
   end
   
fun lexAndParseFile (f: File.t) =
   File.withIn
   (f, fn ins => lexAndParse (Source.new f, ins))

val lexAndParseFile =
    Trace.trace ("FrontEnd.lexAndParseFile", File.layout, Ast.Program.layout)
    lexAndParseFile

end
