(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
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
   
fun lexAndParse (f: File.t) =
   File.withIn
   (f, fn ins =>
    let
       val source = Source.new f
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
    end)

val lexAndParse =
    Trace.trace ("lexAndParse", Layout.ignore, Ast.Program.layout)
    lexAndParse

end
