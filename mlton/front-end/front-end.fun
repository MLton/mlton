(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
   
fun lexAndParse (ins, source) =
   let
      val stream =
	 Parse.makeLexer (fn n => In.inputN (ins, n))
	 {comLevel = ref 0,
	  source = source,
	  stringtype = ref false,
	  stringstart = ref 0}
      val lookahead = 0
      val result =
	 (#1 (Parse.parse (lookahead, stream, fn (s, left, right) =>
			   Control.errorStr (Region.T {left = left,
						       right = right},
					     s),
			   ())))
	 handle _ =>
	    let
	       val i = Source.currentIndex source
	    in
	       Control.errorStr (Region.T {left = i, right = i}, "parse error")
	       ; Ast.Program.T []
	    end
   in result
   end

val lexAndParse =
    Trace.trace ("lexAndParse", Layout.ignore, Ast.Program.layout)
    lexAndParse

end
