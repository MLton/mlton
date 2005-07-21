(* parser.sml *)
(* driver for Pascal parser *)

structure Parser =
struct

structure PascalLrVals = PascalLrValsFun(structure Token = LrParser.Token)

structure PascalLex = PascalLexFun(structure Tokens = PascalLrVals.Tokens)

structure PascalParser = Join(structure Lex= PascalLex
		              structure LrParser = LrParser
		              structure ParserData = PascalLrVals.ParserData)

fun parse s =
    let val dev = TextIO.openIn s
	val stream = PascalParser.makeLexer(fn i => TextIO.inputN(dev,i))
	fun error (e,i:int,_) =
	    TextIO.output(TextIO.stdOut,
               s ^ "," ^ " line " ^ (Int.toString i) ^ ", Error: " ^ e ^ "\n")
     in PascalLex.UserDeclarations.lineNum := 1;
        PascalParser.parse(30,stream,error,())
        before TextIO.closeIn dev
    end

fun keybd () =
    let val stream = 
	    PascalParser.makeLexer (fn i => TextIO.inputLine TextIO.stdIn)
        fun error (e,i:int,_) =
	    TextIO.output(TextIO.stdOut,
              "std_in," ^ " line " ^ (Int.toString i) ^ ", Error: " ^ e ^ "\n")
     in PascalLex.UserDeclarations.lineNum := 1;
	PascalParser.parse(0,stream,error,())
    end

end (* structure Parser *)
