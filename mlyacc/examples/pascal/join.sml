structure PascalLrVals = PascalLrValsFun(structure Token = LrParser.Token)
structure PascalLex = PascalLexFun(structure Tokens = PascalLrVals.Tokens)
structure PascalParser = Join(structure Lex= PascalLex
		              structure LrParser = LrParser
		              structure ParserData = PascalLrVals.ParserData)

val parse = fn s =>
  let val dev = open_in s
      val stream = PascalParser.makeLexer(fn i => input(dev,i))
      val _ = PascalLex.UserDeclarations.lineNum := 1
      val error = fn (e,i:int,_) => output(std_out,s ^ "," ^
		  " line " ^ (makestring i) ^ ", Error: " ^ e ^ "\n")
  in PascalParser.parse(30,stream,error,()) before close_in dev
  end

val keybd = fn () =>
  let val dev = std_in

  (* note: some implementations of ML, such as SML of NJ,
     have more efficient versions of input_line in their built-in
     environment
   *)

      val input_line = fn f =>
         let fun loop result =
              let val c = input (f,1)
	          val result = c :: result
               in if String.size c = 0 orelse c = "\n" then
		      String.implode (rev result)
	          else loop result
	       end
         in loop nil
         end
      val stream = PascalParser.makeLexer (fn i => input_line dev)
      val _ = PascalLex.UserDeclarations.lineNum := 1
      val error = fn (e,i:int,_) => output(std_out,"std_in," ^
		  " line " ^ (makestring i) ^ ", Error: " ^ e ^ "\n")
  in PascalParser.parse(0,stream,error,())
  end
