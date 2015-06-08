(* Copyright (C) 2015 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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

fun lexAndParse (source: Source.t, ins: In.t): Ast.Program.t =
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
      val () = Ast.Program.checkSyntax result

      (* Outputs AST to a file if Control.keepAST is true *)
      val () =
         if !Control.keepAST
            then File.withAppend
                 (concat [!Control.inputFile, ".ast"], fn outputStream =>
                  (Out.outputl (outputStream, concat ["File: ", Source.name source]);
                   Layout.output (Ast.Program.layout result, outputStream);
                   Out.newline outputStream;
                   Out.newline outputStream))
            else ()
   in
      result
   end

fun lexAndParseFile (f: File.t) =
   File.withIn
   (f, fn ins => lexAndParse (Source.new f, ins))

val lexAndParseFile =
    Trace.trace ("FrontEnd.lexAndParseFile", File.layout, Ast.Program.layout)
    lexAndParseFile

end
