(* Copyright (C) 2009,2015 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MLBFrontEnd (S: MLB_FRONT_END_STRUCTS): MLB_FRONT_END = 
struct

open S

(* The lexer recursively invokes the lexer/parser when it encounters a file
 * reference.  So, we need a stub here to feed to the lexer.  The stub is 
 * overridden after the lexer is defined.
 *)

val lexAndParseProgOrMLBRef: (File.t * Region.t -> Ast.Basdec.node) ref =
   ref (fn _ => Error.bug "MLBFrontEnd.lexAndParseProgOrMLB")

val lexAndParseProgOrMLB = fn f => !lexAndParseProgOrMLBRef f

structure LrVals = MLBLrValsFun (structure Token = LrParser.Token
                                 structure Ast = Ast
                                 val lexAndParseProgOrMLB = lexAndParseProgOrMLB)
structure Lex = MLBLexFun (structure Tokens = LrVals.Tokens)
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
               Ast.Basdec.empty
            end
      val () = Ast.Basdec.checkSyntax result

      (* Outputs AST to a file if Control.keepAST is true *)
      val () =
         if !Control.keepAST
            then File.withAppend
                 (concat [!Control.inputFile, ".ast"], fn outputStream =>
                  (Out.outputl (outputStream, concat ["File: ", Source.name source]);
                   Layout.output (Ast.Basdec.layout result, outputStream);
                   Out.newline outputStream;
                   Out.newline outputStream))
            else ()
   in 
      result
   end

fun lexAndParseFile (f: File.t) =
   File.withIn (f, fn ins => lexAndParse (Source.new f, ins))

val lexAndParseFile =
    Trace.trace ("MLBFrontEnd.lexAndParseFile", File.layout, Ast.Basdec.layout)
    lexAndParseFile

fun lexAndParseString (s: String.t) =
   let 
      val source = Source.new "<string>"
      val ins = In.openString s
   in
      lexAndParse (source, ins)
   end

val lexAndParseString =
    Trace.trace ("MLBFrontEnd.lexAndParseString", String.layout,
                 Ast.Basdec.layout)
    lexAndParseString

val lexAndParseString =
   fn (s: string) =>
   let
      val cwd = Dir.current ()
      val relativize = SOME cwd
      val state = {cwd = cwd, relativize = relativize, seen = []}
      val psi : (File.t * Ast.Basdec.t Promise.t) HashSet.t =
         HashSet.new {hash = String.hash o #1}
      local
         val pathMap =
             Control.mlbPathMap ()
         fun peekPathMap var' =
            case List.peek (pathMap, fn {var,...} =>
                            var = var') of
               NONE => NONE
             | SOME {path, ...} => SOME path
      in
         val peekPathMap =
            Trace.trace ("MLBFrontEnd.peekPathMap", 
                         String.layout,
                         Option.layout Dir.layout)
            peekPathMap
      end
      fun expandPathVars (path, seen, region) =
         let
            fun loop (s, acc, accs) =
               case s of
                  [] => String.concat (List.rev
                                       (String.fromListRev acc :: accs))
                | #"$" :: #"(" :: s => 
                     let
                        val accs = String.fromListRev acc :: accs
                        fun loopVar (s, acc) =
                           case s of
                              [] => Error.bug "MLBFrontEnd.lexAndParseString.expandPathVars"
                            | #")" :: s => (s, String.fromListRev acc)
                            | c :: s => loopVar (s, c :: acc)
                        val (s, var) = loopVar (s, [])
                     in
                        if List.exists (seen, fn x => x = var)
                           then
                              let
                                 open Layout
                              in
                                 Control.error
                                 (region,
                                  str "Cyclic MLB path variables",
                                  List.layout Layout.str (var :: seen))
                                 ; loop (s, [], accs)
                              end
                        else
                           case peekPathMap var of
                              NONE =>
                                 let
                                    open Layout
                                 in
                                    Control.error
                                    (region,
                                     seq [str "Undefined MLB path variable: ",
                                          str var],
                                     empty)
                                    ; loop (s, [], accs)
                                 end
                            | SOME path => 
                                 loop (s, [],
                                       expandPathVars (path, var :: seen, region)
                                       :: accs)
                     end
                | c :: s => loop (s, c :: acc, accs)
         in
            loop (String.explode path, [], [])
         end
      fun regularize {fileOrig, cwd, region, relativize} =
         let
            val fileExp = expandPathVars (fileOrig, [], region)
            val fileAbs = OS.Path.mkAbsolute {path = fileExp, relativeTo = cwd}
            val fileAbs = OS.Path.mkCanonical fileAbs
            val relativize =
               if !Control.preferAbsPaths orelse OS.Path.isAbsolute fileExp
                  then NONE
                  else relativize
            val fileUse =
               case relativize of
                  NONE => fileAbs
                | SOME d => OS.Path.mkRelative {path = fileAbs, relativeTo = d}
         in
            {fileAbs = fileAbs,
             fileUse = fileUse,
             relativize = relativize}
         end
      val regularize =
         Trace.trace ("MLBFrontEnd.lexAndParseString.regularize", 
                      fn {fileOrig, cwd, relativize, ...} =>
                      Layout.record
                      [("fileOrig", File.layout fileOrig),
                       ("cwd", Dir.layout cwd),
                       ("relativize", Option.layout Dir.layout relativize)],
                      fn {fileAbs, fileUse, relativize} =>
                      Layout.record
                      [("fileAbs", File.layout fileAbs),
                       ("fileUse", File.layout fileUse),
                       ("relativize", Option.layout Dir.layout relativize)])
         regularize
      fun lexAndParseProg {fileAbs: File.t, fileOrig: File.t, fileUse: File.t, 
                           fail: String.t -> Ast.Program.t} =
         Ast.Basdec.Prog
         ({fileAbs = fileAbs, fileUse = fileUse},
          Promise.delay
          (fn () =>
           Control.checkFile
           (fileUse, {fail = fail,
                      name = fileOrig,
                      ok = fn () => FrontEnd.lexAndParseFile fileUse})))
      and lexAndParseMLB {relativize: Dir.t option,
                          seen: (File.t * File.t * Region.t) list,
                          fileAbs: File.t, fileOrig: File.t, fileUse: File.t,
                          fail: String.t -> Ast.Basdec.t, reg: Region.t} =
         Ast.Basdec.MLB
         ({fileAbs = fileAbs, fileUse = fileUse},
          Promise.delay
          (fn () =>
           Control.checkFile
           (fileUse,
            {fail = fail,
             name = fileOrig,
             ok = fn () => let
                val seen' = (fileAbs, fileUse, reg) :: seen
             in
                if List.exists (seen, fn (fileAbs', _, _) => 
                                String.equals (fileAbs, fileAbs'))
                   then (let open Layout
                   in 
                            Control.error 
                            (reg, seq [str "Basis forms a cycle with ", 
                                       File.layout fileUse],
                             align (List.map (seen', fn (_, f, r) => 
                                              seq [Region.layout r, 
                                                   str ": ", 
                                                   File.layout f])))
                            ; Ast.Basdec.empty
                   end)
                else 
                   let
                      val (_, basdec) =
                         HashSet.lookupOrInsert
                         (psi, String.hash fileAbs, fn (fileAbs', _) =>
                          String.equals (fileAbs, fileAbs'), fn () =>
                          let
                             val cwd = OS.Path.dir fileAbs
                             val basdec =
                                Promise.delay
                                (fn () =>
                                 wrapLexAndParse
                                 ({cwd = cwd,
                                   relativize = relativize,
                                   seen = seen'},
                                  lexAndParseFile, fileUse))
                          in
                             (fileAbs, basdec)
                          end)
                   in
                      Promise.force basdec
                   end
             end})))
      and lexAndParseProgOrMLB {cwd, relativize, seen}
                               (fileOrig: File.t, reg: Region.t) =
         Exn.withEscape
         (fn escape =>
          let
             fun fail default msg =
                let
                   val () = Control.error (reg, Layout.str msg, Layout.empty)
                in
                   default
                end
             fun err msg =
                fail (Ast.Basdec.Seq []) (concat ["File ", fileOrig, msg])
             val {fileAbs, fileUse, relativize, ...} =
                regularize {cwd = cwd,
                            fileOrig = fileOrig,
                            region = reg,
                            relativize = relativize}
                handle _ => escape (err " could not be regularized")
             val mlbExts = ["mlb"]
             val progExts = ["ML","fun","sig","sml"]
             fun errUnknownExt () = err " has an unknown extension"
          in
             case File.extension fileUse of
                NONE => errUnknownExt ()
              | SOME s =>
                   if List.contains (mlbExts, s, String.equals) then
                      lexAndParseMLB {relativize = relativize,
                                      seen = seen,
                                      fileAbs = fileAbs,
                                      fileOrig = fileOrig,
                                      fileUse = fileUse,
                                      fail = fail Ast.Basdec.empty,
                                      reg = reg}
                   else if List.contains (progExts, s, String.equals) then
                      lexAndParseProg {fileAbs = fileAbs,
                                       fileOrig = fileOrig,
                                       fileUse = fileUse,
                                       fail = fail Ast.Program.empty}
                   else errUnknownExt ()
          end)
      and wrapLexAndParse (state, lexAndParse, arg) =
         Ref.fluidLet
         (lexAndParseProgOrMLBRef, lexAndParseProgOrMLB state, fn () =>
          lexAndParse arg)
      val dec = wrapLexAndParse (state, lexAndParseString, s)
   in
      dec
   end

end
