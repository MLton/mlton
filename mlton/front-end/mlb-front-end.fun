(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor MLBFrontEnd (S: MLB_FRONT_END_STRUCTS): MLB_FRONT_END = 
struct

open S

local
   val lexAndParseProgOrMLBFail = fn _ => Error.bug "lexAndParseProgOrMLB"
   val lexAndParseProgOrMLBs : (File.t * Region.t -> Ast.Basdec.node) list ref =
      ref [lexAndParseProgOrMLBFail]
in
   fun pushLexAndParseProgOrMLB lexAndParseProgOrMLB = 
      List.push (lexAndParseProgOrMLBs, lexAndParseProgOrMLB)
   fun popLexAndParseProgOrMLB () = 
      ignore (List.pop lexAndParseProgOrMLBs)

   val lexAndParseProgOrMLB = fn f => 
      List.first (!lexAndParseProgOrMLBs) f
end

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
   in result
   end

fun lexAndParseFile (f: File.t) =
   File.withIn
   (f, fn ins => lexAndParse (Source.new f, ins))

val lexAndParseFile =
    Trace.trace ("MLBFrontEnd.lexAndParseFile", File.layout, Ast.Basdec.layout)
    lexAndParseFile

fun lexAndParseString (s: String.t) =
   let 
      val source = Source.new "<string>"
      val ins = In.openString s
   in lexAndParse (source, ins)
   end

val lexAndParseString =
    Trace.trace ("MLBFrontEnd.lexAndParseString", String.layout, Ast.Basdec.layout)
    lexAndParseString

fun mkLexAndParse () =
   let
      val cwd = Dir.current ()
      val relativize = SOME cwd
      val state = {cwd = cwd, relativize = relativize, seen = []}

      val files: File.t Buffer.t = Buffer.new {dummy = "<dummy>"}

      val psi : (OS.FileSys.file_id * Ast.Basdec.t) HashSet.t =
	 HashSet.new {hash = OS.FileSys.hash o #1}

      local
	 fun make (file : File.t) =
	    if File.canRead file
	       then List.keepAllMap
		    (File.lines file, fn line =>
		     if String.forall (line, Char.isSpace)
			then NONE
			else 
			   case String.tokens (line, Char.isSpace) of
			      [var, path] => SOME {var = var, path = path}
			    | _ => Error.bug (concat ["strange mlb path mapping: ", 
						      file, ":: ", line]))
	       else []

	 val pathMap =
	    (List.rev o List.concat)
	    [make (concat [!Control.libDir, "/mlb-path-map"]),
	     case OS.Process.getEnv "HOME" of
		NONE => []
	      | SOME path => make (concat [path, "/.mlton/mlb-path-map"]),
	     [{var = "LIB_MLTON_DIR", 
	       path = OS.Path.mkAbsolute {path = !Control.libDir,
					  relativeTo = cwd}}]]
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

      fun regularize {fileOrig, cwd, relativize} =
	 let
	    val fileExp = 
	       let
		  fun loop (s, acc, accs) =
		     case s of
			[] => String.concat (List.rev ((String.fromListRev acc)::accs))
		      | (#"$")::(#"(")::s => 
			   let
			      val accs = (String.fromListRev acc)::accs
			      fun loopVar (s, acc) =
				 case s of
				    [] => Error.bug "regularize"
				  | (#")")::s => (s, String.fromListRev acc)
				  | c::s => loopVar (s, c::acc)
			      val (s, var) = loopVar (s, [])
			   in
			      case peekPathMap var of
				 NONE => loop (s, [], accs)
			       | SOME path => 
				    loop ((String.explode path) @ s, [], accs)
			   end
		      | c::s => loop (s, c::acc, accs)
	       in
		  loop (String.explode fileOrig, [], [])
	       end
	    val fileAbs = OS.Path.mkAbsolute {path = fileExp, relativeTo = cwd}
	    val relativize =
	       if OS.Path.isAbsolute fileExp
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
	 Trace.trace ("MLBFrontEnd.regularize", 
		      fn {fileOrig, cwd, relativize} =>
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

      fun lexAndParseProg {fileAbs: File.t, fileUse: File.t} =
	 let
	    val () = Buffer.add (files, fileUse)
	    val prog = FrontEnd.lexAndParseFile fileUse
	 in
	    Ast.Basdec.Prog (fileAbs, prog)
	 end
      and lexAndParseMLB {cwd: Dir.t, relativize: Dir.t option,
			  seen: (OS.FileSys.file_id * File.t * Region.t) list,
			  fileAbs: File.t, fileUse: File.t,
			  reg: Region.t} =
	 let
	    val fid = OS.FileSys.fileId fileAbs
	    val seen' = (fid, fileUse, reg)::seen
	 in
	    if List.exists (seen, fn (fid', _, _) => 
			    OS.FileSys.compare (fid, fid') = EQUAL)
	       then (let open Layout
		     in 
			Control.error 
			(reg, seq [str "Basis forms a cycle with ", File.layout fileUse],
			 align (List.map (seen', fn (_, f, r) => 
					  seq [Region.layout r, 
					       str ": ", 
					       File.layout f])))
			; Ast.Basdec.Seq []
		     end)
	       else 
		  let
		     val (_, basdec) =
			HashSet.lookupOrInsert
			(psi, OS.FileSys.hash fid, fn (fid', _) =>
			 OS.FileSys.compare (fid, fid') = EQUAL, fn () =>
			 let
			    val cwd = OS.Path.dir fileAbs
			    val basdec =
			       wrapLexAndParse
			       {cwd = cwd, relativize = relativize, seen = seen'}
			       (lexAndParseFile, fileUse)
			 in
			    (fid, basdec)
			 end)
		  in
		     Ast.Basdec.MLB (fileAbs, SOME fid, basdec)
		  end
	 end
      and lexAndParseProgOrMLB {cwd, relativize, seen}
	                       (fileOrig: File.t, reg: Region.t) =
	 let
	    val {fileAbs, fileUse, relativize, ...} = 
	       regularize {fileOrig = fileOrig, cwd = cwd, relativize = relativize}
	    fun fail msg =
	       (Control.error
		(reg, Layout.seq [Layout.str "file ", Layout.str fileOrig,
				  Layout.str " (", Layout.str fileUse, Layout.str ") ",
				  Layout.str msg], Layout.empty)
		;  Ast.Basdec.Seq [])
	 in
	    if not (File.doesExist fileUse)
	       then fail "does not exist"
	    else if not (File.canRead fileUse)
	       then fail "cannot be read"
	    else let
		    val mlbExts = ["mlb"]
		    val progExts = ["ML","fun","sig","sml"]
		    fun err () = fail "has an unknown extension"
		 in
		    case File.extension fileUse of
		       SOME s =>
			  if List.contains (mlbExts, s, String.equals)
			     then lexAndParseMLB {cwd = cwd,
						  relativize = relativize,
						  seen = seen,
						  fileAbs = fileAbs,
						  fileUse = fileUse,
						  reg = reg}
			  else if List.contains (progExts, s, String.equals)
			     then lexAndParseProg {fileAbs = fileAbs,
						   fileUse = fileUse}
			  else err ()
		     | NONE => err ()
		 end
	 end
      and wrapLexAndParse state (lexAndParse, arg) =
	 let
	    val () = pushLexAndParseProgOrMLB (lexAndParseProgOrMLB state)
	    val basdec = lexAndParse arg
	    val () = popLexAndParseProgOrMLB ()
	 in
	    basdec
	 end
   in
      fn (s: String.t) => 
      (wrapLexAndParse state (lexAndParseString, s),
       Buffer.toVector files before Buffer.reset files)
   end

val lexAndParseString = fn (s: String.t) =>
   (mkLexAndParse ()) s

end
