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
   val lexAndParseProgFail = fn _ => Error.bug "lexAndParseProg"
   val lexAndParseMLBFail = fn _ => Error.bug "lexAndParseMLB"

   val lexAndParseProgRef : (File.t * Region.t -> 
			     File.t * Ast.Program.t) list ref =
      ref [lexAndParseProgFail]
   val lexAndParseMLBRef : (File.t * Region.t -> 
			    File.t * OS.FileSys.file_id option * Ast.Basdec.t) list ref =
      ref [lexAndParseMLBFail]
in
   fun pushLexAndParse (prog, mlb) = 
      (List.push (lexAndParseProgRef, prog)
       ; List.push (lexAndParseMLBRef, mlb))
   fun popLexAndParse () = 
      (ignore (List.pop lexAndParseProgRef)
       ; ignore (List.pop lexAndParseMLBRef))

   val lexAndParseProg = fn f => List.first (!lexAndParseProgRef) f
   val lexAndParseMLB = fn f => List.first (!lexAndParseMLBRef) f
end

structure LrVals = MLBLrValsFun (structure Token = LrParser.Token
			 	 structure Ast = Ast
				 val lexAndParseProg = lexAndParseProg
				 val lexAndParseMLB = lexAndParseMLB)
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
      val files: File.t Buffer.t = Buffer.new {dummy = "<dummy>"}

      val psi : (OS.FileSys.file_id * Ast.Basdec.t) HashSet.t =
	 HashSet.new {hash = OS.FileSys.hash o #1}

      fun regularize (cwd, relativize, f) =
	 let
	    val f = 
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
			      loop (s, [],
				    case OS.Process.getEnv var of
				       NONE => accs
				     | SOME p => p::accs)
			   end
		      | c::s => loop (s, c::acc, accs)
	       in
		  loop (String.explode f, [], [])
	       end
	    val fa = OS.Path.mkAbsolute {path = f, relativeTo = cwd}
	    val relativize =
	       if OS.Path.isAbsolute f
		  then NONE
		  else relativize
	    val f =
	       case relativize of
		  NONE => fa
		| SOME d => OS.Path.mkRelative {path = fa, relativeTo = d}
	 in
	    (fa, relativize, f)
	 end

      fun lexAndParseProg (cwd: Dir.t, relativize: Dir.t option) 
	                  (f: File.t, r: Region.t) =
	 let
	    val (fa, _, f) = regularize (cwd, relativize, f)
	    fun fail msg =
	       (Control.error
		(r, Layout.seq [Layout.str "file ", Layout.str msg], Layout.empty)
		; (fa, Ast.Program.empty))
	 in
	    if not (File.doesExist f)
	       then fail (concat [f, " does not exist"])
	    else if not (File.canRead f)
	       then fail (concat [f, " cannot be read"])
	    else (Buffer.add (files, f)
		  ; (fa, FrontEnd.lexAndParseFile f))
	 end

      fun lexAndParseMLB (cwd: Dir.t, 
			  relativize: Dir.t option,
			  seen: (OS.FileSys.file_id * File.t * Region.t) list) 
                         (f: File.t, r: Region.t) =
	 let
	    val (fa, relativize, f) = regularize (cwd, relativize, f)
	    fun fail msg =
	       (Control.error
		(r, Layout.seq [Layout.str "file ", Layout.str msg], Layout.empty)
		; (fa, NONE, Ast.Basdec.empty))
	 in
	    if not (File.doesExist f)
	       then fail (concat [f, " does not exist"])
	    else if not (File.canRead f)
	       then fail (concat [f, " cannot be read"])
            else 
	       let
		  val fid = OS.FileSys.fileId fa
		  val seen' = (fid, f, r)::seen
	       in
		  if List.exists (seen, fn (fid', _, _) => OS.FileSys.compare (fid, fid') = EQUAL)
		     then (let open Layout
			   in 
			      Control.error 
			      (r, seq [str "Basis forms a cycle with ", File.layout f],
			       align (List.map (seen', fn (_, f, r) => 
						seq [Region.layout r, str ": ", File.layout f])))
			      ; (fa, SOME fid, Ast.Basdec.empty)
			   end)
		     else 
			let
			   val (_, basdec) =
			      HashSet.lookupOrInsert
			      (psi, OS.FileSys.hash fid, fn (fid', _) => 
			       OS.FileSys.compare (fid, fid') = EQUAL, fn () =>
			       let
				  val cwd = OS.Path.dir fa
				  val basdec =
				     wrapLexAndParse
				     (cwd, relativize, seen')
				     (lexAndParseFile, f)
			       in
				  (fid, basdec)
			       end)
			in
			   (fa, SOME fid, basdec)
			end
	       end
	 end

      and wrapLexAndParse (cwd, relativize, seen) (lexAndParse, arg) =
	 let
	    val () = 
	       pushLexAndParse 
	       (lexAndParseProg (cwd, relativize),
		lexAndParseMLB (cwd, relativize, seen))
	    val basdec = lexAndParse arg
	    val () = popLexAndParse ()
	 in
	    basdec
	 end

      val cwd = Dir.current ()
      val relativize = SOME cwd
      val lexAndParseFile = fn (f: File.t) =>
	 (#3 (lexAndParseMLB (cwd, relativize, []) (f, Region.bogus)),
	  Buffer.toVector files before Buffer.reset files)
      val lexAndParseString = fn (s: String.t) => 
	 (wrapLexAndParse (cwd, relativize, []) (lexAndParseString, s),
	  Buffer.toVector files before Buffer.reset files)
   in
      (lexAndParseFile, lexAndParseString)
   end

val lexAndParseFile = fn (f: File.t) =>
   (#1 (mkLexAndParse ())) f
val lexAndParseString = fn (s: String.t) =>
   (#2 (mkLexAndParse ())) s

end
