(*
 * Author: Matthew Fluet (fluet@cs.cornell.edu) 
 *
 * This requires that you have SML/NJ installed.
 * It works with SML/NJ 110.47 and may require changes to work with other
 * versions, since it depends on the CM structure.
 *
 * cm2mlb takes a ".cm" file and prints on stdout a corresponding ".mlb".
 * cm2mlb will look in $HOME/.mlton/cm2mlb-map.
 *
 * To use from the REPL, do the following:
 * CM2MLB.cm2mlb {defines = ["MLton"],
 *                maps = [],
 *                sources = "sources.cm",
 *	          out = TextIO.stdOut}
 *
 * Before using from the shell, you must do the following, where <smlnj> is
 * the root directory of the SML/NJ installation.  You may need to be root in
 * order to do these.
 * 1. From the SML/NJ REPL:
 *      CM.make "sources.cm";
 *      CM2MLB.export ();
 * 2. ln -s <smlnj>/bin/.run-sml <smlnj>/bin/cm2mlb
 * 3. mv cm2mlb.x86-linux <smlnj>/bin/.heap
 *
 * Once it is installed, the usage is as follows:
 *   cm2mlb [-Dsym ...] [-map file] sources.cm
 *
 * -Dsym can be used to define CM preprocessor symbols.
 * -map file can be used to add cm2mlb mappings.
 *)

structure CM2MLB :
sig
   val cm2mlb : {defines: string list,
		 maps: string list,
		 out: TextIO.outstream,
		 sources: string} -> unit
   val main: string * string list -> OS.Process.status
   val export : unit -> unit
end =
struct
   structure PG = PortableGraph

   fun message s = TextIO.output (TextIO.stdErr, s ^ "\n")
   fun die msg =
      (message ("Error: " ^ msg)
       ; OS.Process.exit OS.Process.failure)

   structure CM =
      struct
	 open CM

	 structure Graph =
	    struct
	       val graph = fn src =>
		  (Graph.graph src)
		  handle _ => NONE
	    end
      end

   fun cm2mlb {defines, maps, out, sources} =
      let
	 (* Define preprocessor symbols *)
	 val _ = 
	    List.app 
	    (fn sym => (#set (CM.symval sym)) (SOME 1))
	    defines
	 val _ = (#set CM.Control.verbose) false
	 val _ = (#set CM.Control.warn_obsolete) false
	 val _ = Control.printWarnings := false

	 local
	    fun make (file : string) =
	       if OS.FileSys.access (file, [OS.FileSys.A_READ])
		  then 
		     let
			val lines =
			   let
			      val f = TextIO.openIn file
			   in
			      let
				 fun loop lines =
				    case TextIO.inputLine f of
				       NONE => List.rev lines
				     | SOME l => loop (l::lines)
			      in
				 loop []
				 before TextIO.closeIn f
			      end handle e => (TextIO.closeIn f; raise e)
			   end handle _ => []
		     in
			List.mapPartial
			(fn line =>
			 if CharVector.all Char.isSpace line
			    then NONE
			    else 
			       case String.tokens Char.isSpace line of
				  [cmLib, mlbLib] => SOME {cmLib = cmLib, mlbLib = mlbLib}
				| _ => die (concat ["strange cm->mlb mapping: ", 
						    file, ":: ", line]))
			lines
		     end
		  else []
	    val libMap =
	       (List.rev o List.concat)
	       ((List.map make maps) @
		[case OS.Process.getEnv "HOME" of
		    NONE => []
		  | SOME path => make (concat [path, "/.mlton/cm2mlb-map"]),
		 [{cmLib = "$/basis.cm", 
		   mlbLib = "$(MLTON_ROOT)/basis/basis.mlb"}]])
	       
	    fun peekLibMap cmLib' =
	       case List.find (fn {cmLib, ...} => cmLib = cmLib') libMap of
		  NONE => NONE
		| SOME {mlbLib, ...} => SOME mlbLib
	 in
	    val peekLibMap = peekLibMap
	 end
      in
	 case CM.Graph.graph sources of
	    SOME {graph as PG.GRAPH {imports, ...}, imports = importLibs, nativesrc} =>
	       let
		  val imports =
		     ListPair.map
		     (fn (bid, cmLib) =>
		      let
			 val cmLibDescr = CM.Library.descr cmLib
			 val cmLibOSString = CM.Library.osstring cmLib
		      in
			 concat 
			 ["  basis ", bid, " = \n",
			  "    bas\n",
			  "      ",
			  case peekLibMap cmLibDescr of
			     SOME mlbLib => concat ["(* ", cmLibDescr, " ====> *) ", mlbLib, "\n"]
			   | NONE => let
					val mlbLib = concat [OS.Path.base cmLibOSString, ".mlb"]
				     in
					concat ["(* ", cmLibOSString, " =??=> *) ", mlbLib, "\n"]
				     end,
			  "    end\n"]
		      end)
		     (imports, importLibs)
	       in
		  TextIO.output (out, "local\n");
		  List.app (fn s => TextIO.output (out, s)) imports;
		  TextIO.output (out, "in\n");
		  GenMLB.gen {graph = graph,
			      nativesrc = nativesrc,
			      importprefix = fn _ => "",
			      exportprefix = "",
			      outstream = out};
		  TextIO.output (out, "end\n")
	       end
	  | NONE => ()
      end

   fun usage msg =
      (message "Usage: cm2mlb [-Dsym ...] [-map file] sources.cm"
       ; die msg)

   fun main (_, args) =
      let
	 val defines = ref ["MLton"]
	 val maps = ref []
	 fun loop args = 
	    case args of
	       [file] =>
		  cm2mlb {defines = !defines,
			  maps = !maps,
			  out = TextIO.stdOut,
			  sources = file}
	     | flag :: args =>
		  if String.isPrefix "-D" flag
		     then
			(defines := String.extract (flag, 2, NONE) :: !defines
			 ; loop args)
		  else if "-map" = flag
		     then case args of
			    file :: args => (maps := file :: !maps
					     ; loop args)
			  | _ => usage "missing map file"
		  else usage (String.concat ["invalid flag ", flag])
	     | _ => usage "wrong number of arguments"
      in
	 loop args handle _ => die "cm2mlb failed"
	 ; 0
      end

   fun export () =
      SMLofNJ.exportFn
      ("cm2mlb", main)
end
