(*
 * Authors: Stephen Weeks (sweeks@sweeks.com)
 *          Matthew Fluet (fluet@cs.cornell.edu) 
 *
 * This requires that you have SML/NJ installed.
 * It works with SML/NJ 110.44 and may require changes to work with other
 * versions, since it depends on the CM structure.
 *
 * cmcat takes a ".cm" file and prints on stdout a list of files in the order
 * deduced by CM.
 *
 * To use from the REPL, do the following:
 * CMcat.cmcat {comments = true,
 *              defines = [],
 *              sources = "sources.cm",
 *	        out = TextIO.stdOut}
 *
 * Before using from the shell, you must do the following, where <smlnj> is
 * the root directory of the SML/NJ installation.  You may need to be root in
 * order to do these.
 * 1. From the SML/NJ REPL:
 *      CM.make "sources.cm";
 *      CMcat.export ();
 * 2. ln -s <smlnj>/bin/.run-sml <smlnj>/bin/cmcat
 * 3. mv cmcat.x86-linux <smlnj>/bin/.heap
 *
 * Once it is installed, the usage is as follows:
 *   cmcat [-comments] [-Dsym ...] sources.cm
 *
 * -comments can be used to add comments to the output, including import dependencies.
 * -Dsym can be used to define CM preprocessor symbols.
 *)

structure CMcat :
sig
   val cmcat : {comments: bool,
		defines: String.t list,
		out: Out.t,
		sources: String.t} -> unit
   val main: string * string list -> OS.Process.status
   val export : unit -> unit
end =
struct
   structure PG = PortableGraph
   structure Graph = DirectedGraph
   structure Node = Graph.Node
   structure Edge = Graph.Edge

   fun message s = Out.output (Out.error, s ^ "\n")

   structure CM =
      struct
	 open CM

	 structure Graph =
	    struct
	       val graph = fn (env, src) =>
		  (Graph.graph (env, src))
		  handle _ => NONE
	    end
      end

   structure EnvSrcDescr :> 
      sig
	 type t
	 val make : CM.SrcPath.env * String.t * String.t -> t
	 val src : t -> String.t
	 val descr : t -> String.t
	 val env : t -> CM.SrcPath.env
	 val equals : t * t -> bool
	 val hash : t -> Word.t
	 val toString : t -> String.t
      end =
      struct
	 datatype t = T of CM.SrcPath.env * String.t * String.t

	 fun make (env,src,descr) = T (env,src,descr)
	 fun src (T (_,s,_)) = s
	 fun descr (T (_,_,d)) = d
	 fun env (T (e,_,_)) = e

	 fun equals (T (env1,src1,descr1), T (env2,src2,descr2)) =
	    String.equals(src1, src2)
	 fun hash (T (env, src, descr)) =
	    String.hash src
	 fun toString (T (env, src, descr)) =
	    concat [descr, ":", src]
      end

   structure Closure =
      struct
	 fun topoSortImportGraph (env,source) =
	    let
	       datatype t = T of {graph: {graph: PG.graph,
					  imports: EnvSrcDescr.t List.t,
					  nativesrc: String.t -> String.t} option,
				  hash: Word.t,
				  node: t Node.t,
				  envsrcdescr: EnvSrcDescr.t}

	       val g : t Graph.t = Graph.new ()
	       val m : t HashSet.t =
		  HashSet.new {hash = fn T {hash, ...} => hash}
	       val {get : t Node.t -> t,
		    set, rem} =
		  Property.getSetOnce
		  (Node.plist, Property.initRaise ("topoSortImportGraph:get", Node.layout))

	       val todo = ref [(EnvSrcDescr.make (env,source,source),fn _ => ())];

	       fun closure () =
		  if List.length (!todo) = 0
		     then ()
		     else DynamicWind.withEscape
			  (fn esc =>
			   let
			      val (envsrcdescr,finish) = List.pop todo
			      val hash = EnvSrcDescr.hash envsrcdescr

			      val T {node, ...} =
				 HashSet.lookupOrInsert
				 (m, hash, fn T {envsrcdescr = envsrcdescr', ...} => 
				  EnvSrcDescr.equals(envsrcdescr, envsrcdescr'),
				  fn () => 
				  let
				     val env = EnvSrcDescr.env envsrcdescr
				     val src = EnvSrcDescr.src envsrcdescr
				  in
				  case CM.Graph.graph (env, src) of
				     NONE => let
						val node = Graph.newNode g
						val result =
						   T {graph = NONE,
						      hash = hash,
						      node = node,
						      envsrcdescr = envsrcdescr}
						val _ = set(node, result)
					     in
						result
					     end
				   | SOME {graph, imports, nativesrc, ...} => 
					let
					   val node = Graph.newNode g
					   val imports =
					      List.map
					      (imports, fn (env,lib) =>
					       let
						  val src = CM.Library.osstring lib
						  val descr = CM.Library.descr lib
						  val descr = List.last (String.split(descr, #":"))
						  val finish = fn import_node =>
						     (ignore o Graph.addEdge)
						     (g, {from = import_node, to = node})
						  val envsrcdescr = EnvSrcDescr.make (env, src, descr)
					       in
						  List.push(todo, (envsrcdescr, finish)) ;
						  envsrcdescr
					       end)
					   val result = 
					      T {graph = SOME {graph = graph,
							       imports = imports,
							       nativesrc = nativesrc},
						 hash = hash,
						 node = node,
						 envsrcdescr = envsrcdescr}
					in
					   set (node, result) ;
					   result
					end
				  end)
			      val _ = finish node
			   in
			      closure ()
			   end)
	       val _ = closure ()

	       val libs =
		  case Graph.topologicalSort g of
		     NONE => raise Fail "topologicalSort of import graph failed"
		   | SOME nodes => 
			let
			   val libs = 
			      List.map
			      (nodes, fn n =>
			       let
				  val T {graph, envsrcdescr, ...} = get n
			       in
				  {graph = graph,
				   envsrcdescr = envsrcdescr}
			       end)
			in
			   libs
			end
	    in
	       libs
	    end

	 fun filter (libs : {graph: {graph: PG.graph, 
				     imports: EnvSrcDescr.t List.t,
				     nativesrc: String.t -> String.t} option, 
			     envsrcdescr: EnvSrcDescr.t} List.t) =
	    let
	       datatype t = T of {hash: Word.t,
				  lhs: EnvSrcDescr.t * PG.varname,
				  syms: (EnvSrcDescr.t * PG.namespace * String.t * t Node.t) list}
	       val symsNodesDefs : t HashSet.t =
		  HashSet.new {hash = fn T {hash, ...} => hash}

	       datatype s = S of {hash: Word.t,
				  known: Bool.t,
				  envsrcdescr: EnvSrcDescr.t,
				  syms: (EnvSrcDescr.t * PG.namespace * String.t * t Node.t) list ref}
	       val exports : s HashSet.t =
		  HashSet.new {hash = fn S {hash, ...} => hash}

	       val g : t Graph.t = Graph.new ()
	       val {get : t Node.t -> (unit -> unit),
		    set, rem} =
		  Property.getSetOnce
		  (Node.plist, Property.initRaise ("filter:get", Node.layout))

	       datatype w = W of {hash: Word.t,
				  lhs: EnvSrcDescr.t * PG.varname}
	       val keep : w HashSet.t =
		  HashSet.new {hash = fn W {hash, ...} => hash}
	       val addKeep =
		  fn (envsrcdescr,vn) =>
		  let
		     val hash = Word.xorb(EnvSrcDescr.hash envsrcdescr, String.hash vn)
		     val result = W {hash = hash,
				     lhs = (envsrcdescr, vn)}
		  in
		     fn () =>
		     (HashSet.insertIfNew
		      (keep, hash, fn W {lhs = (envsrcdescr',vn'), ...} =>
		       EnvSrcDescr.equals(envsrcdescr, envsrcdescr') andalso
		       vn = vn', fn () => result,
		       fn _ => raise Fail "keep") ;
		      ())
		  end

	       datatype x = X of {hash: Word.t,
				  envsrcdescr: EnvSrcDescr.t,
				  syms: (PG.namespace * String.t) list ref}
	       val imports : x HashSet.t =
		  HashSet.new {hash = fn X {hash, ...} => hash}
	       val addImport =
		  fn (envsrcdescr,ns,s) =>
		  let
		     val hash = EnvSrcDescr.hash envsrcdescr
		  in
		     fn () =>
		     let
			val X {syms, ...} =
			   HashSet.lookupOrInsert
			   (imports, hash, fn X {envsrcdescr = envsrcdescr', ...} =>
			    EnvSrcDescr.equals(envsrcdescr, envsrcdescr'), fn () =>
			    X {hash = hash,
			       envsrcdescr = envsrcdescr,
			       syms = ref []})
		     in
			List.push(syms,(ns,s))
		     end
		  end

	       val _ =
		  List.foreach
		  (libs, 
		   fn {graph = NONE, envsrcdescr, ...} =>
		   let
		      val hash = EnvSrcDescr.hash envsrcdescr
		      val _ =
			 HashSet.insertIfNew
			 (exports, hash, 
			  fn S {envsrcdescr = envsrcdescr', ...} => 
			  EnvSrcDescr.equals(envsrcdescr, envsrcdescr'),
			  fn () => S {hash = hash,
				      known = false,
				      envsrcdescr = envsrcdescr,
				      syms = ref []},
			  fn _ => raise Fail (concat ["envsrcdescr: ", 
						      EnvSrcDescr.toString envsrcdescr, 
						      " repeated"]))
		   in
		      ()
		   end
		    | {graph = SOME {graph = PG.GRAPH {defs, export, imports}, 
				     imports = imports', ...},
		       envsrcdescr, ...} =>
		   let
		      val envsrcdescr_hash = EnvSrcDescr.hash envsrcdescr

		      local
			 val imports =
			    List.map2(imports, imports', fn (vn,import) =>
				      (vn,
				       let
					  val hash = EnvSrcDescr.hash import
					  val S {known, syms as envSyms, ...} =
					     case HashSet.peek
						  (exports, hash, fn S {envsrcdescr, ...} =>
						   EnvSrcDescr.equals(import, envsrcdescr)) of
						NONE => raise Fail (concat ["envsrcdescr: ", 
									    EnvSrcDescr.toString envsrcdescr, 
									    " unknown"])
					      | SOME s => s
				       in
					  if known
					     then 
						fn symsSyms =>
						List.keepAll
						(!envSyms, fn (envsrcdescr,ns,v,node) =>
						 List.contains(symsSyms,(ns,v),(op =)))
					     else 
						fn symsSyms =>
						List.map
						(symsSyms, fn (ns,s) =>
						 case List.peek(!syms, fn (_,ns',s',_) => (ns,s) = (ns',s')) of
						    SOME z => z
						  | NONE => let
							       val node = Graph.newNode g
							       val _ = set(node,addImport (import,ns,s))
							       val z = (import,ns,s,node)
							    in
							       List.push(syms,z) ;
							       z
							    end)
				       end))
		      in
			 val importFn =
			    String.memoizeList(fn _ => raise Fail "importFn",
					       imports)
		      end

		      datatype u = U of {hash: Word.t,
					 lhs: PG.varname,
					 sym: PG.namespace * String.t}
		      val symDefs : u HashSet.t =
			 HashSet.new {hash = fn U {hash, ...} => hash}
		      datatype v = V of {hash: Word.t,
					 lhs: PG.varname,
					 syms: (PG.namespace * String.t) list}
		      val symsDefs : v HashSet.t =
			 HashSet.new {hash = fn V {hash, ...} => hash}
				
		      val _ =
			 List.foreach
			 (defs, fn PG.DEF {lhs, rhs} =>
			  case rhs of
			     PG.SYM (ns,s) =>
				let
				   val hash = String.hash lhs
				   val result = U {hash = hash,
						   lhs = lhs,
						   sym = (ns,s)}
				in
				   HashSet.insertIfNew
				   (symDefs, hash, fn U {lhs = lhs', ...} =>
				    lhs = lhs', fn () => result,
				    fn _ => raise Fail (concat ["lhs: ", lhs, " violates VARNAME_ONCE"])) ;
				   ()
				end
			   | PG.SYMS vns => 
				let
				   val hash = String.hash lhs
				   val syms =
				      List.foldr
				      (vns, [], fn (vn,symsAcc) =>
				       let val hash = String.hash vn
				       in
					  case HashSet.peek
					       (symDefs, hash, fn U {lhs, ...} =>
						vn = lhs) of
					     NONE => raise Fail (concat ["lhs: ", lhs, " violates SYM_TYPE"])
					   | SOME (U {sym, ...}) => sym::symsAcc
				       end)
				   val result =
				      V {hash = hash,
					 lhs = lhs,
					 syms = syms}
				in
				   HashSet.insertIfNew
				   (symsDefs, hash, fn V {lhs = lhs', ...} =>
				    lhs = lhs', fn () => result,
				    fn _ => raise Fail (concat ["lhs: ", lhs, " violates VARNAME_ONCE"])) ;
				   ()
				end
			   | PG.IMPORT {lib, syms} => 
				let
				   val hash = Word.xorb(envsrcdescr_hash, String.hash lhs)

				   val symsSyms =
				      let val hash = String.hash syms
				      in
					 case HashSet.peek
					      (symsDefs, hash, fn V {lhs, ...} =>
					       syms = lhs) of
					    NONE => raise Fail (concat ["lhs: ", lhs, " violates SYMS_TYPE"])
					  | SOME (V {syms, ...}) => syms
				      end
				   val syms = importFn lib symsSyms
				   val result = 
				      T {hash = hash,
					 lhs = (envsrcdescr, lhs),
					 syms = syms}
				in
				   HashSet.insertIfNew
				   (symsNodesDefs, hash, fn T {lhs = (envsrcdescr',lhs'), ...} =>
				    EnvSrcDescr.equals(envsrcdescr, envsrcdescr') andalso
				    lhs = lhs', fn () => result,
				    fn _ => raise Fail (concat ["lhs: ", lhs, " violates VARNAME_ONCE"])) ;
				   ()
				end
			   | PG.COMPILE {src, env, syms} => 
				let
				   val hash = Word.xorb(envsrcdescr_hash, String.hash lhs)
				   val envSyms =
				      let val hash = Word.xorb(envsrcdescr_hash, String.hash env)
				      in
					 case HashSet.peek
					      (symsNodesDefs, hash, fn T {lhs = (envsrcdescr',env'), ...} =>
					       EnvSrcDescr.equals(envsrcdescr, envsrcdescr') andalso
					       env = env') of
					    NONE => raise Fail (concat ["lhs: ", lhs, " violates ENV_TYPE"])
					  | SOME (T {syms, ...}) => syms
				      end	
				   val symsSyms =
				      let val hash = String.hash syms
				      in
					 case HashSet.peek
					      (symsDefs, hash, fn V {lhs, ...} =>
					       syms = lhs) of
					    NONE => raise Fail (concat ["lhs: ", lhs, " violates SYMS_TYPE"])
					  | SOME (V {syms, ...}) => syms
				      end
				   val node = Graph.newNode g
				   val _ = set(node, addKeep (envsrcdescr, lhs))
				   val _ = 
				      List.foreach
				      (envSyms, fn (_,_,_,node') =>
				       ignore(Graph.addEdge(g, {from = node, to = node'})))
				   val syms =
				      List.map
				      (symsSyms, fn (ns,v) =>
				       (envsrcdescr,ns,v,node))
				   val result = 
				      T {hash = hash,
					 lhs = (envsrcdescr, lhs),
					 syms = syms}
				in
				   HashSet.insertIfNew
				   (symsNodesDefs, hash, fn T {lhs = (envsrcdescr',lhs'), ...} =>
				    EnvSrcDescr.equals(envsrcdescr, envsrcdescr') andalso
				    lhs = lhs', fn () => result,
				    fn _ => raise Fail (concat ["lhs: ", lhs, " violates VARNAME_ONCE"])) ;
				   ()
				end
			   | PG.FILTER {env, syms} => 
				let
				   val hash = Word.xorb(envsrcdescr_hash, String.hash lhs)
				   val envSyms =
				      let val hash = Word.xorb(envsrcdescr_hash, String.hash env)
				      in
					 case HashSet.peek
					      (symsNodesDefs, hash, fn T {lhs = (envsrcdescr',env'), ...} =>
					       EnvSrcDescr.equals(envsrcdescr, envsrcdescr') andalso
					       env = env') of
					    NONE => raise Fail (concat ["lhs: ", lhs, " violates ENV_TYPE"])
					  | SOME (T {syms, ...}) => syms
				      end
				   val symsSyms =
				      let val hash = String.hash syms
				      in
					 case HashSet.peek
					      (symsDefs, hash, fn V {lhs, ...} =>
					       syms = lhs) of
					    NONE => raise Fail (concat ["lhs: ", lhs, " violates SYMS_TYPE"])
					  | SOME (V {syms, ...}) => syms
				      end
				   val syms =
				      List.keepAll
				      (envSyms, fn (envsrcdescr,ns,v,node) =>
				       List.contains(symsSyms,(ns,v),(op =)))
				   val result = 
				      T {hash = hash,
					 lhs = (envsrcdescr, lhs),
					 syms = syms}
				in
				   HashSet.insertIfNew
				   (symsNodesDefs, hash, fn T {lhs = (envsrcdescr',lhs'), ...} =>
				    EnvSrcDescr.equals(envsrcdescr, envsrcdescr') andalso
				    lhs = lhs', fn () => result,
				    fn _ => raise Fail (concat ["lhs: ", lhs, " violates VARNAME_ONCE"])) ;
				   ()
				end
			   | PG.MERGE vns => 
				let
				   val hash = Word.xorb(envsrcdescr_hash, String.hash lhs)
				   val syms =
				      List.foldr
				      (vns, [], fn (vn,symsAcc) =>
				       let val hash = Word.xorb(envsrcdescr_hash, String.hash vn)
				       in
					  case HashSet.peek
					       (symsNodesDefs, hash, fn T {lhs = (envsrcdescr',vn'), ...} =>
						EnvSrcDescr.equals(envsrcdescr, envsrcdescr') andalso
						vn = vn') of
					     NONE => raise Fail (concat ["lhs: ", lhs, " violates ENV_TYPE"])
					   | SOME (T {syms, ...}) => symsAcc @ syms
				       end)
				   val result =
				      T {hash = hash,
					 lhs = (envsrcdescr, lhs),
					 syms = syms}
				in	
				   HashSet.insertIfNew
				   (symsNodesDefs, hash, fn T {lhs = (envsrcdescr',lhs'), ...} =>
				    EnvSrcDescr.equals(envsrcdescr, envsrcdescr') andalso
				    lhs = lhs', fn () => result,
				    fn _ => raise Fail (concat ["lhs: ", lhs, " violates VARNAME_ONCE"])) ;
				   ()
				end)

		      val exportSyms =
			 let val hash = Word.xorb(envsrcdescr_hash, String.hash export)
			 in
			    case HashSet.peek
			         (symsNodesDefs, hash, fn T {lhs = (envsrcdescr',export'), ...} =>
				  EnvSrcDescr.equals(envsrcdescr, envsrcdescr') andalso
				  export = export') of
			       NONE => raise Fail (concat ["lhs: ", export, " violates ENV_TYPE"])
			     | SOME (T {syms, ...}) => syms
			 end
		      val result = S {hash = envsrcdescr_hash,
				      known = true,
				      envsrcdescr = envsrcdescr,
				      syms = ref exportSyms}
		      val _ =
			 HashSet.insertIfNew
			 (exports, envsrcdescr_hash, fn S {envsrcdescr = envsrcdescr', ...} =>
			  EnvSrcDescr.equals(envsrcdescr, envsrcdescr'), fn () => result,
			  fn _ => raise Fail (concat ["envsrcdescr: ", 
						      EnvSrcDescr.toString envsrcdescr, 
						      " repeated"]))

		   in
		      ()
		   end)

	       val {envsrcdescr, ...} = List.last libs
	       val nodes =
		  case HashSet.peek
		       (exports, EnvSrcDescr.hash envsrcdescr, fn S {envsrcdescr = envsrcdescr', ...} =>
			EnvSrcDescr.equals(envsrcdescr, envsrcdescr')) of
		     NONE => raise Fail "nodes"
		   | SOME (S {syms , ...}) => 
			List.map(!syms,fn (_,_,_,n) => n)
	       val _ =
		  Graph.dfsNodes
		  (g, nodes,
		   Graph.DfsParam.startNode
		   (fn node => (get node) ()))

	       val keep = fn (envsrcdescr, vn) =>
		  Option.isSome
		  (HashSet.peek
		   (keep, Word.xorb(EnvSrcDescr.hash envsrcdescr, String.hash vn), 
		    fn W {lhs = (envsrcdescr',vn'), ...} => 
		    EnvSrcDescr.equals(envsrcdescr, envsrcdescr') andalso
		    vn = vn'))

	       val imports = fn import =>
		  case HashSet.peek
		       (imports, EnvSrcDescr.hash import, fn X {envsrcdescr, ...} =>
			EnvSrcDescr.equals(import, envsrcdescr)) of
		     NONE => []
		   | SOME (X {syms, ...}) => !syms
	    in
	       (keep, imports)
	    end
      end

   fun cmcat {comments, defines, out, sources} =
      let
	 (* Define preprocessor symbols *)
	 val _ = List.foreach(defines, fn sym => 
			      (#set (CM.symval sym)) (SOME 1))
	 val _ = (#set CM.Control.verbose) false
	 val _ = (#set CM.Control.warn_obsolete) false
	 val _ = Control.printWarnings := false
	 val dir = OS.FileSys.getDir ()
	 val env =
	    Option.fold
	    (Option.map(OS.Process.getEnv "HOME", fn h =>
			OS.Path.concat (h, ".cmcat-pathconfig")),
	     CM.SrcPath.penv, fn (f, env) =>
	     CM.SrcPath.processSpecFile {env = env, specfile = f})
	 val _ = List.foreach(CM.Library.known (), CM.Library.unshare)
	 val libs = Closure.topoSortImportGraph (env, sources)
	 val (keep,imports) = Closure.filter libs
      in
	 List.foreach
	 (libs,
	  fn {graph = NONE, envsrcdescr, ...} =>
	  if comments
	     then (Out.output (out, "(* " ^ (EnvSrcDescr.toString envsrcdescr) ^ "\n");
			       List.foreach
			       (imports envsrcdescr, fn (ns,s) =>
			       Out.output (out, " * " ^ (case ns of 
			                                    PG.SGN => "signature " 
	                                                  | PG.STR => "structure " 
	                                                  | PG.FCT => "functor ") ^ 
			                        s ^ "\n"));
			       Out.output (out, " *)\n"))
	     else ()
	   | {graph = SOME {graph, nativesrc, ...}, envsrcdescr, ...} =>
	  (if comments
	      then Out.output (out, 
			       "(* " ^ 
			       (OS.Path.mkRelative {path = EnvSrcDescr.src envsrcdescr, relativeTo = dir}) ^ 
			       " *)\n")
	      else ();
	   let val PG.GRAPH {defs, ...} = graph
	   in 
	      List.foreach
	      (defs, fn def =>
	       case def of
		  PG.DEF {lhs, rhs = PG.COMPILE {src = (src, native), ...}, ...} =>
		     if keep(envsrcdescr,lhs)
			then Out.output(out, (if native then src else nativesrc src) ^ "\n")
			else ()
		| _ => ())
	   end))
      end

   fun die msg =
      (message "Usage: cmcat [-comments] [-Dsym ...] sources.cm"
       ; message ("Error: " ^ msg)
       ; OS.Process.exit OS.Process.failure)

   fun main (_, args) =
      let
	 val comments = ref false
	 val defines = ref ["MLton"]
	 fun loop args = 
	    case args of
	       [file] =>
		  cmcat {comments = !comments,
			 defines = !defines,
			 out = Out.standard,
			 sources = file}
	     | flag :: args =>
		  if String.equals (flag, "-comments")
		     then 
			(comments := true;
			 loop args)
		  else if String.isPrefix {prefix = "-D", string = flag}
		     then
			(defines := String.extract (flag, 2, NONE) :: !defines
			 ; loop args)
		  else die (String.concat ["invalid flag ", flag])
	     | _ => die "wrong number of arguments"
      in
	 loop args handle _ => die "cmcat failed"
	 ; 0
      end

   fun export () =
      SMLofNJ.exportFn
      ("cmcat", main)

end
