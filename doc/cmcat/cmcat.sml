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
 * CMcat.cmcat {defines = [],
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
		source: String.t} -> unit
   val export : unit -> unit
end =
struct
   structure PG = PortableGraph
   structure Graph = DirectedGraph
   structure Node = Graph.Node
   structure Edge = Graph.Edge

   fun message s = Out.output (Out.error, s ^ "\n")

   structure Closure =
      struct
	 structure Import =
	    struct
	       datatype t = Known of String.t | Unknown of CM.Library.lib
	    end

	 fun topoSortImportGraph source =
	    let
	       datatype t = T of {hash: Word.t,
				  graph: {graph: PG.graph,
					  imports: Import.t List.t,
					  nativesrc: String.t -> String.t} option,
				  node: t Node.t,
				  source: String.t}

	       val g : t Graph.t = Graph.new ()
	       val m : t HashSet.t =
		  HashSet.new {hash = fn T {hash, ...} => hash}
	       val {get : t Node.t -> t,
		    set, rem} =
		  Property.getSetOnce
		  (Node.plist, Property.initRaise ("topoSortImportGraph:get", Node.layout))

	       val sources = ref [(source,NONE)];

	       fun closure () =
		  if List.length (!sources) = 0
		     then ()
		     else DynamicWind.withEscape
			  (fn esc =>
			   let
			      val (source,finish) = List.pop sources
			      val hash = String.hash source

			      fun error () =
				 (message ("CM.Graph.graph " ^ source ^ ": failed");
				  esc ())

			      val T {node, ...} =
				 HashSet.lookupOrInsert
				 (m, hash, fn T {source = source', ...} => 
				  String.equals (source, source'),
				  fn () => 
				  (case CM.Graph.graph source of
				      NONE => error ()
				    | SOME {graph, imports, nativesrc, ...} => 
					 let
					    val node = Graph.newNode g
					    val imports =
					       List.map
					       (imports, fn lib =>
						let
						   val descr = CM.Library.descr lib
						   val descr = List.last (String.split(descr, #":"))
						   val source = CM.Library.osstring lib
						in
						   if String.isPrefix {prefix = "$", string = descr}
						   then (let
							    val hash = String.hash descr
							    val T {node = import_node, ...} =
							       HashSet.lookupOrInsert
							       (m, hash, fn T {source, ...} =>
								String.equals (descr, source), 
								fn () =>
								let
								   val node = Graph.newNode g
								   val result =
								      T {graph = NONE,
									 hash = hash,
									 node = node,
									 source = descr}
								   val _ = set(node,result)
								in
								   result
								end)
							 in
							    Graph.addEdge
							    (g, {from = import_node, to = node})
							 end ;
							 Import.Unknown lib)
						   else (let 
							    val finish = fn import_node =>
							       Graph.addEdge
							       (g, {from = import_node, to = node})
							 in 
							    List.push(sources, (source, SOME finish)) ;
							    Import.Known source
							 end)
						end)

					    val result = 
					       T {graph = SOME {graph = graph,
								imports = imports,
								nativesrc = nativesrc},
						  hash = hash,
						  node = node,
						  source = source}
					 in
					    set (node, result) ;
					    result
					 end)
				 handle _ => error ())
			      val _ = Option.map(finish, fn finish => finish node)
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
				  val T {graph, source, ...} = get n
			       in
				  {graph = graph,
				   source = source}
			       end)
			in
			   libs
			end
	    in
	       libs
	    end

	 fun filter (libs : {graph: {graph: PG.graph, 
				     imports: Import.t List.t,
				     nativesrc: String.t -> String.t} option, 
			     source: String.t} List.t) =
	    let
	       datatype t = T of {hash: Word.t,
				  lhs: String.t * PG.varname,
				  syms: (String.t * PG.namespace * String.t * t Node.t) list}
	       val symsNodesDefs : t HashSet.t =
		  HashSet.new {hash = fn T {hash, ...} => hash}

	       datatype s = S of {hash: Word.t,
				  source: String.t,
				  syms: (String.t * PG.namespace * String.t * t Node.t) list ref}
	       val exports : s HashSet.t =
		  HashSet.new {hash = fn S {hash, ...} => hash}

	       val g : t Graph.t = Graph.new ()
	       val {get : t Node.t -> (unit -> unit),
		    set, rem} =
		  Property.getSetOnce
		  (Node.plist, Property.initRaise ("filter:get", Node.layout))

	       datatype w = W of {hash: Word.t,
				  lhs: String.t * PG.varname}
	       val keep : w HashSet.t =
		  HashSet.new {hash = fn W {hash, ...} => hash}
	       val addKeep =
		  fn (source,vn) =>
		  let
		     val hash = Word.xorb(String.hash source, String.hash vn)
		     val result = W {hash = hash,
				     lhs = (source, vn)}
		  in
		     fn () =>
		     (HashSet.insertIfNew
		      (keep, hash, fn W {lhs, ...} =>
		       (source,vn) = lhs, fn () => result,
		       fn _ => raise Fail "keep") ;
		      ())
		  end

	       datatype x = X of {hash: Word.t,
				  source: String.t,
				  syms: (PG.namespace * String.t) list ref}
	       val imports : x HashSet.t =
		  HashSet.new {hash = fn X {hash, ...} => hash}
	       val addImport =
		  fn (descr,ns,s) =>
		  let
		     val hash = String.hash descr
		  in
		     fn () =>
		     let
			val X {syms, ...} =
			   HashSet.lookupOrInsert
			   (imports, hash, fn X {source, ...} =>
			    descr = source, fn () =>
			    X {hash = hash,
			       source = descr,
			       syms = ref []})
		     in
			List.push(syms,(ns,s))
		     end
		  end

	       val _ =
		  List.foreach
		  (libs, 
		   fn {graph = NONE, source, ...} =>
		   ()
		    | {graph = SOME {graph = PG.GRAPH {defs, export, imports}, 
				     imports = imports', ...},
		       source, ...} =>
		   let
		      val source_hash = String.hash source

		      local
			 val imports =
			    List.map2(imports, imports', fn (vn,import) =>
				      (vn,
				       case import of
					  Import.Known source =>
					     let
						val hash = String.hash source
					     in
						case HashSet.peek
						     (exports, hash, fn S {source = source', ...} =>
						      source = source') of
						   NONE => raise Fail "importFn"
						 | SOME (S {syms as envSyms, ...}) => 
						      fn symsSyms =>
						      List.keepAll
						      (!envSyms, fn (source,ns,v,node) =>
						       List.contains(symsSyms,(ns,v),(op =)))
					     end
					| Import.Unknown lib =>
					     let
						val descr = CM.Library.descr lib
						val descr = List.last (String.split(descr, #":"))
						val hash = String.hash descr
						val S {syms, ...} =
						   HashSet.lookupOrInsert
						   (exports, hash, fn S {source, ...} =>
						    descr = source, fn () =>
						    S {hash = hash,
						       source = descr,
						       syms = ref []})
					     in
						fn symsSyms =>
						List.map
						(symsSyms, fn (ns,s) =>
						 case List.peek(!syms, fn (_,ns',s',_) => (ns,s) = (ns',s')) of
						    SOME z => z
						  | NONE => let
							       val node = Graph.newNode g
							       val _ = set(node,addImport (descr,ns,s))
							       val z = (descr,ns,s,node)
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
				   val hash = Word.xorb(source_hash, String.hash lhs)

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
					 lhs = (source, lhs),
					 syms = syms}
				in
				   HashSet.insertIfNew
				   (symsNodesDefs, hash, fn T {lhs = lhs', ...} =>
				    (source,lhs) = lhs', fn () => result,
				    fn _ => raise Fail (concat ["lhs: ", lhs, " violates VARNAME_ONCE"])) ;
				   ()
				end
			   | PG.COMPILE {src, env, syms} => 
				let
				   val hash = Word.xorb(source_hash, String.hash lhs)
				   val envSyms =
				      let val hash = Word.xorb(source_hash, String.hash env)
				      in
					 case HashSet.peek
					      (symsNodesDefs, hash, fn T {lhs, ...} =>
					       (source,env) = lhs) of
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
				   val _ = set(node, addKeep (source, lhs))
				   val _ = 
				      List.foreach
				      (envSyms, fn (_,_,_,node') =>
				       ignore(Graph.addEdge(g, {from = node, to = node'})))
				   val syms =
				      List.map
				      (symsSyms, fn (ns,v) =>
				       (source,ns,v,node))
				   val result = 
				      T {hash = hash,
					 lhs = (source, lhs),
					 syms = syms}
				in
				   HashSet.insertIfNew
				   (symsNodesDefs, hash, fn T {lhs = lhs', ...} =>
				    (source,lhs) = lhs', fn () => result,
				    fn _ => raise Fail (concat ["lhs: ", lhs, " violates VARNAME_ONCE"])) ;
				   ()
				end
			   | PG.FILTER {env, syms} => 
				let
				   val hash = Word.xorb(source_hash, String.hash lhs)
				   val envSyms =
				      let val hash = Word.xorb(source_hash, String.hash env)
				      in
					 case HashSet.peek
					      (symsNodesDefs, hash, fn T {lhs, ...} =>
					       (source,env) = lhs) of
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
				      (envSyms, fn (source,ns,v,node) =>
				       List.contains(symsSyms,(ns,v),(op =)))
				   val result = 
				      T {hash = hash,
					 lhs = (source, lhs),
					 syms = syms}
				in
				   HashSet.insertIfNew
				   (symsNodesDefs, hash, fn T {lhs = lhs', ...} =>
				    (source,lhs) = lhs', fn () => result,
				    fn _ => raise Fail (concat ["lhs: ", lhs, " violates VARNAME_ONCE"])) ;
				   ()
				end
			   | PG.MERGE vns => 
				let
				   val hash = Word.xorb(source_hash, String.hash lhs)
				   val syms =
				      List.foldr
				      (vns, [], fn (vn,symsAcc) =>
				       let val hash = Word.xorb(source_hash, String.hash vn)
				       in
					  case HashSet.peek
					       (symsNodesDefs, hash, fn T {lhs, ...} =>
						lhs = (source,vn)) of
					     NONE => raise Fail (concat ["lhs: ", lhs, " violates ENV_TYPE"])
					   | SOME (T {syms, ...}) => symsAcc @ syms
				       end)
				   val result =
				      T {hash = hash,
					 lhs = (source, lhs),
					 syms = syms}
				in	
				   HashSet.insertIfNew
				   (symsNodesDefs, hash, fn T {lhs = lhs', ...} =>
				    (source,lhs) = lhs', fn () => result,
				    fn _ => raise Fail (concat ["lhs: ", lhs, " violates VARNAME_ONCE"])) ;
				   ()
				end)

		      val exportSyms =
			 let val hash = Word.xorb(source_hash, String.hash export)
			 in
			    case HashSet.peek
			         (symsNodesDefs, hash, fn T {lhs, ...} =>
				  (source,export) = lhs) of
			       NONE => raise Fail (concat ["lhs: ", export, " violates ENV_TYPE"])
			     | SOME (T {syms, ...}) => syms
			 end
		      val result = S {hash = source_hash,
				      source = source,
				      syms = ref exportSyms}
		      val _ =
			 HashSet.insertIfNew
			 (exports, source_hash, fn S {source = source', ...} =>
			  source = source', fn () => result,
			  fn _ => raise Fail (concat ["source: ", source, " repeated"]))

		   in
		      ()
		   end)

	       val {source, ...} = List.last libs
	       val nodes =
		  case HashSet.peek
		       (exports, String.hash source, fn S {source = source', ...} =>
			source = source') of
		     NONE => raise Fail "nodes"
		   | SOME (S {syms , ...}) => 
			List.map(!syms,fn (_,_,_,n) => n)
	       val _ =
		  Graph.dfsNodes
		  (g, nodes,
		   Graph.DfsParam.startNode
		   (fn node => (get node) ()))
		  
	       val keep = fn (source, vn) =>
		  Option.isSome
		  (HashSet.peek
		   (keep, Word.xorb(String.hash source, String.hash vn), 
		    fn W {lhs, ...} => (source, vn) = lhs))

	       val imports = fn descr =>
		  case HashSet.peek
		       (imports, String.hash descr, fn X {source, ...} =>
			descr = source) of
		     NONE => raise Fail "import"
		   | SOME (X {syms, ...}) => !syms

	    in
	       (keep, imports)
	    end
      end

   fun cmcat {comments, defines, out, source} =
      let
	 (* Define preprocessor symbols *)
	 val _ = List.foreach(defines, fn sym => 
			      (#set (CM.symval sym)) (SOME 1))
	 val _ = (#set CM.Control.verbose) false
	 val _ = (#set CM.Control.warn_obsolete) false
	 val dir = OS.FileSys.getDir ()
	 val libs = Closure.topoSortImportGraph
	            (OS.Path.mkAbsolute {path = source, relativeTo = dir})
	 val (keep,imports) = Closure.filter libs
      in
	 List.foreach
	 (libs,
	  fn {graph = NONE, source, ...} =>
	  if comments
	     then (Out.output (out, "(* " ^ source ^ "\n");
			       List.foreach
			       (imports source, fn (ns,s) =>
			       Out.output (out, " * " ^ (case ns of 
			                                    PG.SGN => "signature " 
	                                                  | PG.STR => "structure " 
	                                                  | PG.FCT => "functor ") ^ 
			                        s ^ "\n"));
			       Out.output (out, " *)\n"))
	     else ()
	   | {graph = SOME {graph, nativesrc, ...}, source, ...} =>
	  (if comments
	      then Out.output (out, "(* " ^ (OS.Path.mkRelative {path = source, relativeTo = dir}) ^ " *)\n")
	      else ();
	   let val PG.GRAPH {defs, ...} = graph
	   in 
	      List.foreach
	      (defs, fn def =>
	       case def of
		  PG.DEF {lhs, rhs = PG.COMPILE {src = (src, native), ...}, ...} =>
		     if keep(source,lhs)
			then Out.output(out, (if native then src else nativesrc src) ^ "\n")
			else ()
		| _ => ())
	   end))
      end

   fun die msg =
      (message "Usage: cmcat [-comments] [-Dsym ...] sources.cm"
       ; message ("Error: " ^ msg)
       ; OS.Process.exit OS.Process.failure)

   fun export () =
      SMLofNJ.exportFn
      ("cmcat", fn (_, args) =>
       let
	  val comments = ref false
	  val defines = ref ["MLton"]
	  fun loop args = 
	     case args of
		[file] =>
		   cmcat {comments = !comments,
			  defines = !defines,
			  out = Out.standard,
			  source = file}
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
       end)

end
