(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(*
 * This pass is based on
 * Contification Using Dominators, by Fluet and Weeks.  ICFP 2001.
 *)

functor Contify (S: CONTIFY_STRUCTS): CONTIFY = 
struct

open S
open Dec Transfer

(* Return = {Uncalled, Unknown} U Jump U Func
 *)
structure Return =
  struct
    datatype t
      = Uncalled
      | Unknown
      | JumpLabel of Jump.t
      | FuncLabel of Func.t

    fun layout r
      = let 
	  open Layout
	in
	  case r
	    of Uncalled => str "Uncalled"
	     | Unknown => str "Unknown"
	     | JumpLabel j => seq [Jump.layout j]
	     | FuncLabel f => seq [Func.layout f]
	end
    val toString = Layout.toString o layout
      
    val equals
      = fn (Uncalled, Uncalled) => true
         | (Unknown, Unknown) => true
	 | (JumpLabel j1, JumpLabel j2) => Jump.equals (j1, j2)
	 | (FuncLabel f1, FuncLabel f2) => Func.equals (f1, f2)
	 | _ => false

    val isUncalled 
      = fn Uncalled => true
         | _ => false
    val isUnknown
      = fn Unknown => true
         | _ => false
  end

datatype status = TODO | CURRENT | DONE

structure JumpData =
  struct
    datatype t = T of {node: DirectedGraph.Node.t option ref,
		       inside: bool ref,
		       rootEdge: bool ref,
		       status: status ref,
		       prefixes: Func.t list ref}
      
    fun new () = T {node = ref NONE,
		    inside = ref false,
		    rootEdge = ref false,
		    status = ref TODO,
		    prefixes = ref []}

    local
      fun make s = let
		     fun S' (T r) = s r
		     val S = ! o S'
		   in
		     (S', S)
		   end
    in
      val (node', node) = make #node
      val (inside', inside) = make #inside
      val (rootEdge', rootEdge) = make #rootEdge
      val (status', status) = make #status
      val (prefixes', prefixes) = make #prefixes
    end
    fun nodeReset (T {node, ...}) = node := NONE
  end

structure FuncData =
  struct
    datatype t = T of {node: DirectedGraph.Node.t option ref,
		       reach: bool ref,
		       callers: {nontail: (Func.t * Jump.t) list ref,
				 tail: Func.t list ref},
		       callees: {nontail: (Func.t * Jump.t) list ref,
				 tail: Func.t list ref},
		       A: Return.t ref,
		       contify: bool ref,
		       status: status ref,
		       prefixes: Func.t list ref,
		       replace: {args: (Var.t * Type.t) vector,
				 body: Exp.t,
				 jump: Jump.t} option ref}
      
    fun new () = T {node = ref NONE,
		    reach = ref false,
		    callers = {nontail = ref [], tail = ref []},
		    callees = {nontail = ref [], tail = ref []},
		    A = ref Return.Uncalled,
		    contify = ref true,
		    status = ref TODO,
		    prefixes = ref [],
		    replace = ref NONE}
      
    local
      fun make s = let
		     fun S' (T r) = s r
		     val S = ! o S'
		   in
		     (S', S)
		   end
      fun make' s = let
		      fun S' (T r) = s r
		    in
		      S'
		    end
    in
      val (node', node) = make #node
      val (reach', reach) = make #reach
      val callers' = make' #callers
      val callees' = make' #callees
      val (A', A) = make #A
      val (contify', contify) = make #contify
      val (status', status) = make #status
      val (prefixes', prefixes) = make #prefixes
      val (replace', replace) = make #replace
    end
    fun nodeReset (T {node, ...}) = node := NONE
  end

structure JumpFuncGraph =
  struct
    structure Graph = DirectedGraph
    structure Node = Graph.Node
    structure Edge = Graph.Edge
    structure DfsParam = Graph.DfsParam

    datatype t = JumpNode of Jump.t
               | FuncNode of Func.t
    fun newJumpFuncGraph {getJumpData: Jump.t -> JumpData.t,
			  getFuncData: Func.t -> FuncData.t}
      = let
	  val G = Graph.new ()

	  fun addEdge edge
	    = (Graph.addEdge (G, edge); ())

	  fun addEdge' edge
	    = if Node.hasEdge edge
		then ()
		else addEdge edge

	  val {get = getNodeInfo : Node.t -> t, 
	       set = setNodeInfo}
	    = Property.getSetOnce
	      (Node.plist,
	       Property.initRaise ("nodeInfo", Node.layout))

	  fun getFuncNode f
	    = let
		val node = FuncData.node' (getFuncData f)
	      in
		case !node
		  of SOME n => n
		   | NONE => let
			       val n = Graph.newNode G
			     in
			       setNodeInfo (n, FuncNode f);
			       node := SOME n;
			       n
			     end
	      end

	  fun getJumpNode j
	    = let
		val node = JumpData.node' (getJumpData j)
	      in
		case !node
		  of SOME n => n
		   | NONE => let
			       val n = Graph.newNode G
			     in
			       setNodeInfo (n, JumpNode j);
			       node := SOME n;
			       n
			     end
	      end

	  fun reset p
	    = Graph.foreachNode
	      (G,
	       fn n => if p n
			 then case getNodeInfo n
				of JumpNode j
				 => JumpData.nodeReset (getJumpData j)
		                 | FuncNode f 
			         => FuncData.nodeReset (getFuncData f)
			 else ())
	in
	  {G = G, 
	   addEdge = addEdge, 
	   addEdge' = addEdge',
	   getNodeInfo = getNodeInfo, 
	   getJumpNode = getJumpNode, 
	   getFuncNode = getFuncNode,
	   reset = reset}
	end

    fun newFuncGraph {getFuncData: Func.t -> FuncData.t}
      = let
	  val {G, addEdge, addEdge',
	       getNodeInfo, 
	       getJumpNode, getFuncNode, 
	       reset}
	    = newJumpFuncGraph {getJumpData = fn _ => Error.bug "newFuncGraph",
				getFuncData = getFuncData}
	in
	  {G = G,
	   addEdge = addEdge,
	   addEdge' = addEdge',
	   getNodeInfo = fn n => case getNodeInfo n
				   of FuncNode f => f
				    | JumpNode j => Error.bug "newFuncGraph",
	   getFuncNode = getFuncNode,
	   reset = reset}
	end
  end

structure InitReachCallersCallees =
  struct
    structure Graph = DirectedGraph
    structure DfsParam = Graph.DfsParam

    (* Define Reach: Func -> Bool as follows:
     *  Reach (f) iff there is a path of calls from fm to f.
     *
     * Define NontailCallers: Func -> P (Func x Jump) as follows:
     *  NontailCallers (f) = {(g, j) | (g, f, J) in N}
     * Define TailCallers: Func -> P (Func) as follows:
     *  Callers (f) = {g | (g, f) in T}
     * Define NontailCallees: Func -> P (Func x Jump) as follows:
     *  NontailCallers (f) = {(g, j) | (f, g, J) in N}
     * Define TailCallees: Func -> P (Func) as follows:
     *  Callers (f) = {g | (f, g) in T}
     *
     * Precondition: forall f in Func. (FuncData.node o getFuncData) f = NONE
     *               forall f in Func. (JumpData.callers o getFuncData) f 
     *                                 = {nontail = [], tail = []}
     *               forall f in Func. (JumpData.callees o getFuncData) f
     *                                 = {nontail = [], tail = []}
     * Postcondition: FuncData.reach o getFuncData = Reach
     *                #nontail (FuncData.callers o getFuncData)
     *                = NontailCallers
     *                #tail (FuncData.callers o getFuncData)
     *                = TailCallers
     *                #nontail (FuncData.callees o getFuncData)
     *                = NontailCallees
     *                #tail (FuncData.callees o getFuncData)
     *                = TailCallees
     *)
    fun initReachCallersCallees 
        {program as Program.T {functions, main = fm, ...},
	 getFuncData: Func.t -> FuncData.t} : unit
      = let
	  val {G, addEdge, addEdge',
	       getNodeInfo, getFuncNode, 
	       reset}
	    = JumpFuncGraph.newFuncGraph {getFuncData = getFuncData}

	  val _ 
	    = Vector.foreach
	      (functions, 
	       fn Function.T {name = f, body = f_body, ...}
	        => let
		     val callees = FuncData.callees' (getFuncData f)
		     val f_node = getFuncNode f
		   in
		     Exp.foreachCall
		     (f_body,
		      fn {func = g, cont, ...}
		       => let
			    val callers = FuncData.callers' (getFuncData g)
			    val g_node = getFuncNode g
			  in
			    case cont
			      of NONE 
			       => (List.push (#tail callees, g);
				   List.push (#tail callers, f))
			       | SOME j 
			       => (List.push (#nontail callees, (g, j));
				   List.push (#nontail callers, (f, j)));
			    addEdge {from = f_node,
				     to = g_node}
			  end)
		   end)
	      
	  val dfs_param
	    = DfsParam.finishNode
	      (fn n => let
			 val reach 
			   = FuncData.reach' (getFuncData (getNodeInfo n))
		       in
			 reach := true
		       end)
	  val fm_node = getFuncNode fm
	in
	  Graph.dfsNodes (G, [fm_node], dfs_param);
	  reset (fn _ => true)
	end
    val initReachCallersCallees 
      = Control.trace (Control.Pass, "initReachCallerCallees") initReachCallersCallees 
  end

structure AnalyzeDom =
  struct
    structure Graph = DirectedGraph
    structure Node = Graph.Node

    (* Now define a directed graph G = (Node, Edge) where
     *      Node = Jump U Fun U {Root}
     *      Edge = {(Root, fm)}
     *             U {(Root, j) | j in Jump}
     *             U {(Root, f) | not (Reach (f))}
     *             U {(f, g) | (f, g) in T and Reach (f)}
     *             U {(j, g) | (f, g, j) in N and Reach (f)}
     *
     * Let D be the dominator tree of G rooted at Root.
     * For f in Fun, let idom (f) be the parent of f in D.
     *
     * Define an analysis, A_Dom, based on D as follows:
     *      A_Dom (f) = 
     *           if idom (f) = Root
     *             then if Reach (f) then Unknown else Uncalled
     *             else the ancestor g of f in D such that idom (g) = Root
     *
     * Precondition: forall j in Jump. (JumpData.node o getJumpData) j = NONE
     *               forall j in Jump. (JumpData.rootEdge o getJumpData) j = false
     *               forall f in Func. (FuncData.node o getFuncData) f = NONE
     *               forall f in Func. (FuncData.reach o getFuncData) f = Reach
     * Postcondition: FuncData.ADom o getFuncData = A_Dom
     *                forall j in Jump. (JumpData.node o getJumpData) j = NONE
     *                forall f in Func. (FuncData.node o getFuncData) f = NONE
     *)
    fun analyzeDom {program as Program.T {functions, main = fm, ...},
		    getFuncData: Func.t -> FuncData.t,
		    getJumpData: Jump.t -> JumpData.t} : unit
      = let
	  datatype z = datatype Return.t

	  val {G, addEdge, addEdge',
	       getNodeInfo, getFuncNode, getJumpNode, 
	       reset}
	    = JumpFuncGraph.newJumpFuncGraph {getJumpData = getJumpData,
					      getFuncData = getFuncData}
	  val Root = DirectedGraph.newNode G

	  fun buildGraph () = let
	  val fm_node = getFuncNode fm
	  (* {(Root, fm)} *)
	  val _ = addEdge {from = Root, to = fm_node}

	  val _
	    = Vector.foreach
	      (functions,
	       fn Function.T {name = f, body = f_body, ...}
	        => let
		     val f_reach = FuncData.reach (getFuncData f)
		     val f_node = getFuncNode f
		   in
		     if f_reach
		       then Exp.foreach
			    (f_body,
			     {handleDec
			      = fn Fun {name = j, ...}
			         => let
				      val inside = JumpData.inside' (getJumpData j)
				    in
				      inside := true;
				      fn () => inside := false
				    end
				 | _ => ignore,
			      handleTransfer
			      = fn Call {func = g, cont, ...}
			         => if FuncData.reach (getFuncData g)
				      then let
					     val g_node = getFuncNode g
					   in
					     case cont
					       of NONE
						=> (* {(f, g) | (f, g) in T
						    *       and Reach (f)} *)
						   addEdge {from = f_node,
							    to = g_node}
						| SOME j
						=> let
						     val j_node = getJumpNode j
						     val inside
						       = JumpData.inside
						         (getJumpData j)
						     val rootEdge
						       = JumpData.rootEdge'
						         (getJumpData j)
						   in
						     if !rootEdge
						       then ()
						       else ((* {(Root, j) | j in Jump} *)
							     addEdge {from = Root,
								      to = j_node};
							     rootEdge := true);
						     if inside
						       then addEdge {from = Root,
								     to = g_node}
						       else (* {(j, g) | (f, g, j) in N
							             and Reach (f)} *)
							    addEdge {from = j_node,
								     to = g_node}
						   end
					   end
				      else ()
				 | _ => ()})
		       else (* {(Root, f) | not (Reach (f))} *)
			    addEdge {from = Root,
				     to = f_node}
		   end)

(*
	  val _
	    = Vector.foreach
	      (functions,
	       fn Function.T {name = f, body = f_body, ...}
	        => let
		     val f_reach = FuncData.reach (getFuncData f)
		     val f_node = getFuncNode f
		   in
		     if f_reach
		       then Exp.foreachCall
			    (f_body,
			     fn {func = g, cont, ...}
			      => if FuncData.reach (getFuncData g)
				   then let
					  val g_node = getFuncNode g
					in
					  case cont
					    of NONE
					     => (* {(f, g) | (f, g) in T 
						 *       and Reach (f)} *)
					        addEdge {from = f_node,
							 to = g_node}
					     | SOME j
					     => let
						  val j_node = getJumpNode j
						  val rootEdge 
						    = JumpData.rootEdge'
						      (getJumpData j)
						in
						  if !rootEdge
						    then ()
						    else ((* {(Root, j) | j in Jump} *)
						          addEdge {from = Root,
								   to = j_node};
							  rootEdge := true);
						  (* {(j, g) | (f, g, j) in N
						   *       and Reach (f)} *)
						  addEdge {from = j_node,
							   to = g_node}
						end
					end
				   else ())
		       else (* {(Root, f) | not (Reach (f))} *)
			    addEdge {from = Root,
				     to = f_node}
		   end)
*)
          in () end
	  val buildGraph 
	    = Control.trace (Control.Pass, "buildGraph") buildGraph
	  val _ = buildGraph ()

	  fun computeDominators () = let
	  val {idom} = Graph.dominators (G, {root = Root})
          in idom end
	  val computeDominators 
	    = Control.trace (Control.Pass, "computeDominators") computeDominators
	  val idom = computeDominators ()

	  fun computeADom () = let
	  fun ancestor node
	    = let
		val parent = idom node
	      in
		if Node.equals (parent, Root)
		  then node
		  else ancestor parent
	      end

          val _
	    = Vector.foreach
	      (functions,
	       fn Function.T {name, ...}
	        => let
		     val FuncData.T {A, reach, node, ...} =
			getFuncData name
		     val f_ADom = A
		     val f_reach = !reach
		     val f_node = valOf (!node)
		     datatype z = datatype JumpFuncGraph.t
		   in
		     if Node.equals (idom f_node, Root)
		       then if f_reach
			      then f_ADom := Unknown
			      else f_ADom := Uncalled
		       else let
			      (* Use this for the ancestor version *)
                              val l_node = ancestor f_node
			      (* Use this for the parent version *)
			      (* val l_node = idom f_node *)
			      val l = getNodeInfo l_node
			    in
			      case getNodeInfo l_node
				of FuncNode g => f_ADom := FuncLabel g
				 | JumpNode j => f_ADom := JumpLabel j
			    end
		   end)
	  in () end
	  val computeADom 
	    = Control.trace (Control.Pass, "compute ADom") computeADom
	  val _ = computeADom ()

	  val _ = reset (fn n => not (Node.equals (n, Root)))
	in
	  ()
	end
    val analyzeDom 
      = Control.trace (Control.Pass, "analyzeDom") analyzeDom
end

structure Transform =
  struct

    structure Graph = DirectedGraph
    structure Node = Graph.Node
    structure Edge = Graph.Edge
    structure DfsParam = Graph.DfsParam

    (*
     * Precondition: forall j in Jump. (JumpData.node o getJumpData) j = NONE
     *               forall j in Jump. (JumpData.status o getJumpData) j = TODO
     *               forall j in Jump. (JumpData.prefixes o getJumpData) j = []
     *               forall f in Func. (FuncData.node o getFuncData) f = NONE
     *               FuncData.A o getFuncData = A
     *                where A is a safe analysis
     *               FuncData.callers o getFuncData
     *               = {nontail = NontailCallers, tail = TailCallers}
     *               FuncData.callees o getFuncData
     *               = {nontail = NontailCallees, tail = TailCallees}
     *               forall f in Func. (JumpData.status o getFuncData) f = TODO
     *               forall f in Func. (JumpData.prefixes o getFuncData) f = []
     *               forall f in Func. (JumpData.replace o getFuncData) f = NONE
     * Postcondition: forall j in Jump. (JumpData.node o getJumpData) j = NONE
     *                forall f in Func. (FuncData.node o getFuncData) f = NONE
     *)
    fun transform {program as Program.T {datatypes, globals, 
					 functions, main},
		   getFuncData: Func.t -> FuncData.t,
		   getJumpData: Jump.t -> JumpData.t} : Program.t
      = let
	  datatype z = datatype Return.t

	  val {G, addEdge, addEdge',
	       getNodeInfo, getJumpNode, getFuncNode, 
	       reset}
	    = JumpFuncGraph.newJumpFuncGraph {getFuncData = getFuncData,
					      getJumpData = getJumpData}

	  val roots = ref []
	  val _ 
	    = Vector.foreach
	      (functions,
	       fn Function.T {name = f, ...}
	        => let
		     val FuncData.T {A, contify, callers, callees, ...} 
		       = getFuncData f
		     val f_node = getFuncNode f

		     val _ = Control.diagnostics
		             (fn display 
			       => let open Layout
				  in
				    display (seq [Func.layout f,
						  str ": ",
						  Return.layout (!A)])
				  end)
		   in
		     case !A
		       of Uncalled => (List.push (roots, f_node);
				       contify := false)
			| Unknown => (List.push (roots, f_node);
				      contify := false)
		        | JumpLabel j
			=> let
			     val l_node = getJumpNode j
			   in
			     List.push (roots, l_node);
			     contify := true;
			     addEdge {from = l_node,
				      to = f_node}
			   end
		        | FuncLabel g
			=> let
			     val l_node = getFuncNode g
			   in
			     contify := true;
			     addEdge {from = l_node,
				      to = f_node}
			   end
		   end)
	  val _ = reset (fn _ => true)

	  fun cascade fs
	    = let
		fun doit f
		  = let
		      val FuncData.T {callees, prefixes, ...} = getFuncData f
		    in
		      List.foreach
		      (!(#tail callees),
		       fn g => let
				 val FuncData.T {contify, status, ...} = getFuncData g
			       in
				 if !status <> DONE
				   then (contify := false;
					 status := DONE;
					 doit g)
				   else ()
			       end) ;
		      List.foreach(!prefixes, doit)
		    end
	      in
		List.foreach
		(fs,
		 fn f => let
			   val FuncData.T {contify, status, ...} = getFuncData f
			 in 
			   contify := false;
			   status := DONE;
			   doit f
			 end)
	      end

	  (* Strongly connected components of a group of functions. *)
	  fun getSCCS (fs: Func.t list) : Func.t list list
	    = let
		val {G, addEdge, addEdge',
		     getNodeInfo, getFuncNode, 
		     reset}
		  = JumpFuncGraph.newFuncGraph {getFuncData = getFuncData}

		val _ 
		  = List.foreach
		    (fs,
		     fn f => let
			       val f_node = getFuncNode f

			       fun doit g
				 = let
				     val FuncData.T {callees, prefixes, ...}
				       = getFuncData g
				       
				     fun doit' h
				       = if List.contains (fs, h, Func.equals)
					   then let
						  val h_node = getFuncNode h
						in
						  addEdge {from = h_node,
							   to = f_node}
						end
					   else ()
				   in
				     List.foreach
				     (!(#nontail callees), 
				      fn (h,_) => doit' h);
				     List.foreach
				     (!(#tail callees), 
				      fn h => doit' h);
				     List.foreach
				     (!prefixes,
				      fn g' => doit g')
				   end
			     in
			       doit f
			     end)

		val sccs = Graph.stronglyConnectedComponents G
		val _ = reset (fn _ => true)
	      in
		List.revMap (sccs, fn scc => List.revMap (scc, getNodeInfo))
	      end

	  exception Nope
	  val nested = ref 0
	  val rejected = ref 0
	  fun prefixSCCSHeads (prefix, fs)
	    = let
	      in
		List.foreach
		(getSCCS fs,
		 fn fs as [] => ()
		  | fs as [f] 
		  => let
		       val FuncData.T {contify, status, ...} = getFuncData f
		     in
		       status := DONE;
		       if !contify
			 then prefix f
			 else ()
		     end
		  | fs
		  => let
		       val outsideCallee = ref NONE
		       val insideFunctions
			 = let
			     fun doit f
			       = let
				   val prefixes = FuncData.prefixes (getFuncData f)
				 in 
				   f :: List.concatMap (prefixes, doit)
				 end
			   in
			     List.concatMap (fs, doit)
			   end
		       fun check g
			 = not (List.contains (insideFunctions, g, Func.equals))
		     in
		       List.foreach
		       (fs,
			fn f => let
				  val callers = FuncData.callers' (getFuncData f)
				in
				  if List.exists(!(#nontail callers), 
						 fn (g, _) => check g)
				     orelse
				     List.exists(!(#tail callers),
						 fn g => check g)
				    then case !outsideCallee
					   of NONE => outsideCallee := SOME f
					    | SOME _ => raise Nope
				    else ()
				end) ;
		       case !outsideCallee
			 of NONE => Error.bug "getSCCSHeads"
			  | SOME f
			  => let
			       val FuncData.T {contify, status, prefixes, ...}
				 = getFuncData f
			       val rest 
				 = List.removeFirst(fs, fn g => Func.equals (f, g))
			     in
			       nested := !nested + (List.length rest);
			       if !contify
				 then (prefixSCCSHeads
				       (fn f => List.push(prefixes, f), rest);
				       status := DONE;
				       if !contify
					 then prefix f
					 else ())
				 else ()
			     end
		     end
		     handle Nope
		     => let
			in
			  rejected := !rejected + (List.length fs);
			  cascade fs
			end)
	      end

	  datatype z = datatype JumpFuncGraph.t
	  fun processNode n
	    = let
		val succs = List.map (Node.successors n, Edge.to)
		val _ 
		  = Assert.assert
		    ("Transform.transform: processNode",
		     fn () 
		      => List.forall
		         (succs,
			  fn n 
			   => case getNodeInfo n
				of JumpNode j
				 => JumpData.status (getJumpData j) = CURRENT
			         | FuncNode f
				 => FuncData.status (getFuncData f) = CURRENT
				    orelse
				    (FuncData.status (getFuncData f) = DONE
				     andalso
				     FuncData.contify (getFuncData f) = false)))

		val succs = List.keepAllMap
		            (succs,
			     fn n => case getNodeInfo n
				       of JumpNode j => NONE
				        | FuncNode f 
					=> if FuncData.contify (getFuncData f)
					     then SOME f
					     else NONE)

		fun doit (prefixes, status)
		  = let
		    in
		      prefixSCCSHeads(fn f => List.push(prefixes, f), succs);
		      Assert.assert
		      ("Transform.transform: processNode",
		       fn () 
		        => List.forall
		           (succs,
			    fn f
			     => FuncData.status (getFuncData f) = DONE));
		      if !status <> DONE
			then status := CURRENT
			else ()
		    end
	      in 
		case getNodeInfo n
		  of JumpNode j 
		   => let
			val JumpData.T {prefixes, status, ...} = getJumpData j
		      in 
			doit (prefixes, status)
		      end 
		   | FuncNode f
		   => let
			val FuncData.T {prefixes, status, ...} = getFuncData f
		      in 
			doit (prefixes, status)
		      end
	      end

	  val dfs_param = DfsParam.finishNode processNode
	  val _ = Graph.dfsNodes (G, !roots, dfs_param)

	  (* For functions turned into continuations,
	   *  record their args, body, and new name.
	   *)
	  val _ 
	    = Vector.foreach
	      (functions,
	       fn Function.T {name = f, args = f_args, body = f_body, ...} 
	        => let
		     val FuncData.T {contify, replace, prefixes, ...} = getFuncData f
		     val _ 
		       = Assert.assert
		         ("Transform.transform: DONE or CURRENT",
			  fn () => FuncData.status (getFuncData f) = DONE
                                   orelse
				   FuncData.status (getFuncData f) = CURRENT)
		   in
		     if !contify
		       then let
			      val jump = Jump.newString (Func.originalName f)
			      val _ = Control.diagnostics
				      (fn display 
				        => let open Layout
					   in display (seq [Func.layout f,
							    str " -> ",
							    Jump.layout jump])
					   end)
			    in 
			      replace := SOME {jump = jump,
					       args = f_args,
					       body = f_body}
					       
			    end
		       else ()
		   end)

	  (* Walk over all functions, removing those that aren't top level,
	   *  and descening those that are, inserting local functions
	   *  where necessary.
	   * - turn tail calls into nontail calls
	   * - turn returns into jumps
	   *)
	  fun walkExp (f: Func.t, e: Exp.t, c: Jump.t option): Exp.t 
	    = let
		val {decs, transfer} = Exp.dest e
		val decs
		  = List.fold
		    (List.rev decs,
		     [],
		     fn (d, ds)
		      => case d 
			   of Bind _ => d :: ds
			    | Fun {name = j, args, body} 
			    => Fun {name = j,
				    args = args,
				    body = walkExp (f, body, c)} ::
                               let
				 val prefixes
				   = JumpData.prefixes (getJumpData j)
			       in
				 prefix_dec (prefixes, SOME j, ds)
			       end
			    | HandlerPush h => HandlerPush h :: ds
			    | HandlerPop => HandlerPop :: ds)

		fun make transfer = Exp.make {decs = decs, transfer = transfer}
	      in
		case transfer
		  of Call {func, args, cont} 
		   => let
			val replace = FuncData.replace (getFuncData func)
		      in
			case replace
			  of NONE => make (Call {func = func,
						 args = args,
						 cont = case cont
							  of NONE => c
							   | SOME _ => cont})
			   | SOME {jump, ...}
			   => make (Jump {dst = jump, args = args})
		      end
		   | Return xs
		   => make (case c
			      of NONE => transfer
			       | SOME c => Jump {dst = c, args = xs})
		   | _ => make transfer
	      end
	  and prefix_dec (fs: Func.t list, 
			  c: Jump.t option, 
			  ds: Dec.t list) : Dec.t list
	    = List.fold
	      (List.rev fs,
	       ds,
	       fn (f, ds)
	        => let
		     val FuncData.T {replace, prefixes, ...}
		       = getFuncData f
		     val {jump, args, body} 
		       = valOf (!replace)
		     val body = prefix_exp (!prefixes, c, walkExp (f, body, c))
		   in
		     Fun {name = jump, args = args, body = body} :: ds
		   end)
	  and prefix_exp (fs: Func.t list,
			  c: Jump.t option,
			  e: Exp.t) : Exp.t
	    = let
		val {decs, transfer} = Exp.dest e
	      in
		Exp.make {decs = prefix_dec (fs, c, decs),
			  transfer = transfer}
	      end

	  val shrinkExp = shrinkExp globals
	  val functions 
	    = Vector.fromList
	      (Vector.foldr
	       (functions, 
		[], 
	        fn (Function.T {name = f, args = f_args, 
				body = f_body, returns = f_returns}, 
		    functions)
	         => let
		      val FuncData.T {A, replace, prefixes, ...} 
			= getFuncData f
		    in
		      case !replace
			of NONE
			 => if Return.isUncalled (!A)
			      then functions
			      else let
				     val f_body 
				       = prefix_exp (!prefixes, 
						     NONE, 
						     walkExp (f, f_body, NONE))
				     val f_body' = shrinkExp f_body
				   in
				      Function.T {name = f,
						  args = f_args,
						  body = f_body',
						  returns = f_returns}
				      :: functions
				   end
			 | _ => functions
		    end))
	  val program 
	    = Program.T {datatypes = datatypes,
			 globals = globals,
			 functions = functions,
			 main = main}
	in
	  program
	end
    val transform 
      = Control.trace (Control.Pass, "transform") transform
  end

fun contify (program as Program.T {functions, ...})
  = let
      val {get = getFuncData : Func.t -> FuncData.t}
	= Property.get (Func.plist,
			Property.initFun
			(fn _ => FuncData.new ()))
      val {get = getJumpData : Jump.t -> JumpData.t}
	= Property.get (Jump.plist,
			Property.initFun
			(fn _ => JumpData.new ()))

      val _ = InitReachCallersCallees.initReachCallersCallees 
	      {program = program,
	       getFuncData = getFuncData}
      val _ = AnalyzeDom.analyzeDom 
	      {program = program,
	       getFuncData = getFuncData,
	       getJumpData = getJumpData}
      val program = Transform.transform 
	            {program = program,
		     getFuncData = getFuncData,
		     getJumpData = getJumpData}
      val _ = Program.clear program
    in
      program
    end
end
