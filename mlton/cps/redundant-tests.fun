functor RedundantTests (S: REDUNDANT_TESTS_STRUCTS): REDUNDANT_TESTS = 
struct

open S

type int = Int.t

structure Rel =
   struct
      datatype t = EQ | LT | LE | NE

      val equals: t * t -> bool = op =

      val toString =
	 fn EQ => "="
	  | LT => "<"
	  | LE => "<="
	  | NE => "<>"

      val layout = Layout.str o toString
   end

structure Oper =
   struct
      datatype t =
	 Const of Const.t
       | Var of Var.t

      val layout =
	 fn Const c => Const.layout c
	  | Var x => Var.layout x

      val zero = Const (Const.fromInt 0)
      val equals =
	 fn (Const c, Const c') => Const.equals (c, c')
	  | (Var x, Var x') => Var.equals (x, x')
	  | _ => false
   end

structure Fact =
   struct
      datatype t = T of {rel: Rel.t,
			 lhs: Oper.t,
			 rhs: Oper.t}

      fun layout (T {rel, lhs, rhs}) =
	 let open Layout
	 in seq [Oper.layout lhs, str " ", Rel.layout rel,
		 str " ", Oper.layout rhs]
	 end

      fun equals (T {rel, lhs = l, rhs = r},
		  T {rel = rel', lhs = l', rhs = r'}) =
	 Rel.equals (rel, rel')
	 andalso Oper.equals (l, l')
	 andalso Oper.equals (r, r')

      fun negate (T {rel, lhs, rhs}): t =
	 let
	    datatype z = datatype Rel.t
	    val rel =
	       case rel of
		  EQ => NE
		| LT => LE
		| LE => LT
		| NE => EQ
	 in
	    T {rel = rel, lhs = rhs, rhs = lhs}
	 end

      datatype result = False | True | Unknown
      fun determine (facts: t list, f: t): result =
	 if List.contains (facts, f, equals)
	    then True
	 else if List.contains (facts, negate f, equals)
		 then False
	      else Unknown
   end

open Dec PrimExp Transfer

structure Graph = DirectedGraph
structure Edge = Graph.Edge
structure Node = Graph.Node

structure NodeInfo =
   struct
      type t = {(* Youngest ancestor in dominator tree with nonempty facts. *)
		ancestor: Node.t option ref,
		(* Children in dominator tree. *)
		children: Node.t list ref,
		facts: Fact.t list ref,
		(* Number of predecessors in control-flow graph. *)
		numPreds: int ref}

      fun new (): t =
	 {ancestor = ref NONE,
	  children = ref [],
	  facts = ref [],
	  numPreds = ref 0}

      val dummy: t = new ()
   end

fun simplify (program as Program.T {globals, datatypes, functions, main}) =
   let
      datatype varInfo =
	 Const of Const.t
       | Fact of Fact.t
       | None
       | Or of Fact.t * Fact.t
      val {get = varInfo: Var.t -> varInfo, set = setVarInfo, ...} =
	 Property.getSetOnce (Var.plist, Property.initConst None)
      datatype z = datatype Fact.result
      datatype z = datatype Rel.t
      fun makeVarInfo {prim, args, targs, info}: varInfo =
	 let
	    fun arg i =
	       let
		  val x = Vector.sub (args, i)
	       in
		  case varInfo x of
		     Const c => Oper.Const c
		   | _ => Oper.Var x
	       end
	    fun z (r, a, b) =
	       Fact (Fact.T {rel = r,
			     lhs = arg a,
			     rhs = arg b})
	    fun doit rel = z (rel, 0, 1)
	    fun doit' rel = z (rel, 1, 0)
	    datatype z = datatype Prim.Name.t
	 in
	    case Prim.name prim of
	       Char_gt => doit' LT
	     | Char_ge => doit' LE
	     | Char_lt => doit LT
	     | Char_le => doit LE
	     | Int_gt => doit' LT
	     | Int_ge => doit' LE
	     | Int_geu =>
		  Or (Fact.T {rel = LT,
			      lhs = arg 0,
			      rhs = Oper.zero},
		      Fact.T {rel = LE,
			      lhs = arg 1,
			      rhs = arg 0})
	     | Int_gtu =>
		  Or (Fact.T {rel = LT,
			      lhs = arg 0,
			      rhs = Oper.zero},
		      Fact.T {rel = LT,
			      lhs = arg 1,
			      rhs = arg 0})
	     | Int_lt => doit LT
	     | Int_le => doit LE
	     | MLton_eq => doit EQ
	     | Word32_ge => doit' LE
	     | Word32_gt => doit' LT
	     | Word32_le => doit LE
	     | Word32_lt => doit LT
	     | Word8_ge => doit' LE
	     | Word8_gt => doit' LT
	     | Word8_le => doit LE
	     | Word8_lt => doit LT
	     | _ => None
	 end
      fun setConst (x, c) = setVarInfo (x, Const c)
      val _ =
	 Vector.foreach (globals, fn {var, exp, ...} =>
			 case exp of
			    PrimExp.Const c => setConst (var, c)
			  | _ => ())
      val jumpHandlers = inferHandlers program
      fun conVar c =
	 let
	    val x = Var.newNoname ()
	 in
	    {var = x, ty = Type.bool,
	     exp = ConApp {con = c, args = Vector.new0 ()}}
	 end
      val t as {var = trueVar, ...} = conVar Con.truee
      val f as {var = falseVar, ...} = conVar Con.falsee
      val globals = Vector.concat [Vector.new2 (t, f), globals]
      val shrink = shrinkExp globals
      val numSimplified = ref 0
      fun simplifyFunction (f as Function.T {name, args, body, returns}) =
	  let
	     val _ =
		Control.diagnostic
		(fn () => 
		 let open Layout
		 in seq [str "processing ", Func.layout name]
		 end)
	     val {graph, root, jumpNode} =
		Function.controlFlowGraph (f, jumpHandlers)
	     val {idom} = DirectedGraph.dominators (graph, {root = root})
	     val {get = nodeInfo: Node.t -> NodeInfo.t, ...} =
		Property.get
		(Node.plist, Property.initFun (fn _ => NodeInfo.new ()))
	     val jumpInfo = nodeInfo o jumpNode
	     val _ =
		List.foreach
		(Graph.nodes graph, fn n =>
		 let
		    val _ =
		       case idom n of
			  Graph.Idom n' => List.push (#children (nodeInfo n'), n)
			| _ => ()
		    val _ =
		       List.foreach (Node.successors n, fn e =>
				     Int.inc (#numPreds (nodeInfo (Edge.to e))))
		 in
		    ()
		 end)
	     fun loopTransfer (t, n: Node.t) =
		case t of
		   Case {test, cases, default, ...} =>
		      let
			 fun falseTrue () =
			    case cases of
			       Cases.Con v =>
				  let
				     fun ca i = Vector.sub (v, i)
				  in
				     case (Vector.length v, default) of
					(1, SOME j) =>
					   let
					      val (c, j') = ca 0
					   in
					      if Con.equals (c, Con.truee)
						 then (j, j')
					      else (j', j)
					   end
				      | (2, NONE) =>
					   let
					      val (c, j) = ca 0
					      val (_, j') = ca 1
					   in
					      if Con.equals (c, Con.truee)
						 then (j', j)
					      else (j, j')
					   end
				      | _ => Error.bug "redundant expected two branches"
				  end
			     | _ => Error.bug "redundant expected con"
			 fun add (j, f) =
			    let
			       val {numPreds, facts, ...} = jumpInfo j
			    in
			       if 1 = !numPreds
				  then List.push (facts, f)
			       else ()
			    end
		      in
			 case varInfo test of
			    Fact f =>
			       let
				  val (j, j') = falseTrue ()
				  val _ = add (j, Fact.negate f)
				  val _ = add (j', f)
			       in
				  ()
			       end
			  | Or (f, f') =>
			       let
				  val (j, _) = falseTrue ()
				  val {facts, numPreds, ...} = jumpInfo j
			       in
				  if 1 = !numPreds
				     then
					facts :=
					Fact.negate f :: Fact.negate f'
					:: !facts
				  else ()
			       end
			  | _ => ()
		      end
	       | _ => ()
	     fun loop (e: Exp.t, node): unit =
		let
		   val {decs, transfer} = Exp.dest e
		   val _ =
		      List.foreach
		      (decs, fn (d: Dec.t) =>
		       case d of
			  Bind {var, exp, ...} =>
			     (case exp of
				 PrimExp.Const c => setConst (var, c)
			       | PrimApp pa => setVarInfo (var, makeVarInfo pa)
			       | _ => ())
			| Fun {name, body, ...} =>
			     loop (body, jumpNode name)
			| HandlerPop => ()
			| HandlerPush _ => ())
		   val _ = loopTransfer (transfer, node)
		in
		   ()
		end
	     val _ = loop (body, root)
	     (* Analysis is done. *)
	     (* Set up ancestors. *)
	     fun loop (n: Node.t, a: Node.t option) =
		let
		   val {ancestor, children, facts, ...} = nodeInfo n
		   val _ = ancestor := a
		   val a =
		      case !facts of
			 [] => a
		       | _ => SOME n
		in
		   List.foreach (!children, fn n => loop (n, a))
		end
	     val _ = loop (root, NONE)
	     (* Diagnostic. *)
	     val _ = 
		Control.diagnostics
		(fn display =>
		 Exp.foreachDec
		 (body,
		  fn Fun {name, ...} =>
		  let open Layout
		  in display (seq [Jump.layout name,
				   str " ",
				   List.layout Fact.layout
				   (! (#facts (jumpInfo name)))])
		  end
		   | _ => ()))
	     (* Transformation. *)
	     fun determine (i: NodeInfo.t, f: Fact.t) =
		let
		   fun loop ({facts, ancestor, ...}: NodeInfo.t) =
		      case Fact.determine (!facts, f) of
			 Unknown =>
			    (case !ancestor of
				NONE => Unknown
			      | SOME n => loop (nodeInfo n))
		       | r => r
		in
		   loop i
		end
	     fun loop (e: Exp.t, i: NodeInfo.t): Exp.t =
		let
		   val {decs, transfer} = Exp.dest e
		   val decs =
		      List.map
		      (decs, fn d =>
		       case d of
			  Bind {var, ty, ...} =>
			     let
				fun doit x =
				   (Int.inc numSimplified
				    ; (Control.diagnostic
				       (fn () =>
					let open Layout
					in seq [Var.layout var, str " ",
						Var.layout x]
					end))
				    ; Bind {var = var, ty = ty,
					    exp = Var x})
				fun falsee () = doit falseVar
				fun truee () = doit trueVar
			     in
				case varInfo var of
				   Or (f, f') =>
				      (case determine (i, f) of
					  False =>
					     (case determine (i, f') of
						 False => falsee ()
					       | True => truee ()
					       | Unknown => d)
					| True => truee ()
					| Unknown => d)
				 | Fact f =>
				      (case determine (i, f) of
					  False => falsee ()
					| True => truee ()
					| Unknown => d)
				 | _ => d
			     end
			| Fun {name, args, body} =>
			     Fun {name = name,
				  args = args,
				  body = loop (body, jumpInfo name)}
			| HandlerPop => d
			| HandlerPush _ => d)
		in
		   Exp.make {decs = decs,
			     transfer = transfer}
		end
	     val body = shrink (loop (body, NodeInfo.dummy))
	     val _ = Exp.clear body
	  in
	     Function.T {name = name,
			 args = args,
			 body = body,
			 returns = returns}
	  end
      val functions = Vector.map (functions, simplifyFunction)
      val _ =
	 Control.diagnostic
	 (fn () =>
	  let open Layout
	  in seq [str "numSimplified = ", Int.layout (!numSimplified)]
	  end)
      val program = 
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
   in
      program
   end

end
