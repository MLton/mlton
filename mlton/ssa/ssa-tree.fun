(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor SsaTree (S: SSA_TREE_STRUCTS): SSA_TREE = 
struct

open S

structure Type = Cps.Type
structure Func = Cps.Func
structure Label = Cps.Jump
structure Cases = Cps.Cases

local open Layout
in
   fun layoutTuple xs = Vector.layout Var.layout xs
end

structure Var =
   struct
      open Var

      fun pretty (x, global) =
	 case global x of
	    NONE => toString x
	  | SOME s => s

      fun prettys (xs: Var.t vector, global: Var.t -> string option) =
	 Layout.toString (Vector.layout
			  (fn x =>
			   case global x of
			      NONE => layout x
			    | SOME s => Layout.str s)
			  xs)
   end

structure Exp =
   struct
      datatype t =
	 ConApp of {con: Con.t,
		    args: Var.t vector}
       | Const of Const.t
       | PrimApp of {prim: Prim.t,
		     targs: Type.t vector,
		     args: Var.t vector}
       | Select of {tuple: Var.t,
		    offset: int}
       | SetExnStackLocal
       | SetExnStackSlot
       | SetSlotExnStack
       | SetHandler of Label.t
       | Tuple of Var.t vector
       | Var of Var.t

      val unit = Tuple (Vector.new0 ())
	 
      fun foreachLabelVar (e, j, v) =
	 let
	    fun vs xs = Vector.foreach (xs, v)
	 in
	    case e of
	       ConApp {args, ...} => vs args
	     | Const _ => ()
	     | PrimApp {args, ...} => vs args
	     | Select {tuple, ...} => v tuple
	     | SetExnStackLocal => ()
	     | SetExnStackSlot => ()
	     | SetSlotExnStack => ()
	     | SetHandler h => j h
	     | Tuple xs => vs xs
	     | Var x => v x
	 end

      fun foreachLabel (e, j) = foreachLabelVar (e, j, fn _ => ())
      fun foreachVar (e, v) = foreachLabelVar (e, fn _ => (), v)

      fun replaceLabelVar (e, fj, f) =
	 let
	    fun fs xs = Vector.map (xs, f)
	 in
	    case e of
	       ConApp {con, args} => ConApp {con = con, args = fs args}
	     | Const _ => e
	     | PrimApp {prim, targs, args} =>
		  PrimApp {prim = prim, targs = targs, args = fs args}
	     | Select {tuple, offset} =>
		  Select {tuple = f tuple, offset = offset}
	     | SetExnStackLocal => e
	     | SetExnStackSlot => e
	     | SetHandler h => SetHandler (fj h)
	     | SetSlotExnStack => e
	     | Tuple xs => Tuple (fs xs)
	     | Var x => Var (f x)
	 end

      fun replaceVar (e, f) = replaceLabelVar (e, fn j => j, f)

      fun layout e =
	 let
	    open Layout
	 in
	    case e of
	       ConApp {con, args} =>
		  seq [Con.layout con, str " ", layoutTuple args]
	     | Const c => Const.layout c
	     | PrimApp {prim, targs, args} =>
		  seq [Prim.layout prim,
		       if !Control.showTypes
			  then if 0 = Vector.length targs
				  then empty
			       else Vector.layout Type.layout targs
		       else empty,
		       if isSome (Prim.numArgs prim)
			  then seq [str " ", layoutTuple args]
		       else empty]
	     | Select {tuple, offset} =>
		  seq [str "#", Int.layout (offset + 1), str " ",
		       Var.layout tuple]
	     | SetExnStackLocal => str "SetExnStackLocal"
	     | SetExnStackSlot => str "SetExnStackSlot"
	     | SetHandler h => seq [str "SetHandler ", Label.layout h]
	     | SetSlotExnStack => str "SetSlotExnStack"
	     | Tuple xs => layoutTuple xs
	     | Var x => Var.layout x
	 end

      val isFunctional =
	 fn ConApp _ => true
	  | Const _ => true
	  | PrimApp {prim, ...} => Prim.isFunctional prim
	  | Select _ => true
	  | SetExnStackLocal => false
	  | SetExnStackSlot => false
	  | SetHandler _ => false
	  | SetSlotExnStack => false
	  | Tuple _ => true
	  | Var _ => true
	       
      fun maySideEffect (e: t): bool =
	 case e of
	    ConApp _ => false
	  | Const _ => false
	  | PrimApp {prim,...} => Prim.maySideEffect prim
	  | Select _ => false
	  | SetExnStackLocal => true
	  | SetExnStackSlot => true
	  | SetHandler _ => true
	  | SetSlotExnStack => true
	  | Tuple _ => false
	  | Var _ => false

      fun varsEquals (xs, xs') = Vector.equals (xs, xs', Var.equals)

      local
	 val newHash = Random.word
	 val conApp = newHash ()
	 val primApp = newHash ()
	 val select = newHash ()
	 val setExnStackLocal = newHash ()
	 val setExnStackSlot = newHash ()
	 val setHandler = newHash ()
	 val setSlotExnStack = newHash ()
	 val tuple = newHash ()
	 fun hashVars (xs: Var.t vector, w: Word.t): Word.t =
	    Vector.fold (xs, w, fn (x, w) => Word.xorb (w, Var.hash x))
      in
	 val hash: t -> Word.t =
	    fn ConApp {con, args, ...} => hashVars (args, Con.hash con)
	     | Const c => Const.hash c
	     | PrimApp {args, ...} => hashVars (args, primApp)
	     | Select {tuple, offset} =>
		  Word.xorb (select, Var.hash tuple + Word.fromInt offset)
	     | SetExnStackLocal => setExnStackLocal
	     | SetExnStackSlot => setExnStackSlot
	     | SetHandler h => Word.xorb (Label.hash h, setHandler)
	     | SetSlotExnStack => setSlotExnStack
	     | Tuple xs => hashVars (xs, tuple)
	     | Var x => Var.hash x
      end

      val hash = Trace.trace ("Exp.hash", layout, Word.layout) hash

      val toString = Layout.toString o layout

      fun toPretty (e: t, global: Var.t -> string option): string =
	 case e of
	    ConApp {con, args} =>
	       concat [Con.toString con, " ", Var.prettys (args, global)]
	  | Const c => Const.toString c
	  | PrimApp {prim, args, ...} =>
	       Layout.toString
	       (Prim.layoutApp (prim, args, fn x =>
				case global x of
				   NONE => Var.layout x
				 | SOME s => Layout.str s))
	  | SetExnStackLocal => "SetExnStackLocal"
	  | SetExnStackSlot => "SetExnStackSlot"
	  | SetSlotExnStack => "SetSlotExnStack"
	  | Select {tuple, offset} =>
	       concat ["#", Int.toString (offset + 1), " ", Var.toString tuple]
	  | SetHandler h => concat ["SetHandler ", Label.toString h]
	  | Tuple xs => Var.prettys (xs, global)
	  | Var x => Var.toString x
   end
datatype z = datatype Exp.t

structure Statement =
   struct
      datatype t = T of {var: Var.t option, (* NONE iff ty = unit. *)
			 ty: Type.t,
			 exp: Exp.t}

      local
	 fun make f (T r) = f r
      in
	 val var = make #var
	 val ty = make #ty
	 val exp = make #exp
      end

      fun layout (T {var, ty, exp}) =
	 let
	    open Layout
	 in
	    seq [case var of
		    NONE => empty
		  | SOME x =>
		       seq [Var.layout x,
			    if !Control.showTypes
			       then seq [str ": ", Type.layout ty]
			    else empty,
			       str " = "],
                 Exp.layout exp]
      end

      fun noVar (e: Exp.t): t =
	  T {var = NONE,
	     ty = Type.unit,
	     exp = e}

      val setExnStackLocal = noVar Exp.SetExnStackLocal
      val setExnStackSlot = noVar Exp.SetExnStackSlot
      val setSlotExnStack = noVar Exp.SetSlotExnStack
      fun setHandler h = noVar (Exp.SetHandler h)

      fun clear (T {var, ...}): unit = Option.app (var, Var.clear)

      fun lastHandler (ss: t vector, first: Label.t option) =
	 Vector.fold
	 (ss, first, fn (T {exp, ...}, ac) =>
	  case exp of
	     SetHandler l => SOME l
	   | _ => ac)
   end

structure Transfer =
   struct
      datatype t =
	 Bug
       | Call of {func: Func.t,
		  args: Var.t vector,
		  return: Label.t option}
       | Case of {test: Var.t,
		  cases: Label.t Cases.t,
		  default: Label.t option}
       | Goto of {dst: Label.t,
		  args: Var.t vector}
       | Prim of {prim: Prim.t,
		  args: Var.t vector,
		  failure: Label.t, (* Must be nullary. *)
		  success: Label.t  (* Must be unary. *)
		  }
       | Raise of Var.t vector
       | Return of Var.t vector
	 
      fun foreachFuncLabelVar (t, func, label, var) =
	 let
	    fun vars xs = Vector.foreach (xs, var)
	 in
	    case t of
	       Bug => ()
	     | Call {func = f, args, return, ...} =>
		  (func f
		   ; Option.app (return, label)
		   ; vars args)
	     | Case {test, cases, default, ...} =>
		  (var test
		   ; Cases.foreach (cases, label)
		   ; Option.app (default, label))
	     | Goto {dst, args, ...} => (vars args; label dst)
	     | Prim {args, failure, success, ...} =>
		  (vars args; label failure; label success)
	     | Raise xs => vars xs
	     | Return xs => vars xs
	 end

      fun foreachLabelVar (t, label, var) =
	 foreachFuncLabelVar (t, fn _ => (), label, var)
	 
      fun foreachLabel (t, j) = foreachLabelVar (t, j, fn _ => ())
      fun foreachVar (t, v) = foreachLabelVar (t, fn _ => (), v)

      fun replaceVar (t, f) =
	 let
	    fun fs xs = Vector.map (xs, f)
	 in
	    case t of
	       Bug => Bug
	     | Call {func, args, return} =>
		  Call {func = func, args = fs args, return = return}
	     | Case {test, cases, default} =>
		  Case {test = f test, cases = cases, default = default}
	     | Goto {dst, args} => Goto {dst = dst, args = fs args}
	     | Prim {prim, args, failure, success} =>
		  Prim {prim = prim,
			args = fs args,
			failure = failure,
			success = success}
	     | Raise xs => Raise (fs xs)
	     | Return xs => Return (fs xs)
	 end

      local open Layout
      in
	 fun layoutCase {test, cases, default} =
	    let
	       fun doit (l, layout) =
		  Vector.toListMap
		  (l, fn (i, l) =>
		   seq [layout i, str " => ", Label.layout l])
	       datatype z = datatype Cases.t
	       val cases =
		  case cases of
		     Char l => doit (l, Char.layout)
		   | Con l => doit (l, Con.layout)
		   | Int l => doit (l, Int.layout)
		   | Word l => doit (l, Word.layout)
		   | Word8 l => doit (l, Word8.layout)
	       val cases =
		  case default of
		     NONE => cases
		   | SOME j =>
			cases @ [seq [str "_ => ", Label.layout j]]
	    in align [seq [str "case ", Var.layout test, str " of"],
		      indent (alignPrefix (cases, "| "), 2)]
	    end

	 val layout =
	    fn Bug => str "Bug"
	     | Call {func, args, return} =>
		  let
		     val call = seq [Func.layout func, str " ", layoutTuple args]
		  in
		     case return of
			NONE => call
		      | SOME l => seq [Label.layout l, str " ", paren call]
		  end
	     | Case arg => layoutCase arg
	     | Goto {dst, args} =>
		  seq [Label.layout dst, str " ", layoutTuple args]
	     | Prim {prim, args, failure, success} =>
		  Prim.layoutApp (prim, args, Var.layout)
	     | Raise xs => seq [str "raise ", layoutTuple xs]
	     | Return xs => if 1 = Vector.length xs
			       then Var.layout (Vector.sub (xs, 0))
			    else layoutTuple xs
      end
   end
datatype z = datatype Transfer.t

   
structure Handler =
   struct
      (* A flat lattice.
       * Unset <= Handler l <= Many
       *)
      datatype handler =
	 Handler of Label.t
       | Many
       | Unset

      datatype t = T of {handler: handler ref,
			 lessThan: t list ref}

      fun new () = T {handler = ref Unset,
		      lessThan = ref []}

      val isHandler =
	 fn (T {handler = ref (Handler _), ...}) => true
	  | _ => false
	       
      fun layout (T {handler, ...}) =
	 let
	    open Layout
	 in
	    case !handler of
	       Handler l => Label.layout l
	     | Many => str "Many"
	     | Unset => str "Unset"
	 end

      val bottom = Unset
      val isTop = fn Many => true | _ => false

      fun up (T {handler, lessThan, ...}, h) =
	 let
	    fun change h = (handler := h
			    ; List.foreach (!lessThan, fn i => up (i, h)))
	 in
	    case (h, !handler) of
	       (Unset, _) => ()
	     | (_, Unset) => change h
	     | (Handler l, Handler l') =>
		  if Label.equals (l, l')
		     then ()
		  else change Many
	     | _ => ()
	 end

      fun set (h, l) = up (h, Handler l)

      val op <= =
	 fn (T {handler = ref h, lessThan}, i) =>
	 (if isTop h
	     then ()
	  else List.push (lessThan, i)
	     ; up (i, h))
   end

structure ExnStack =
   struct
      (* A flat lattice.
       *)
      datatype exnStack =
	 Caller
       | Me
       | Unknown
       | Unreachable
 
      val bottom = Unreachable
      val isTop = fn Unknown => true | _ => false

      datatype t = T of {exnStack: exnStack ref,
			 lessThan: t list ref}

      fun layout (T {exnStack = ref e, ...}) =
	 Layout.str
	 (case e of
	     Caller => "Caller"
	   | Me => "Me"
	   | Unknown => "Unknown"
	   | Unreachable => "Unreachable")

      local
	 fun make e (T {exnStack, ...}) = e = !exnStack
      in
	 val isCaller = make Caller
	 val isMe = make Me
      end

      fun new () = T {exnStack = ref bottom,
		      lessThan = ref []}

      fun up (T {exnStack, lessThan, ...}, e) =
	 let
	    fun change e = (exnStack := e
			    ; List.foreach (!lessThan, fn i => up (i, e)))
	 in
	    case (e, !exnStack) of
	       (Unreachable, _) => ()
	     | (_, Unreachable) => change e
	     | (Caller, Me) => change Unknown
	     | (Me, Caller) => change Unknown
	     | _ => ()
	 end

      val me =
	 let
	    val e = new ()
	    val _ = up (e, Me)
	 in
	    e
	 end

      val op <= =
	 fn (T {exnStack = ref e, lessThan}, i) =>
	 (if isTop e
	     then ()
	  else List.push (lessThan, i)
	     ; up (i, e))
   end

structure After =
   struct
      datatype t =
	 None
       | Some of Label.t
       | Unset

      fun toLabelOpt a =
	 case a of
	    None => NONE
	  | Some l => SOME l
	  | Unset => Error.bug "After.toLabelOpt of Unset"
   end

structure HandlerInfo =
   struct
      datatype t = T of {after: After.t ref,
			 exnStack: ExnStack.t,
			 handler: Handler.t,
			 slot: ExnStack.t}

      fun new () = T {after = ref After.Unset,
		      exnStack = ExnStack.new (),
		      handler = Handler.new (),
		      slot = ExnStack.new ()}
		      
      fun after (T {after = ref a, ...}) = After.toLabelOpt a

      fun beforeLay (T {exnStack, handler, slot, ...}) =
	 Layout.tuple [ExnStack.layout exnStack,
		       Handler.layout handler,
		       ExnStack.layout slot]
   end

structure Handlers =
   struct
      type t = Label.t -> HandlerInfo.t

      fun after (f: t, l) = HandlerInfo.after (f l)
   end

local open Layout
in
   fun layoutFormals (xts: (Var.t * Type.t) vector) =
      Vector.layout (fn (x, t) =>
		    seq [Var.layout x,
			 if !Control.showTypes
			    then seq [str ": ", Type.layout t]
			 else empty])
      xts
end

structure Block =
   struct
      datatype t =
	 T of {
	       label: Label.t,
	       args: (Var.t * Type.t) vector,
	       statements: Statement.t vector,
	       transfer: Transfer.t
	       }

      local
	 fun make f (T r) = f r
      in
	 val label = make #label
	 val args = make #args
	 val statements = make #statements
	 val transfer = make #transfer
      end
   
      fun layout (T {label, args, statements, transfer}, handlers) =
	 let
	    open Layout
	 in
	    align [seq [Label.layout label, str " ",
			Vector.layout (fn (x, t) =>
				       if !Control.showTypes
					  then seq [Var.layout x, str ": ",
						    Type.layout t]
				       else Var.layout x) args,
			HandlerInfo.beforeLay (handlers label)],
		   indent (align
			   [align
			    (Vector.toListMap (statements, Statement.layout)),
			    Transfer.layout transfer],
			   2)]
	 end

      fun clear (T {label, args, statements, ...}) =
	 (Label.clear label
	  ; Vector.foreach (args, Var.clear o #1)
	  ; Vector.foreach (statements, Statement.clear))
   end

structure Datatype =
   struct
      datatype t =
	 T of {
	       tycon: Tycon.t,
	       cons: {con: Con.t,
		      args: Type.t vector} vector
	       }

      fun layout (T {tycon, cons}) =
	 let
	    open Layout
	 in
	    seq [Tycon.layout tycon,
		 str " = ",
		 alignPrefix
		 (Vector.toListMap
		  (cons, fn {con, args} =>
		   seq [Con.layout con,
			if Vector.isEmpty args
			   then empty
			else seq [str " of ",
				  Vector.layout Type.layout args]]),
		  "| ")]
	 end

      fun clear (T {tycon, cons}) =
	 (Tycon.clear tycon
	  ; Vector.foreach (cons, Con.clear o #con))
   end

structure Function =
   struct
      datatype t = T of {name: Func.t,
			 args: (Var.t * Type.t) vector,
			 start: Label.t,
			 blocks: Block.t vector,
			 returns: Type.t vector}
			    
      local
	 structure Graph = DirectedGraph
	 structure Node = Graph.Node
	 structure Edge = Graph.Edge
      in
	 fun controlFlowGraph (T {name, args, start, blocks,...},
			       handlers: Handlers.t) =
	    let
    	       open Graph.LayoutDot
	       val g = Graph.new ()
	       fun newNode () = Graph.newNode g
	       val {get = labelNode} =
		  Property.get
		  (Label.plist, Property.initFun (fn _ => newNode ()))
	       val {get = nodeInfo: Node.t -> {block: Block.t,
					       children: Node.t list ref},
		    set = setNodeInfo} =
		  Property.getSetOnce
		  (Node.plist, Property.initRaise ("info", Node.layout))
	       val _ =
		  Vector.foreach
		  (blocks, fn b as Block.T {label, statements, transfer, ...} =>
		   let
		      val from = labelNode label
		      val _ = setNodeInfo (from, {block = b,
						  children = ref []})
		      fun edge (to: Label.t): unit =
			(Graph.addEdge (g, {from = from, to = labelNode to})
			 ; ())
		      fun after () =
			 Option.app (Handlers.after (handlers, label), edge)
		      val _ =
			 case transfer of
			   Bug => ()
			 | Call {return, ...} =>
			      Option.app (return, fn l =>
					  (edge l; after ()))
			 | Case {cases, default, ...} =>
			      (Cases.foreach (cases, edge)
			       ; Option.app (default, edge))
			 | Goto {dst, ...} => edge dst
			 | Prim {failure, success, ...} =>
			      (edge failure; edge success)
			 | Raise _ => after ()
			 | Return _ => ()
		   in
		      ()
		   end)
	       val root = labelNode start
	       fun dominatorTree () =
		  let
		     val {idom} = Graph.dominators {graph = g, root = root}
		     val _ =
			List.foreach
			(Graph.nodes g, fn n =>
			 if Node.equals (n, root)
			    then ()
			 else List.push (#children (nodeInfo (idom n)), n))
		     fun treeAt (n: Node.t): Block.t Tree.t =
			let
			   val {block, children} = nodeInfo n
			in
			   Tree.T (block, List.revMap (!children, treeAt))
			end
		  in
		     treeAt root
		  end
	    in
	       {graph = g,
		labelNode = labelNode,
		dominatorTree = dominatorTree}
	    end

	 fun dominatorTree z =
	    #dominatorTree (controlFlowGraph z) ()

	 fun layoutDot (T {name, args, start, blocks, ...},
			handlers: Handlers.t,
			global) =
	    let
	       fun makeName (name: string,
			     formals: (Var.t * Type.t) vector): string =
		  concat [name, " ",
			  let
			     open Layout
			  in
			     toString
			     (vector
			      (Vector.map
			       (formals, fn (var, ty) =>
				if !Control.showTypes
				   then seq [Var.layout var,
					     str ": ",
					     Type.layout ty]
				else Var.layout var)))
			  end]
	       open Graph.LayoutDot
	       val graph = Graph.new ()
	       val {get = nodeOptions, ...} =
		  Property.get (Node.plist, Property.initFun (fn _ => ref []))
	       fun setNodeText (n: Node.t, l): unit =
		  List.push (nodeOptions n, NodeOption.Label l)
	       fun newNode () = Graph.newNode graph
	       val {destroy, get = labelNode} =
		  Property.destGet (Label.plist,
				    Property.initFun (fn _ => newNode ()))
	       val {get = edgeOptions, set = setEdgeOptions} =
		  Property.getSetOnce (Edge.plist, Property.initConst [])
	       val _ =
		  Vector.foreach
		  (blocks, fn Block.T {label, args, statements, transfer} =>
		   let
		      val from = labelNode label
		      fun edge (to: Label.t,
				label: string,
				style: style): unit =
			 let
			    val e = Graph.addEdge (graph, {from = from,
							   to = labelNode to})
			    val _ = setEdgeOptions (e, [EdgeOption.label label,
							EdgeOption.Style style])
			 in
			    ()
			 end
		      val rest =
			 case transfer of
			    Bug => ["bug"]
			  | Call {func, args, return, ...} =>
			       let
				  val f = Func.toString func
				  val args = Var.prettys (args, global)
			       in
				  case return of
				     NONE => [f, " ", args]
				   | SOME l =>
					(edge (l, "", Dotted)
					 ; Option.app (Handlers.after
						       (handlers, label),
						       fn h => 
						       edge (h, "", Dashed))
					 ; [Label.toString l,
					    " (", f, args, ")"])
			       end
			  | Case {test, cases, default, ...} =>
			       let
				  fun doit (v, toString) =
				     Vector.foreach
				     (v, fn (x, j) =>
				      edge (j, toString x, Solid))
				  val _ =
				     case cases of
					Cases.Char v => doit (v, Char.toString)
				      | Cases.Con v => doit (v, Con.toString)
				      | Cases.Int v => doit (v, Int.toString)
				      | Cases.Word v => doit (v, Word.toString)
				      | Cases.Word8 v => doit (v, Word8.toString)
				  val _ = 
				     case default of
					NONE => ()
				      | SOME j =>
					   edge (j, "default", Solid)
			       in
				  ["case ", Var.toString test]
			       end
			  | Goto {dst, args} =>
			       (edge (dst, "", Solid)
				; [Label.toString dst, " ",
				   Var.prettys (args, global)])
			  | Prim {prim, args, failure, success} =>
			       (edge (success, "", Solid)
				; edge (failure, "Overflow", Dashed)
				; [Layout.toString
				   (Prim.layoutApp (prim, args, fn x =>
						    Layout.str
						    (Var.pretty (x, global))))])
			  | Raise xs =>
			       (Option.app
				(Handlers.after (handlers, label),
				 fn l => edge (l, "", Solid))
				; ["raise ", Var.prettys (xs, global)])
			  | Return xs => ["return ", Var.prettys (xs, global)]
		      val lab =
			 Vector.foldr
			 (statements, [(concat rest, Left)],
			  fn (Statement.T {var, ty, exp, ...}, ac) =>
			  let
			     val exp = Exp.toPretty (exp, global)
			     val s =
				case var of
				   NONE => exp
				 | SOME x =>
				      concat
				      [Var.toString x,
				       if !Control.showTypes
					  then concat [": ",
						       Layout.toString
						       (Type.layout ty)]
				       else "",
				       " = ", exp]
			  in
			     (s, Left) :: ac
			  end)
		      val name =
			 concat [makeName (Label.toString label, args), " ",
				 Layout.toString
				 (HandlerInfo.beforeLay (handlers label))]
		      val _ = setNodeText (from, (name, Left) :: lab)
		   in
		      ()
		   end)
	       val root = labelNode start
	       val graphLayout =
		  Graph.LayoutDot.layout
		  {graph = graph,
		   title = concat [makeName (Func.toString name, args),
				   " control-flow graph"],
		   options = [GraphOption.Rank (Min, [root])],
		   edgeOptions = edgeOptions,
		   nodeOptions =
		   fn n => let
			      val l = ! (nodeOptions n)
			      open NodeOption
			   in FontColor Black :: Shape Box :: l
			   end}
	       fun treeLayout () =
		  let
		     val {tree, graphToTree} =
			Graph.dominatorTree {graph = graph, root = root}
		     val _ =
			Vector.foreach
			(blocks, fn Block.T {label, ...} =>
			 setNodeText (graphToTree (labelNode label),
				      [(Label.toString label, Center)]))
		     val treeLayout =
			Graph.LayoutDot.layout
			{graph = tree,
			 title = concat [Func.toString name, " dominator tree"],
			 options = [],
			 edgeOptions = fn _ => [],
			 nodeOptions = ! o nodeOptions}
		     val _ = destroy ()
		  in
		     treeLayout
		  end
	    in
	       {graph = graphLayout,
		tree = treeLayout}
	    end
      end
   
      fun layout (func as T {name, args, start, blocks, returns},
		  handlers: Handlers.t,
		  global: Var.t -> string option) =
	 let
	    open Layout
	 in
	    align [seq [str "fun ",
			Func.layout name,
			str " ",
			layoutFormals args,
			if !Control.showTypes
			   then seq [str ": ", Vector.layout Type.layout returns]
			else empty,
			str " = ", Label.layout start, str "()"],
		   indent (align (Vector.toListMap
				  (blocks, fn b =>
				   Block.layout (b, handlers))),
			   2)]
	 end
      
      fun layouts (fs: t vector,
		   handlers: Handlers.t,
		   global,
		   output: Layout.t -> unit): unit =
	 Vector.foreach
	 (fs, fn f as T {name, ...} =>
	  let
	     val _ = output (layout (f, handlers, global))
	     val _ =
		if not (!Control.keepDot)
		   then ()
		else
		   let
		      val {graph, tree} = layoutDot (f, handlers, global)
		      val name = Func.toString name
		      fun doit (s, g) =
			 Control.saveToFile
			 ({suffix = concat [name, ".", s, ".dot"]}, g)
		      val _ = doit ("cfg", graph)
		      val _ = doit ("dom", tree ())
		   in
		      ()
		   end
	  in
	     ()
	  end)
   end

structure Program =
   struct
      datatype t =
	 T of {
	       datatypes: Datatype.t vector,
	       globals: Statement.t vector,
	       functions: Function.t vector,
	       main: Func.t
	       }
   end

fun inferHandlers (Program.T {functions, ...}) =
   let
      val {get = labelInfo: Label.t -> HandlerInfo.t} =
	 Property.get (Label.plist,
		       Property.initFun (fn _ => HandlerInfo.new ()))
      val _ =
	 Vector.foreach
	 (functions, fn Function.T {start, blocks, ...} =>
	  let
	     val HandlerInfo.T {exnStack, ...} = labelInfo start
	     val _ = ExnStack.up (exnStack, ExnStack.Caller)
	     val constraints: (unit -> unit) list ref = ref []
	     val _ =
		Vector.foreach
		(blocks, fn b as Block.T {label, statements, transfer, ...} =>
		 let
		    val hi as HandlerInfo.T {after, exnStack, handler, slot} =
		       labelInfo label
		    fun assert (msg, f) =
		       List.push
		       (constraints, fn () =>
			if f ()
			   then ()
			else 
			   (Control.message
			    (Control.Silent, fn () =>
			     let open Layout
			     in align [seq [str "block: ",
					    Block.layout (b, labelInfo),
					    str "info: ",
					    HandlerInfo.beforeLay hi]]
			     end)
			    ; Error.bug (concat ["handler mismatch at ", msg])))
		    val {exnStack, handler, slot} =
		       Vector.fold
		       (statements,
			{exnStack = exnStack, handler = handler, slot = slot},
			fn (Statement.T {exp, ...}, {exnStack, handler, slot}) =>
			case exp of
			   SetExnStackLocal =>
			      {exnStack = ExnStack.me,
			       handler = handler,
			       slot = slot}
			 | SetExnStackSlot =>
			      {exnStack = slot,
			       handler = handler,
			       slot = slot}
			 | SetSlotExnStack =>
			      {exnStack = exnStack,
			       handler = handler,
			       slot = exnStack}
			 | SetHandler l =>
			      let
				 val handler = Handler.new ()
				 val _ = Handler.set (handler, l)
				 val HandlerInfo.T {exnStack = e,
						    handler = h, slot = s, ...} =
				    labelInfo l
				 val _ = ExnStack.up (e, ExnStack.Me)
				 val _ = Handler.set (h, l)
				 val _ = ExnStack.up (s, ExnStack.Caller)
			      in
				 {exnStack = exnStack,
				  handler = handler,
				  slot = slot}
			      end
			 | _ => {exnStack = exnStack,
				 handler = handler,
				 slot = slot})
		    fun goto l =
		       let
			  val HandlerInfo.T {exnStack = e, handler = h,
					     slot = s, ...} =
			     labelInfo l
		       in
			  ExnStack.<= (exnStack, e)
			  ; Handler.<= (handler, h)
			  ; ExnStack.<= (slot, s)
		       end
		    fun tail name =
		       assert (name, fn () => ExnStack.isCaller exnStack)
		    fun check name =
		       assert
		       (name, fn () =>
			let
			   val ExnStack.T {exnStack = ref e, ...} = exnStack
			in
			   case e of
			      ExnStack.Caller =>
				 (after := After.None; true)
			    | ExnStack.Me => 
				 (case handler of
				     Handler.T
				     {handler = ref (Handler.Handler l), ...} =>
					(after := After.Some l; true)
				   | _ => false)
			    | _ => false
			end)
		 in
		    case transfer of
		       Bug => ()
		     | Call {return, ...} =>
			  (Option.app (return, goto)
			   ; (case return of
				 NONE => tail "tail call"
			       | SOME l => check "nontail call"))
		     | Case {cases, default, ...} =>
			  (Cases.foreach (cases, goto)
			   ; Option.app (default, goto))
		     | Goto {dst, ...} => goto dst
		     | Prim {failure, success, ...} =>
			  (goto failure; goto success)
		     | Raise _ => check "raise"
		     | Return _ => tail "return"
		 end)
	     val _ =
		if true
		   then ()
		else
		   Vector.foreach
		   (blocks, fn b =>
		    Layout.outputl (Block.layout (b, labelInfo),
				    Out.error))
	     (* The dataflow equations have been solved.
	      * Check the constraints.
	      *)
	     val _ = List.foreach (!constraints, fn f => f ())
	  in
	     ()
	  end)
   in
      labelInfo
   end

val inferHandlers = Control.trace (Control.Pass, "inferHandlers") inferHandlers

structure Program =
   struct
      open Program

      val inferHandlers = inferHandlers

      local
	 structure Graph = DirectedGraph
	 structure Node = Graph.Node
	 structure Edge = Graph.Edge
      in
	 fun layoutCallGraph (T {functions, main, ...},
			      title: string): Layout.t =
	    let
	       open Graph.LayoutDot
	       val graph = Graph.new ()
	       val {get = nodeOptions, set = setNodeOptions, ...} =
		  Property.getSetOnce
		  (Node.plist, Property.initRaise ("options", Node.layout))
	       val {get = funcNode, destroy} =
		  Property.destGet
		  (Func.plist, Property.initFun
		   (fn f =>
		    let
		       val n = Graph.newNode graph
		       val _ =
			  setNodeOptions
			  (n,
			   let open NodeOption
			   in [FontColor Black, label (Func.toString f)]
			   end)
		    in
		       n
		    end))
	       val {get = edgeOptions, set = setEdgeOptions} =
		  Property.getSetOnce (Edge.plist, Property.initConst [])
	       val _ =
		  Vector.foreach
		  (functions, fn Function.T {name, blocks, ...} =>
		   let
		      val from = funcNode name
		      val {get, destroy} =
			 Property.destGet
			 (Node.plist,
			  Property.initFun (fn _ => {nontail = ref false,
						     tail = ref false}))
		      val _ = 
			 Vector.foreach
			 (blocks, fn Block.T {transfer, ...} =>
			  case transfer of
			     Call {func, return, ...} =>
				let
				   val to = funcNode func
				   val {tail, nontail} = get to
				   val r = if isSome return
					      then nontail
					   else tail
				in
				   if !r
				      then ()
				   else (r := true
					 ; (setEdgeOptions
					    (Graph.addEdge
					     (graph, {from = from, to = to}),
					     if isSome return
						then []
					     else [EdgeOption.Style Dotted])))
				end
			   | _ => ())
		      val _ = destroy ()
		   in
		      ()
		   end)
	       val l =
		  Graph.LayoutDot.layout
		  {graph = graph,
		   title = title,
		   options = [],
		   edgeOptions = edgeOptions,
		   nodeOptions = nodeOptions}
	       val _ = destroy ()
	    in
	       l
	    end
      end
	 
      fun layouts (p as T {datatypes, globals, functions, main},
		   output': Layout.t -> unit) =
	 let
	    val {get = global, set = setGlobal} =
	       Property.getSet (Var.plist,
				Property.initConst NONE)
	    val _ = 
	       Vector.foreach
	       (globals, fn Statement.T {var, exp, ...} =>
		case var of
		   NONE => ()
		 | SOME x =>
		      let
			 fun set s = setGlobal (x, SOME s)
		      in
			 case exp of
			    Const c => set (Layout.toString (Const.layout c))
			  | ConApp {con, args, ...} =>
			       if Vector.isEmpty args
				  then set (Con.toString con)
			       else set (concat [Con.toString con, "(...)"])
			  | _ => ()
		      end)
	    val handlers = inferHandlers p
	    open Layout
	    val output = output'
	 in output (align (Vector.toListMap (datatypes, Datatype.layout)))
	    ; output (str "\n\nGlobals:")
	    ; Vector.foreach (globals, output o Statement.layout)
	    ; output (str "\n\nFunctions:")
	    ; Function.layouts (functions, handlers, global, output)
	    ; output (seq [str "\n\nMain: ", Func.layout main])
	    ; if not (!Control.keepDot)
		 then ()
	      else (Control.saveToFile
		    ({suffix = "call-graph.dot"},
		     layoutCallGraph (p, !Control.inputFile)))
	 end

      fun layoutStats (T {datatypes, globals, functions, ...}) =
	 let
	    val numTypes = ref 0
	    fun inc _ = Int.inc numTypes
	    val {hom = countType, destroy} =
	       Type.makeHom
	       {var = fn _ => Error.bug "cps-tree saw var",
		con = inc}
	    val numStatements = ref (Vector.length globals)
	    val numBlocks = ref 0
	    val _ =
	       Vector.foreach
	       (functions, fn Function.T {args, blocks, ...} =>
		(Vector.foreach (args, countType o #2)
		 ; (Vector.foreach
		    (blocks, fn Block.T {statements, ...} =>
		     (Int.inc numBlocks
		      ; (Vector.foreach
			 (statements, fn Statement.T {ty, ...} =>
			  (countType ty
			   ; Int.inc numStatements))))))))
	    val numFunctions = Vector.length functions
	    open Layout
	    val _ = destroy ()
	 in
	    align
	    (List.map
	     ([("num functions", Int.layout numFunctions),
	       ("num blocks", Int.layout (!numBlocks)),
	       ("num statements", Int.layout (!numStatements))],
	      fn (name, value) => seq [str (name ^ " "), value]))
	 end

      (* clear all property lists reachable from program *)
      fun clear (T {datatypes, globals, functions, main}) =
	 ((* Can't do Type.clear because it clears out the info needed for
	   * dest.
	   *)
	  (* Type.clear () *)
	  Vector.foreach (datatypes, Datatype.clear)
	  ; Vector.foreach (globals, Statement.clear)
	  ; (Vector.foreach
	     (functions, fn Function.T {name, args, blocks, ...} =>
	      (Func.clear name
	       ; Vector.foreach (args, Var.clear o #1)
	       ; Vector.foreach (blocks, Block.clear)))))

      fun hasPrim (T {globals, functions, ...},  f) =
	 DynamicWind.withEscape
	 (fn escape =>
	  let
	     fun loopStatement (Statement.T {exp, ...}) =
		case exp of
		   PrimApp {prim, ...} =>
		      if f prim
			 then escape true
		      else ()
		 | _ => ()
	     val _ = Vector.foreach (globals, loopStatement)
	     val _ =
		Vector.foreach
		(functions, fn Function.T {blocks, ...} =>
		 Vector.foreach
		 (blocks, fn Block.T {statements, ...} =>
		  Vector.foreach (statements, loopStatement)))
	  in
	     false
	  end)

      fun fromCps (p as Cps.Program.T {datatypes, globals, functions, main},
		   {jumpToLabel, funcToFunc}) =
	 let
	    val jumpHandlers = Cps.inferHandlers p
	    datatype z =
	       Normal of Statement.t
	     | Overflow of {prim: Prim.t,
			    args: Var.t vector,
			    failure: Label.t}
	    fun bindToStatement {var, ty, exp} =
	       let
		  fun normal exp =
		     Normal (Statement.T {var = SOME var, ty = ty, exp = exp})
		  datatype z = datatype Cps.PrimExp.t
	       in
		  case exp of
		     ConApp z => normal (Exp.ConApp z)
		   | Const z => normal (Exp.Const z)
		   | PrimApp {prim, targs, args, info} =>
			(case info of
			    Cps.PrimInfo.None =>
			       normal (Exp.PrimApp {prim = prim,
						    targs = targs,
						    args = args})
			  | Cps.PrimInfo.Overflow j =>
			       Overflow {prim = prim,
					 args = args,
					 failure = jumpToLabel j})
		   | Select z => normal (Exp.Select z)
		   | Tuple z => normal (Exp.Tuple z)
		   | Var z => normal (Exp.Var z)
	       end
	    fun loopTransfer t =
	       let
		  datatype z = datatype Cps.Transfer.t
	       in
		  case t of
		     Bug => Transfer.Bug
		   | Call {func, args, cont} =>
			Transfer.Call {func = funcToFunc func,
				       args = args,
				       return = Option.map (cont, jumpToLabel)}
		   | Case {test, cases, default, ...} =>
			Transfer.Case {test = test,
				       cases = cases,
				       default = default}
		   | Jump {dst, args} =>
			Transfer.Goto {dst = dst, args = args}
		   | Raise xs => Transfer.Raise xs
		   | Return xs => Transfer.Return xs
	       end
	    fun loopFunc (Cps.Function.T {name, args, body, returns}) =
	       let
		  val blocks = ref []
		  fun loop (e: Cps.Exp.t,
			    l: Label.t,
			    args: (Var.t * Type.t) vector,
			    handlers: Cps.Jump.t list): unit =
		     let
			val {decs, transfer} = Cps.Exp.dest e
		     in
			loopDecs (decs, [], l, args, handlers, transfer)
		     end
		  and loopDecs (decs, statements, l: Label.t, args,
				handlers, transfer) =
		     let
			fun addBlock (t: Transfer.t): unit =
			   List.push
			   (blocks,
			    Block.T {label = l,
				     args = args,
				     statements = Vector.fromListRev statements,
				     transfer = t})
		     in
			case decs of
			   [] => addBlock (loopTransfer transfer)
			 | d :: decs =>
			      let
				 fun continue statements =
				    loopDecs (decs, statements, l, args,
					      handlers, transfer)
				 datatype z = datatype Cps.Dec.t
			      in
				 case d of
				    Bind (b as {var, ...}) =>
				       (case bindToStatement b of
					   Normal s =>
					      continue (s :: statements)
					 | Overflow {prim, args, failure} =>
					      let
						 val success = Label.newNoname ()
						 val _ =
						    addBlock
						    (Prim {args = args,
							   failure = failure,
							   prim = prim,
							   success = success})
					      in loopDecs
						 (decs, [],
						  success,
						  Vector.new1 (var, Type.int),
						  handlers, transfer)
					      end)
				  | Fun {name, args, body} =>
				       (loop (body, jumpToLabel name, args,
					      jumpHandlers name)
					; continue statements)
				  | HandlerPush h =>
				       let
					  val ac =
					     Statement.setHandler (jumpToLabel h)
					     :: statements
					  val ac =
					     case handlers of
						[] =>
						   Statement.setExnStackLocal
						   :: Statement.setSlotExnStack
						   :: ac
					      | _ => ac
				       in
					  loopDecs (decs, ac,
						    l, args, h :: handlers,
						    transfer)
				       end
				  | HandlerPop =>
				       (case handlers of
					   [] =>
					      Error.bug
					      "pop of empty handler stack"
					 | _ :: handlers =>
					      let
						 val s =
						    case handlers of
						       [] =>
							  Statement.setExnStackSlot
						     | h :: _ =>
							  Statement.setHandler
							  (jumpToLabel h)
					      in
						 loopDecs
						 (decs, s :: statements,
						  l, args, handlers, transfer)
					      end)
			      end
		     end
		  val start = Label.newNoname ()
		  val _ = loop (body, start, Vector.new0 (), [])
	       in
		  Function.T {args = args,
			      blocks = Vector.fromList (!blocks),
			      name = funcToFunc name,
			      returns = returns,
			      start = start}
	       end
	    val program =
	       T {datatypes = Vector.map (datatypes, Datatype.T),
		  globals = (Vector.map
			     (globals, fn b =>
			      case bindToStatement b of
				 Normal s => s
			       | Overflow _ => Error.bug "overflow in globals")),
		  functions = Vector.map (functions, loopFunc),
		  main = funcToFunc main}
	    val _ = clear program
	 in
	    program
	 end
   end

end
