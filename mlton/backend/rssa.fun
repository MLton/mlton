(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Rssa (S: RSSA_STRUCTS): RSSA =
struct

open S
local
   open Runtime
in
   structure CFunction = CFunction
   structure GCField = GCField
end

structure Operand =
   struct
      datatype t =
	 ArrayOffset of {base: t,
			 index: t,
			 ty: Type.t}
       | Cast of t * Type.t
       | Const of Const.t
       | EnsuresBytesFree
       | File
       | GCState
       | Line
       | Offset of {base: t,
		    offset: int,
		    ty: Type.t}
       | PointerTycon of PointerTycon.t
       | Runtime of GCField.t
       | SmallIntInf of word
       | Var of {var: Var.t,
		 ty: Type.t}

      val char = Const o Const.fromChar
      val int = Const o Const.fromInt
      val word = Const o Const.fromWord
      fun bool b = Cast (int (if b then 1 else 0), Type.bool)
	 
      val ty =
	 fn ArrayOffset {ty, ...} => ty
	  | Cast (_, ty) => ty
	  | Const c =>
	       let
		  datatype z = datatype Const.Node.t
	       in
		  case Const.node c of
		     Char _ => Type.char
		   | Int _ => Type.int
		   | IntInf _ => Type.intInf
		   | Real _ => Type.real
		   | String _ => Type.string
		   | Word _ =>
			let
			   val ty = Const.ty c
			in
			   if Const.Type.equals (ty, Const.Type.word)
			      then Type.word
			   else if Const.Type.equals (ty, Const.Type.word8)
				   then Type.char
				else Error.bug "strange word"
			end
	       end
	  | EnsuresBytesFree => Type.word
	  | File => Type.cpointer
	  | GCState => Type.cpointer
	  | Line => Type.int
	  | Offset {ty, ...} => ty
	  | PointerTycon _ => Type.word
	  | Runtime z => Type.fromRuntime (GCField.ty z)
	  | SmallIntInf _ => Type.IntInf
	  | Var {ty, ...} => ty

      fun layout (z: t): Layout.t =
	 let
	    open Layout 
	    fun constrain (ty: Type.t): Layout.t =
	       if !Control.showTypes
		  then seq [str ": ", Type.layout ty]
	       else empty
	 in
	    case z of
	       ArrayOffset {base, index, ty} =>
		  seq [str (concat ["X", Type.name ty, " "]),
		       tuple [layout base, layout index],
		       constrain ty]
	     | Cast (z, ty) =>
		  seq [str "Cast ", tuple [layout z, Type.layout ty]]
	     | Const c => Const.layout c
	     | EnsuresBytesFree => str "<EnsuresBytesFree>"
	     | File => str "<File>"
	     | GCState => str "<GCState>"
	     | Line => str "<Line>"
	     | Offset {base, offset, ty} =>
		  seq [str (concat ["O", Type.name ty, " "]),
		       tuple [layout base, Int.layout offset],
		       constrain ty]
	     | PointerTycon pt => PointerTycon.layout pt
	     | Runtime r => GCField.layout r
	     | SmallIntInf w => seq [str "SmallIntInf ", paren (Word.layout w)]
	     | Var {var, ty} => seq [Var.layout var, constrain ty]
	 end

      val toString = Layout.toString o layout

      fun cast (z, t) =
	 if Type.equals (t, ty z)
	    then z
	 else Cast (z, t)

      val cast = Trace.trace2 ("Operand.cast", layout, Type.layout, layout) cast
	 
      val rec isLocation =
	 fn ArrayOffset _ => true
	  | Cast (z, _) => isLocation z
	  | Offset _ => true
	  | Runtime _ => true
	  | Var _ => true
	  | _ => false

      fun 'a foldVars (z: t, a: 'a, f: Var.t * 'a -> 'a): 'a =
	 case z of
	    ArrayOffset {base, index, ...} =>
	       foldVars (index, foldVars (base, a, f), f)
	  | Cast (z, _) => foldVars (z, a, f)
	  | Offset {base, ...} => foldVars (base, a, f)
	  | Var {var, ...} => f (var, a)
	  | _ => a

      fun foreachVar (z: t, f: Var.t -> unit): unit =
	 foldVars (z, (), f o #1)

      fun caseBytes (z, {big: t -> 'a,
			 small: word -> 'a}): 'a =
	 case z of
	    Const c =>
	       (case Const.node c of
		   Const.Node.Word w =>
		      if w <= 0w512 (* pretty arbitrary *)
			 then small w
		      else big z
		 | _ => Error.bug "strangse numBytes")
	  | _ => big z
   end

structure Switch = Switch (open S
			   structure Use = Operand)

structure Statement =
   struct
      datatype t =
	 Bind of {isMutable: bool,
		  oper: Operand.t,
		  var: Var.t}
       | HandlerPop of Label.t (* the label is redundant, but useful *)
       | HandlerPush of Label.t
       | Move of {dst: Operand.t,
		  src: Operand.t}
       | Object of {dst: Var.t,
		    size: int,
		    stores: {offset: int,
			     value: Operand.t} vector,
		    ty: Type.t,
		    tycon: PointerTycon.t}
       | PrimApp of {args: Operand.t vector,
		     dst: (Var.t * Type.t) option,
		     prim: Prim.t}
       | Profile of ProfileExp.t
       | ProfileLabel of ProfileLabel.t
       | SetExnStackLocal
       | SetExnStackSlot
       | SetHandler of Label.t
       | SetSlotExnStack

      fun hasPrim (s, f) =
	 case s of
	    PrimApp {prim, ...} => f prim
	  | _ => false

      fun 'a foldDefUse (s, a: 'a, {def: Var.t * Type.t * 'a -> 'a,
				    use: Var.t * 'a -> 'a}): 'a =
	 let
	    fun useOperand (z: Operand.t, a) = Operand.foldVars (z, a, use)
	 in
	    case s of
	       Bind {oper, var, ...} =>
		  def (var, Operand.ty oper, useOperand (oper, a))
	     | HandlerPop _ => a
	     | HandlerPush _ => a
	     | Move {dst, src} => useOperand (src, useOperand (dst, a))
	     | Object {dst, stores, ty, ...} =>
		  Vector.fold (stores, def (dst, ty, a),
			       fn ({value, ...}, a) => useOperand (value, a))
	     | PrimApp {dst, args, ...} =>
		  Vector.fold (args,
			       Option.fold (dst, a, fn ((x, t), a) =>
					    def (x, t, a)),
			       useOperand)
	     | Profile _ => a
	     | ProfileLabel _ => a
	     | SetExnStackLocal => a
	     | SetExnStackSlot => a
	     | SetHandler _ => a
	     | SetSlotExnStack => a
	 end

      fun foreachDefUse (s: t, {def, use}) =
	 foldDefUse (s, (), {def = fn (x, t, ()) => def (x, t),
			     use = use o #1})

      fun 'a foldDef (s: t, a: 'a, f: Var.t * Type.t * 'a -> 'a): 'a =
	 foldDefUse (s, a, {def = f, use = #2})

      fun foreachDef (s:t , f: Var.t * Type.t -> unit) =
	 foldDef (s, (), fn (x, t, ()) => f (x, t))

      fun 'a foldUse (s: t, a: 'a, f: Var.t * 'a -> 'a) =
	 foldDefUse (s, a, {def = #3, use = f})

      fun foreachUse (s, f) = foldUse (s, (), f o #1)

      val layout =
	 let
	    open Layout
	    fun constrain ty =
	       if !Control.showTypes
		  then seq [str ": ", Type.layout ty]
	       else empty
	 in
	    fn Bind {oper, var, ...} =>
		  seq [Var.layout var, constrain (Operand.ty oper),
		       str " = ", Operand.layout oper]
	     | HandlerPop l => seq [str "HandlerPop ", Label.layout l]
	     | HandlerPush l => seq [str "HandlerPush ", Label.layout l]
	     | Move {dst, src} =>
		  mayAlign [Operand.layout dst,
			    seq [str " = ", Operand.layout src]]
	     | Object {dst, size, stores, ty, tycon} =>
		  mayAlign
		  [seq [Var.layout dst, constrain ty],
		   seq [str " = Object ",
			record
			[("size", Int.layout size),
			 ("tycon", PointerTycon.layout tycon),
			 ("stores",
			  Vector.layout
			  (fn {offset, value} =>
			   record [("offset", Int.layout offset),
				   ("value", Operand.layout value)])
			  stores)]]]
	     | PrimApp {dst, prim, args, ...} =>
		  let
		     val rest =
			seq [Prim.layout prim, str " ",
			     Vector.layout Operand.layout args]
		  in
		     case dst of
			NONE => rest
		      | SOME (x, t) =>
			   mayAlign [seq [Var.layout x, constrain t],
				     seq [str " = ", rest]]
		  end
	     | Profile e => ProfileExp.layout e
	     | ProfileLabel p =>
		  seq [str "ProfileLabel ", ProfileLabel.layout p]
	     | SetExnStackLocal => str "SetExnStackLocal"
	     | SetExnStackSlot => str "SetExnStackSlot "
	     | SetHandler l => seq [str "SetHandler ", Label.layout l]
	     | SetSlotExnStack => str "SetSlotExnStack "
	 end

      val toString = Layout.toString o layout

      fun clear (s: t) =
	 foreachDef (s, Var.clear o #1)
   end

structure Transfer =
   struct
      datatype t =
	 Arith of {args: Operand.t vector,
		   dst: Var.t,
		   overflow: Label.t,
		   prim: Prim.t,
		   success: Label.t,
		   ty: Type.t}
       | CCall of {args: Operand.t vector,
		   func: CFunction.t,
		   return: Label.t option}
       | Call of {args: Operand.t vector,
		  func: Func.t,
		  return: Return.t}
       | Goto of {dst: Label.t,
		  args: Operand.t vector}
       | Raise of Operand.t vector
       | Return of Operand.t vector
       | Switch of Switch.t

      fun layout t =
	 let
	    open Layout
	 in
	    case t of
	       Arith {args, dst, overflow, prim, success, ty} =>
		  seq [str "Arith ",
		       record [("args", Vector.layout Operand.layout args),
			       ("dst", Var.layout dst),
			       ("overflow", Label.layout overflow),
			       ("prim", Prim.layout prim),
			       ("success", Label.layout success),
			       ("ty", Type.layout ty)]]
	     | CCall {args, func, return} =>
		  seq [str "CCall ",
		       record [("args", Vector.layout Operand.layout args),
			       ("func", CFunction.layout func),
			       ("return", Option.layout Label.layout return)]]
	     | Call {args, func, return} =>
		  seq [Func.layout func, str " ",
		       Vector.layout Operand.layout args,
		       str " ", Return.layout return]
	     | Goto {dst, args} =>
		  seq [Label.layout dst, str " ",
		       Vector.layout Operand.layout args]
	     | Raise xs => seq [str "raise ", Vector.layout Operand.layout xs]
	     | Return xs => seq [str "return ", Vector.layout Operand.layout xs]
	     | Switch s => Switch.layout s
	 end

      val bug =
	 CCall {args = (Vector.new1
			(Operand.Const
			 (Const.fromString "control shouldn't reach here"))),
		func = CFunction.bug,
		return = NONE}

      fun 'a foldDefLabelUse (t, a: 'a,
			      z as {def: Var.t * Type.t * 'a -> 'a,
				    label: Label.t * 'a -> 'a,
				    use: Var.t * 'a -> 'a}): 'a =
	 let
	    fun useVars (xs: Var.t vector, a) =
	       Vector.fold (xs, a, use)
	    fun useOperand (z, a) = Operand.foldVars (z, a, use)
	    fun useOperands (zs: Operand.t vector, a) =
	       Vector.fold (zs, a, useOperand)
	 in
	    case t of
	       Arith {args, dst, overflow, success, ty, ...} =>
		  let
		     val a = label (overflow, a)
		     val a = label (success, a)
		     val a = def (dst, ty, a)
		     val a = useOperands (args, a)
		  in
		     a
		  end
	     | CCall {args, return, ...} =>
		  useOperands (args,
			       case return of
				  NONE => a
				| SOME l => label (l, a))
	     | Call {args, return, ...} =>
		  useOperands (args, Return.foldLabel (return, a, label))
	     | Goto {args, dst, ...} => label (dst, useOperands (args, a))
	     | Raise zs => useOperands (zs, a)
	     | Return zs => useOperands (zs, a)
	     | Switch s => Switch.foldLabelUse (s, a, {label = label,
						       use = useOperand})
	 end

      fun foreachDefLabelUse (t, {def, label, use}) =
	 foldDefLabelUse (t, (), {def = fn (x, t, ()) => def (x, t),
				  label = label o #1,
				  use = use o #1})

      fun foldLabel (t, a, f) = foldDefLabelUse (t, a, {def = #3,
							label = f,
							use = #2})

      fun foreachLabel (t, f) = foldLabel (t, (), f o #1)

      fun foldDef (t, a, f) = foldDefLabelUse (t, a, {def = f,
						      label = #2,
						      use = #2})

      fun foreachDef (t, f) =
	 foldDef (t, (), fn (x, t, ()) => f (x, t))

      fun foldUse (t, a, f) = foldDefLabelUse (t, a, {def = #3,
						      label = #2,
						      use = f})

      fun foreachUse (t, f) = foldUse (t, (), f o #1)

      fun clear (t: t): unit =
	 foreachDef (t, Var.clear o #1)

      fun ifBool (test, {falsee, truee}) =
	 Switch (Switch.Int
		 {cases = Vector.new2 ((0, falsee), (1, truee)),
		  default = NONE,
		  test = test})
	 
      fun ifInt (test, {falsee, truee}) =
	 Switch (Switch.Int
		 {cases = Vector.new1 (0, falsee),
		  default = SOME truee,
		  test = test})
   end

structure Kind =
   struct
      datatype t =
	 Cont of {handler: Handler.t}
       | CReturn of {func: CFunction.t}
       | Handler
       | Jump

      fun layout k =
	 let
	    open Layout
	 in
	    case k of
	       Cont {handler} =>
		  seq [str "Cont ",
		       record [("handler", Handler.layout handler)]]
	     | CReturn {func} =>
		  seq [str "CReturn ",
		       record [("func", CFunction.layout func)]]
	     | Handler => str "Handler"
	     | Jump => str "Jump"
	 end

      fun isFrame (k: t): bool =
	 case k of
	    Cont _ => true
	  | CReturn {func = CFunction.T {mayGC, ...}, ...} => mayGC
	  | Handler => true
	  | Jump => false
   end

local
   open Layout
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
	 T of {args: (Var.t * Type.t) vector,
	       kind: Kind.t,
	       label: Label.t,
	       statements: Statement.t vector,
	       transfer: Transfer.t}

      local
	 fun make f (T r) = f r
      in
	 val args = make #args
	 val kind = make #kind
	 val label = make #label
	 val statements = make #statements
	 val transfer = make #transfer
      end
   
      fun clear (T {args, label, statements, transfer, ...}) =
	 (Vector.foreach (args, Var.clear o #1)
	  ; Label.clear label
	  ; Vector.foreach (statements, Statement.clear)
	  ; Transfer.clear transfer)

      fun hasPrim (T {statements, transfer, ...}, f) =
	 Vector.exists (statements, fn s => Statement.hasPrim (s, f))

      fun layout (T {args, kind, label, statements, transfer, ...}) =
	 let
	    open Layout
	 in
	    align [seq [Label.layout label, str " ",
			Vector.layout (fn (x, t) =>
				       if !Control.showTypes
					  then seq [Var.layout x, str ": ",
						    Type.layout t]
				       else Var.layout x) args,
			str " ", Kind.layout kind, str " = "],
		   indent (align
			   [align
			    (Vector.toListMap (statements, Statement.layout)),
			    Transfer.layout transfer],
			   2)]
	 end
   end

structure Function =
   struct
      datatype t = T of {args: (Var.t * Type.t) vector,
			 blocks: Block.t vector,
			 name: Func.t,
			 raises: Type.t vector option,
			 returns: Type.t vector option,
			 start: Label.t}

      local
	 fun make f (T r) = f r
      in
	 val args = make #args
	 val blocks = make #blocks
	 val name = make #name
	 val raises = make #raises
	 val returns = make #returns
	 val start = make #start
      end

      fun dest (T r) = r
      val new = T

      fun clear (T {name, args, blocks, ...}) =
	 (Func.clear name
	  ; Vector.foreach (args, Var.clear o #1)
	  ; Vector.foreach (blocks, Block.clear))

      fun hasPrim (T {blocks, ...}, pred) =
	 Vector.exists (blocks, fn b => Block.hasPrim (b, pred))

      fun layoutHeader (T {args, name, raises, returns, start, ...}): Layout.t =
	 let
	    open Layout
	 in
	    seq [str "fun ", Func.layout name,
		 str " ", layoutFormals args,
		 if !Control.showTypes
		    then seq [str ": ",
			      record [("raises",
				       Option.layout
				       (Vector.layout Type.layout) raises),
				      ("returns",
				       Option.layout
				       (Vector.layout Type.layout) returns)]]
		 else empty,
		 str " = ", Label.layout start, str " ()"]
	 end

      fun layouts (f as T {blocks, ...}, output) =
	 (output (layoutHeader f)
	  ; Vector.foreach (blocks, fn b =>
			    output (Layout.indent (Block.layout b, 2))))

      fun layout (f as T {blocks, ...}) =
	 let
	    open Layout
	 in
	    align [layoutHeader f,
		   indent (align (Vector.toListMap (blocks, Block.layout)), 2)]
	 end

      fun dfs (T {blocks, start, ...}, v) =
	 let
	    val numBlocks = Vector.length blocks
	    val {get = labelIndex, set = setLabelIndex, rem, ...} =
	       Property.getSetOnce (Label.plist,
				    Property.initRaise ("index", Label.layout))
	    val _ = Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
				     setLabelIndex (label, i))
	    val visited = Array.array (numBlocks, false)
	    fun visit (l: Label.t): unit =
	       let
		  val i = labelIndex l
	       in
		  if Array.sub (visited, i)
		     then ()
		  else
		     let
			val _ = Array.update (visited, i, true)
			val b as Block.T {transfer, ...} =
			   Vector.sub (blocks, i)
			val v' = v b
			val _ = Transfer.foreachLabel (transfer, visit)
			val _ = v' ()
		     in
			()
		     end
	       end
	    val _ = visit start
	    val _ = Vector.foreach (blocks, rem o Block.label)
	 in
	    ()
	 end

      structure Graph = DirectedGraph
      structure Node = Graph.Node
      structure Edge = Graph.Edge

      fun dominatorTree (T {blocks, start, ...}): Block.t Tree.t =
	 let
	    open Dot
	    val g = Graph.new ()
	    fun newNode () = Graph.newNode g
	    val {get = labelNode, ...} =
	       Property.get
	       (Label.plist, Property.initFun (fn _ => newNode ()))
	    val {get = nodeInfo: Node.t -> {block: Block.t},
		 set = setNodeInfo, ...} =
	       Property.getSetOnce
	       (Node.plist, Property.initRaise ("info", Node.layout))
	    val _ =
	       Vector.foreach
	       (blocks, fn b as Block.T {label, ...}=>
		setNodeInfo (labelNode label, {block = b}))
	    val _ =
	       Vector.foreach
	       (blocks, fn b as Block.T {label, transfer, ...} =>
		let
		   val from = labelNode label
		   val _ =
		      Transfer.foreachLabel
		      (transfer, fn to =>
		       (Graph.addEdge (g, {from = from, to = labelNode to})
			; ()))
		in
		   ()
		end)
	 in
	    Graph.dominatorTree (g, {root = labelNode start,
				     nodeValue = #block o nodeInfo})
	 end
   end

structure Program =
   struct
      datatype t =
	 T of {functions: Function.t list,
	       main: Function.t,
	       objectTypes: ObjectType.t vector}

      fun clear (T {functions, main, ...}) =
	 (List.foreach (functions, Function.clear)
	  ; Function.clear main)

      fun hasPrim (T {functions, main, ...}, pred) =
	 let
	    fun has f = Function.hasPrim (f, pred)
	 in
	    has main orelse List.exists (functions, has)
	 end

      fun handlesSignals p =
	 hasPrim (p, fn p =>
		  Prim.name p = Prim.Name.MLton_installSignalHandler)
	 
      fun layouts (T {functions, main, objectTypes, ...},
		   output': Layout.t -> unit): unit =
	 let
	    open Layout
	    val output = output'
	 in
	    output (str "\nObjectTypes:")
	    ; Vector.foreachi (objectTypes, fn (i, ty) =>
			       output (seq [str "pt_", Int.layout i,
					    str " = ", ObjectType.layout ty]))
	    ; output (str "\nMain:")
	    ; Function.layouts (main, output)
	    ; output (str "\nFunctions:")
	    ; List.foreach (functions, fn f => Function.layouts (f, output))
	 end

      structure ExnStack =
	 struct
	    structure ZPoint =
	       struct
		  datatype t = Caller | Me

		  val equals: t * t -> bool = op =
	       
		  val toString =
		     fn Caller => "Caller"
		      | Me => "Me"

		  val layout = Layout.str o toString
	       end

	    structure L = FlatLattice (structure Point = ZPoint)
	    open L
	    structure Point = ZPoint
	 
	    val me = point Point.Me
	    val caller = point Point.Caller
	 end

      structure HandlerLat = FlatLattice (structure Point = Label)

      structure HandlerInfo =
	 struct
	    datatype t = T of {block: Block.t,
			       global: ExnStack.t,
			       handler: HandlerLat.t,
			       slot: ExnStack.t,
			       visited: bool ref}

	    fun new (b: Block.t): t =
	       T {block = b,
		  global = ExnStack.new (),
		  handler = HandlerLat.new (),
		  slot = ExnStack.new (),
		  visited = ref false}

	    fun layout (T {global, handler, slot, ...}) =
	       Layout.record [("global", ExnStack.layout global),
			      ("slot", ExnStack.layout slot),
			      ("handler", HandlerLat.layout handler)]
	 end

      val traceGoto =
	 Trace.trace ("checkHandlers.goto", Label.layout, Unit.layout)
	 
      fun checkHandlers (T {functions, ...}) =
	 let
	    val debug = false
	    fun checkFunction (f: Function.t): unit =
	       let
		  val {name, start, blocks, ...} = Function.dest f
		  val {get = labelInfo: Label.t -> HandlerInfo.t,
		       rem = remLabelInfo, 
		       set = setLabelInfo} =
		     Property.getSetOnce
		     (Label.plist, Property.initRaise ("info", Label.layout))
		  val _ =
		     Vector.foreach
		     (blocks, fn b =>
		      setLabelInfo (Block.label b, HandlerInfo.new b))
		  (* Do a DFS of the control-flow graph. *)
		  fun visitLabel l = visitInfo (labelInfo l)
		  and visitInfo
		     (hi as HandlerInfo.T {block, global, handler, slot,
					   visited, ...}): unit =
		     if !visited
			then ()
		     else
			let
			   val _ = visited := true
			   val Block.T {label, statements, transfer, ...} = block
			   val _ =
			      if debug
				 then
				    let
				       open Layout
				    in
				       outputl
				       (seq [str "visiting ",
					     Label.layout label],
					Out.error)
				    end
			      else ()
			   datatype z = datatype ExnStack.t
			   datatype z = datatype Statement.t
			   val {global, handler, slot} =
			      Vector.fold
			      (statements,
			       {global = global, handler = handler, slot = slot},
			       fn (s, {global, handler, slot}) =>
			       case s of
				  SetExnStackLocal => {global = ExnStack.me,
						       handler = handler,
						       slot = slot}
				| SetExnStackSlot => {global = slot,
						      handler = handler,
						      slot = slot}
				| SetSlotExnStack => {global = global,
						      handler = handler,
						      slot = global}
				| SetHandler l => {global = global,
						   handler = HandlerLat.point l,
						   slot = slot}
				| _ => {global = global,
					handler = handler,
					slot = slot})
			   fun fail msg =
			      (Control.message
			       (Control.Silent, fn () =>
				let open Layout
				in align
				   [str "before: ", HandlerInfo.layout hi,
				    str "block: ", Block.layout block,
				    seq [str "after: ",
					 Layout.record
					 [("global", ExnStack.layout global),
					  ("slot", ExnStack.layout slot),
					  ("handler",
					   HandlerLat.layout handler)]],
				    Vector.layout
				    (fn Block.T {label, ...} =>
				     seq [Label.layout label,
					  str " ",
					  HandlerInfo.layout (labelInfo label)])
				    blocks]
				end)
			       ; Error.bug (concat ["handler mismatch at ", msg]))
			   fun assert (msg, f) =
			      if f
				 then ()
			      else fail msg
			   fun goto (l: Label.t): unit =
			      let
				 val HandlerInfo.T {global = g, handler = h,
						    slot = s, ...} =
				    labelInfo l
				 val _ =
				    assert ("goto",
					    ExnStack.<= (global, g)
					    andalso ExnStack.<= (slot, s)
					    andalso HandlerLat.<= (handler, h))
			      in
				 visitLabel l
			      end
			   val goto = traceGoto goto
			   fun tail name =
			      assert (name,
				      ExnStack.forcePoint
				      (global, ExnStack.Point.Caller))
			   datatype z = datatype Transfer.t
			in
			   case transfer of
			      Arith {overflow, success, ...} =>
				 (goto overflow; goto success)
			    | CCall {return, ...} => Option.app (return, goto)
			    | Call {func, return, ...} =>
				 assert
				 ("return",
				  let
				     datatype z = datatype Return.t
				  in
				     case return of
					Dead => true
				      | NonTail {handler = h, ...} =>
					   (case h of
					       Handler.Caller =>
						  ExnStack.forcePoint
						  (global, ExnStack.Point.Caller)
					     | Handler.Dead => true
					     | Handler.Handle l =>
						  let
						     val res =
							ExnStack.forcePoint
							(global,
							 ExnStack.Point.Me)
							andalso
							HandlerLat.forcePoint
							(handler, l)
						     val _ = goto l
						  in
						     res
						  end)
				      | Tail => true
				  end)
			    | Goto {dst, ...} => goto dst
			    | Raise _ => tail "raise"
			    | Return _ => tail "return"
			    | Switch s => Switch.foreachLabel (s, goto)
			end
		  val info as HandlerInfo.T {global, ...} = labelInfo start
		  val _ = ExnStack.forcePoint (global, ExnStack.Point.Caller)
		  val _ = visitInfo info
		  val _ =
		     Control.diagnostics
		     (fn display =>
		      let
			 open Layout
			 val _ = 
			    display (seq [str "checkHandlers ",
					  Func.layout name])
			 val _ =
			    Vector.foreach
			    (blocks, fn Block.T {label, ...} =>
			     display (seq
				      [Label.layout label,
				       str " ",
				       HandlerInfo.layout (labelInfo label)]))
		      in
			 ()
		      end)
		  val _ = Vector.foreach (blocks, fn b =>
					  remLabelInfo (Block.label b))
	       in
		  ()
	       end
	    val _ = List.foreach (functions, checkFunction)
	 in
	    ()
	 end
	    
      fun checkScopes (program as T {functions, main, ...}): unit =
	 let
	    datatype status =
	       Defined
	     | Global
	     | InScope
	     | Undefined
	    fun make (layout, plist) =
	       let
		  val {get, set, ...} =
		     Property.getSet (plist, Property.initConst Undefined)
		  fun bind (x, isGlobal) =
		     case get x of
			Global => ()
		      | Undefined =>
			   set (x, if isGlobal then Global else InScope)
		      | _ => Error.bug ("duplicate definition of "
					^ (Layout.toString (layout x)))
		  fun reference x =
		     case get x of
			Global => ()
		      | InScope => ()
		      | _ => Error.bug (concat
					["reference to ",
					 Layout.toString (layout x),
					 " not in scope"])
		  fun unbind x =
		     case get x of
			Global => ()
		      | _ => set (x, Defined)
	       in (bind, reference, unbind)
	       end
	    val (bindVar, getVar, unbindVar) = make (Var.layout, Var.plist)
	    val (bindFunc, getFunc, _) = make (Func.layout, Func.plist)
	    val bindFunc = fn f => bindFunc (f, false)
	    val (bindLabel, getLabel, unbindLabel) =
	       make (Label.layout, Label.plist)
	    val bindLabel = fn l => bindLabel (l, false)
	    fun getVars xs = Vector.foreach (xs, getVar)
	    fun loopFunc (f: Function.t, isMain: bool): unit =
	       let
		  val bindVar = fn x => bindVar (x, isMain)
		  val {args, blocks, start, ...} = Function.dest f
		  val _ = Vector.foreach (args, bindVar o #1)
		  val _ = Vector.foreach (blocks, bindLabel o Block.label)
		  val _ =
		     Vector.foreach
		     (blocks, fn Block.T {transfer, ...} =>
		      Transfer.foreachLabel (transfer, getLabel))
		  (* Descend the dominator tree, verifying that variable
		   * definitions dominate variable uses.
		   *)
		  val _ =
		     Tree.traverse
		     (Function.dominatorTree f,
		      fn Block.T {args, statements, transfer, ...} =>
		      let
			 val _ = Vector.foreach (args, bindVar o #1)
			 val _ =
			    Vector.foreach
			    (statements, fn s =>
			     (Statement.foreachUse (s, getVar)
			      ; Statement.foreachDef (s, bindVar o #1)))
			 val _ = Transfer.foreachUse (transfer, getVar)
			 val _ = Transfer.foreachDef (transfer, bindVar o #1)
		      in
			 fn () =>
			 if isMain
			    then ()
			 else
			    let
			       val _ =
				  Vector.foreach
				  (statements, fn s =>
				   Statement.foreachDef (s, unbindVar o #1))
			       val _ =
				  Transfer.foreachDef (transfer, unbindVar o #1)
			       val _ = Vector.foreach (args, unbindVar o #1)
			    in
			       ()
			    end
		      end)
		  val _ = Vector.foreach (blocks, unbindLabel o Block.label)
		  val _ = Vector.foreach (args, unbindVar o #1)
	       in
		  ()
	       end
	    val _ = List.foreach (functions, bindFunc o Function.name)
	    val _ = loopFunc (main, true)
	    val _ = List.foreach (functions, fn f => loopFunc (f, false))
	    val _ = clear program
	 in ()
	 end

      fun typeCheck (p as T {functions, main, objectTypes, ...}) =
	 let
	    val _ =
	       Vector.foreach
	       (objectTypes, fn ty =>
		Err.check ("objectType",
			   fn () => ObjectType.isOk ty,
			   fn () => ObjectType.layout ty))
	    fun tyconTy (pt: PointerTycon.t): ObjectType.t =
	       Vector.sub (objectTypes, PointerTycon.index pt)
	    val _ = checkScopes p
	    val {get = labelBlock: Label.t -> Block.t,
		 set = setLabelBlock, ...} =
	       Property.getSetOnce (Label.plist,
				    Property.initRaise ("block", Label.layout))
	    val {get = funcInfo, set = setFuncInfo, ...} =
	       Property.getSetOnce (Func.plist,
				    Property.initRaise ("info", Func.layout))
	    val {get = varType: Var.t -> Type.t, set = setVarType, ...} =
	       Property.getSetOnce (Var.plist,
				    Property.initRaise ("type", Var.layout))
	    val setVarType =
	       Trace.trace2 ("setVarType", Var.layout, Type.layout,
			     Unit.layout)
	       setVarType
	    fun checkOperand (x: Operand.t): unit =
		let
		   datatype z = datatype Operand.t
		   fun ok () =
		      case x of
			 ArrayOffset z => arrayOffsetIsOk z
		       | Cast (z, ty) =>
			    (checkOperand z
			    ; (castIsOk
			       {from = Operand.ty z,
				fromInt = (case z of
					      Const c =>
						 (case Const.node c of
						     Const.Node.Int n => SOME n
						   | _ => NONE)
					    | _ => NONE),
				to = ty,
				tyconTy = tyconTy}))
		       | Const _ => true
		       | EnsuresBytesFree => true
		       | File => true
		       | GCState => true
		       | Line => true
		       | Offset z => offsetIsOk z
		       | PointerTycon _ => true
		       | Runtime _ => true
		       | SmallIntInf _ => true
		       | Var {ty, var} => Type.equals (ty, varType var)
		in
		   Err.check ("operand", ok, fn () => Operand.layout x)
		end
	    and arrayOffsetIsOk {base, index, ty} =
	       let
		  val _ = checkOperand base
		  val _ = checkOperand index
	       in
		  Type.equals (Operand.ty index, Type.int)
		  andalso
		  case Operand.ty base of
		     Type.CPointer => true (* needed for card marking *)
		   | Type.EnumPointers {enum, pointers} =>
			0 = Vector.length enum
			andalso
			Vector.forall
			(pointers, fn p =>
			 case tyconTy p of
			    ObjectType.Array
			    (MemChunk.T {components, ...}) =>
			       1 = Vector.length components
			       andalso
			       let
				  val {offset, ty = ty', ...} =
				     Vector.sub (components, 0)
			       in
				  offset = 0
				  andalso Type.equals (ty, ty')
			       end
			  | _ => false)
		   | _ => false
	       end
	    and offsetIsOk {base, offset, ty} =
	       let
		  val _ = checkOperand base
		  fun memChunkIsOk (MemChunk.T {components, ...}) =
		     case Vector.peek (components, fn {offset = offset', ...} =>
				       offset = offset') of
			NONE => false
		      | SOME {ty = ty', ...} => Type.equals (ty, ty')
	       in
		  case Operand.ty base of
		     Type.EnumPointers {enum, pointers} =>
			0 = Vector.length enum
			andalso
			((* Vector_fromArray header update. *)
			 (offset = Runtime.headerOffset
			  andalso Type.equals (ty, Type.word))
			 orelse
			 Vector.forall
			 (pointers, fn p =>
			  case tyconTy p of
			     ObjectType.Normal m => memChunkIsOk m
			   | _ => false))
		   | Type.MemChunk m => memChunkIsOk m
		   | _ => false
	       end
	    val checkOperand =
	       Trace.trace ("checkOperand", Operand.layout, Unit.layout)
	       checkOperand
	    fun checkOperands v = Vector.foreach (v, checkOperand)
	    fun check' (x, name, isOk, layout) =
	       Err.check (name, fn () => isOk x, fn () => layout x)
	    val labelKind = Block.kind o labelBlock
	    fun labelIsJump (l: Label.t): bool =
	       case labelKind l of
		  Kind.Jump => true
		| _ => false
	    fun statementOk (s: Statement.t): bool =
	       let
		  datatype z = datatype Statement.t
	       in
		  case s of
		     Bind {oper, ...} => (checkOperand oper; true)
		   | HandlerPop _ => true
		   | HandlerPush _ => true
		   | Move {dst, src} =>
			(checkOperand dst
			 ; checkOperand src
			 ; (Type.equals (Operand.ty dst, Operand.ty src)
			    andalso Operand.isLocation dst))
		   | Object {dst, size, stores, tycon, ...} =>
			(Vector.foreach (stores, checkOperand o # value)
			 ; (case tyconTy tycon of
			       ObjectType.Normal mc =>
				  MemChunk.isValidInit
				  (mc, 
				   Vector.map
				   (stores, fn {offset, value} =>
				    {offset = offset,
				     ty = Operand.ty value}))
			     | _ => false))
		   | PrimApp {args, ...} =>
			(Vector.foreach (args, checkOperand)
			 ; true)
		   | Profile _ => true
		   | ProfileLabel _ => true
		   | SetExnStackLocal => true
		   | SetExnStackSlot => true
		   | SetHandler l =>
			(case labelKind l of
			    Kind.Handler => true
			  | _ => false)
		   | SetSlotExnStack => true
	       end
	    fun goto {args: Type.t vector,
		      dst: Label.t}: bool =
	       let
		  val Block.T {args = formals, kind, ...} = labelBlock dst
	       in
		  Vector.equals (args, formals, fn (t, (_, t')) =>
				 Type.equals (t, t'))
		  andalso (case kind of
			      Kind.Jump => true
			    | _ => false)
	       end
	    fun labelIsNullaryJump l = goto {dst = l, args = Vector.new0 ()}
	    fun tailIsOk (caller: Type.t vector option,
			  callee: Type.t vector option): bool =
	       case (caller, callee) of
		  (_, NONE) => true
		| (SOME ts, SOME ts') => Vector.equals (ts, ts', Type.equals)
		| _ => false
	    fun nonTailIsOk (formals: (Var.t * Type.t) vector,
			     returns: Type.t vector option): bool =
	       case returns of
		  NONE => true
		| SOME ts => 
		     Vector.equals (formals, ts, fn ((_, t), t') =>
				    Type.equals (t, t'))
	    fun callIsOk {args, func, raises, return, returns} =
	       let
		  val Function.T {args = formals,
				  raises = raises',
				  returns = returns', ...} =
		     funcInfo func

	       in
		  Vector.equals (args, formals, fn (z, (_, t)) =>
				 Type.equals (t, Operand.ty z))
		  andalso
		  (case return of
		      Return.Dead =>
			 Option.isNone raises'
			 andalso Option.isNone returns'
		    | Return.NonTail {cont, handler} =>
			 let
			    val Block.T {args = cArgs, kind = cKind, ...} =
			       labelBlock cont
			 in
			    nonTailIsOk (cArgs, returns')
			    andalso
			    (case cKind of
				Kind.Cont {handler = h} =>
				   Handler.equals (handler, h)
				   andalso
				   (case h of
				       Handler.Caller =>
					  tailIsOk (raises, raises')
				     | Handler.Dead => true
				     | Handler.Handle l =>
					  let
					     val Block.T {args = hArgs,
							  kind = hKind, ...} =
						labelBlock l
					  in
					     nonTailIsOk (hArgs, raises')
					     andalso
					     (case hKind of
						 Kind.Handler => true
					       | _ => false)
					  end)
			      | _ => false)
			 end
		    | Return.Tail =>
			 tailIsOk (raises, raises')
			 andalso tailIsOk (returns, returns'))
	       end

	    fun checkFunction (Function.T {args, blocks, raises, returns, start,
					   ...}) =
	       let
		  val _ = Vector.foreach (args, setVarType)
		  val _ =
		     Vector.foreach
		     (blocks, fn b as Block.T {args, label, statements,
					       transfer, ...} =>
		      (setLabelBlock (label, b)
		       ; Vector.foreach (args, setVarType)
		       ; Vector.foreach (statements, fn s =>
					 Statement.foreachDef
					 (s, setVarType))
		       ; Transfer.foreachDef (transfer, setVarType)))
		  val _ = labelIsNullaryJump start
		  fun transferOk (t: Transfer.t): bool =
		     let
			datatype z = datatype Transfer.t
		     in
			case t of
			   Arith {args, dst, overflow, prim, success, ty} =>
			      let
				 val _ = checkOperands args
			      in
				 Prim.mayOverflow prim
				 andalso labelIsNullaryJump overflow
				 andalso labelIsNullaryJump success
				 andalso
				 Vector.forall (args, fn x =>
						Type.equals (ty, Operand.ty x))
			      end
			 | CCall {args, func, return} =>
			      let
				 val _ = checkOperands args
			      in
				 CFunction.isOk func
				 andalso
				 case return of
				    NONE => true
				  | SOME l =>
				       case labelKind l of
					  Kind.CReturn {func = f} =>
					     CFunction.equals (func, f)
					| _ => false
			      end
			 | Call {args, func, return} =>
			      let
				 val _ = checkOperands args
			      in
				 callIsOk {args = args,
					   func = func,
					   raises = raises,
					   return = return,
					   returns = returns}
			      end
			 | Goto {args, dst} =>
			      (checkOperands args
			       ; goto {args = Vector.map (args, Operand.ty),
				       dst = dst})
			 | Raise zs =>
			      (checkOperands zs
			       ; (case raises of
				     NONE => false
				   | SOME ts =>
					Vector.equals
					(zs, ts, fn (z, t) =>
					 Type.equals (t, Operand.ty z))))
			 | Return zs =>
			      (checkOperands zs
			       ; (case returns of
				     NONE => false
				   | SOME ts =>
					Vector.equals
					(zs, ts, fn (z, t) =>
					 Type.equals (t, Operand.ty z))))
			 | Switch s =>
			      Switch.isOk (s, {checkUse = checkOperand,
					       labelIsOk = labelIsNullaryJump})
		     end
		  fun blockOk (Block.T {args, kind, label, 
					statements, transfer, ...}): bool =
		     let
			fun kindOk (k: Kind.t): bool =
			   let
			      datatype z = datatype Kind.t
			      val _ =
				 case k of
				    Cont _ => true
				  | CReturn _ => true
				  | Handler => true
				  | Jump => true
			   in
			      true
			   end
			val _ = check' (kind, "kind", kindOk, Kind.layout)
			val _ =
			   Vector.foreach
			   (statements, fn s =>
			    check' (s, "statement", statementOk,
				    Statement.layout))
			val _ = check' (transfer, "transfer", transferOk,
					Transfer.layout)
		     in
			true
		     end

		  val _ = 
		     Vector.foreach
		     (blocks, fn b =>
		      check' (b, "block", blockOk, Block.layout))
	       in
		  ()
	       end
	    val _ =
	       List.foreach
	       (functions, fn f as Function.T {name, ...} =>
		setFuncInfo (name, f))
	    val _ = checkFunction main
	    val _ = List.foreach (functions, checkFunction)
	    val _ =
	       check'
	       (main, "main function",
		fn f =>
		let
		   val {args, ...} = Function.dest f
		in
		   0 = Vector.length args
		end,
		Function.layout)
	    val _ = clear p
	 in
	    ()
	 end handle Err.E e => (Layout.outputl (Err.layout e, Out.error)
				; Error.bug "Rssa type error")
   end

end
