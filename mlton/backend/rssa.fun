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
	 ArrayHeader of {numBytesNonPointers: int,
			 numPointers: int}
       | ArrayOffset of {base: Var.t,
			 index: Var.t,
			 ty: Type.t}
       | CastInt of t
       | CastWord of t
       | Const of Const.t
       | EnsuresBytesFree
       | File
       | GCState
       | Line
       | Offset of {base: Var.t,
		    bytes: int,
		    ty: Type.t}
       | Pointer of int
       | Runtime of GCField.t
       | Var of {var: Var.t,
		 ty: Type.t}

      val char = Const o Const.fromChar
      val int = Const o Const.fromInt
      val word = Const o Const.fromWord
      fun bool b = int (if b then 1 else 0)
	 
      val rec toString =
	 fn ArrayHeader {numBytesNonPointers, numPointers} =>
	       concat ["AH (",
		       Int.toString numBytesNonPointers,
		       ", ",
		       Int.toString numPointers,
		       ")"]
	  | ArrayOffset {base, index, ty} =>
	       concat ["X", Type.name ty, 
		       "(", Var.toString base, ",", Var.toString index, ")"]
	  | CastInt z => concat ["CastInt ", toString z]
	  | CastWord z => concat ["CastWord ", toString z]
	  | Const c => Const.toString c
	  | EnsuresBytesFree => "<EnsuresBytesFree>"
	  | File => "<File>"
	  | GCState => "<GCState>"
	  | Line => "<Line>"
	  | Offset {base, bytes, ty} =>
	       concat ["O", Type.name ty,
		       "(", Var.toString base, ",", Int.toString bytes, ")"]
	  | Pointer n => concat ["IntAsPointer (", Int.toString n, ")"]
	  | Runtime r => GCField.toString r
	  | Var {var, ...} => Var.toString var

      val layout: t -> Layout.t = Layout.str o toString

      val rec isLocation =
	 fn ArrayOffset _ => true
	  | CastWord z => isLocation z
	  | Offset _ => true
	  | Runtime _ => true
	  | Var _ => true
	  | _ => false

      val ty =
	 fn ArrayHeader _ => Type.word
	  | ArrayOffset {ty, ...} => ty
	  | CastInt _ => Type.int
	  | CastWord _ => Type.word
	  | Const c =>
	       let
		  datatype z = datatype Const.Node.t
	       in
		  case Const.node c of
		     Char _ => Type.char
		   | Int _ => Type.int
		   | IntInf _ => Type.pointer
		   | Real _ => Type.double
		   | String _ => Type.pointer
		   | Word _ =>
			let
			   val t = Const.tycon c
			in
			   if Tycon.equals (t, Tycon.word)
			      then Type.uint
			   else if Tycon.equals (t, Tycon.word8)
				   then Type.char
				else Error.bug "strange word"
			end
	       end
	  | EnsuresBytesFree => Type.word
	  | File => Type.pointer
	  | GCState => Type.pointer
	  | Line => Type.int
	  | Offset {ty, ...} => ty
	  | Pointer _ => Type.pointer
	  | Runtime z => GCField.ty z
	  | Var {ty, ...} => ty

      fun 'a foldVars (z: t, a: 'a, f: Var.t * 'a -> 'a): 'a =
	 case z of
	    ArrayOffset {base, index, ...} => f (index, f (base, a))
	  | CastInt z => foldVars (z, a, f)
	  | CastWord z => foldVars (z, a, f)
	  | Offset {base, ...} => f (base, a)
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

structure Statement =
   struct
      datatype t =
	 Bind of {isMutable: bool,
		  oper: Operand.t,
		  var: Var.t}
       | Move of {dst: Operand.t,
		  src: Operand.t}
       | Object of {dst: Var.t,
		    numPointers: int,
		    numWordsNonPointers: int,
		    stores: {offset: int,
			     value: Operand.t} vector}
       | PrimApp of {args: Operand.t vector,
		     dst: (Var.t * Type.t) option,
		     prim: Prim.t}
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
	     | Move {dst, src} => useOperand (src, useOperand (dst, a))
	     | Object {dst, stores, ...} =>
		  Vector.fold (stores, def (dst, Type.pointer, a),
			       fn ({value, ...}, a) => useOperand (value, a))
	     | PrimApp {dst, args, ...} =>
		  Vector.fold (args,
			       Option.fold (dst, a, fn ((x, t), a) =>
					    def (x, t, a)),
			       useOperand)
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
	 in
	    fn Bind {oper, var, ...} =>
		  seq [Var.layout var, str " = ", Operand.layout oper]
	     | Move {dst, src} =>
		  seq [Operand.layout dst, str " = ", Operand.layout src]
	     | Object {dst, numPointers, numWordsNonPointers, stores, ...} =>
		  seq [Var.layout dst, str " = Object ",
		       tuple [Int.layout numWordsNonPointers,
			      Int.layout numPointers],
		       str " ",
		       Vector.layout (fn {offset, value} =>
				      record [("offset", Int.layout offset),
					      ("value", Operand.layout value)])
		       stores]
	     | PrimApp {dst, prim, args, ...} =>
		  seq [(case dst of
			   NONE => empty
			 | SOME (x, _) => seq [Var.layout x, str " = "]),
		       Prim.layout prim, str " ",
		       Vector.layout Operand.layout args]
	     | SetExnStackLocal => str "SetExnStackLocal"
	     | SetExnStackSlot => str "SetExnStackSlot "
	     | SetHandler l => seq [str "SetHandler ", Label.layout l]
	     | SetSlotExnStack => str "SetSlotExnStack "
	 end

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
       | Call of {func: Func.t,
		  args: Operand.t vector,
		  return: Return.t}
       | Goto of {dst: Label.t,
		  args: Operand.t vector}
       | Raise of Operand.t vector
       | Return of Operand.t vector
       | Switch of {cases: Cases.t,
		    default: Label.t option,
		    test: Operand.t}
       | SwitchIP of {int: Label.t,
		      pointer: Label.t,
		      test: Operand.t}

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
	     | Call {args, func, return, ...} =>
		  let
		     val call = seq [Func.layout func, str " ",
				     Vector.layout Operand.layout args]
		     val call =
			case return of
			   Return.Dead => seq [str "Dead ", call]
			 | Return.HandleOnly => seq [str "HandleOnly ", call]
			 | Return.Tail => call
			 | Return.NonTail {cont, handler} => 
			      let
				 val call =
				    seq [Label.layout cont, str " ", paren call]
			      in
				 case handler of
				    Handler.CallerHandler => call
				  | Handler.Handle l =>
				       seq [call, str " handle ", Label.layout l]
				  | Handler.None => seq [call, str " None"]
			      end
		  in
		     call
		  end
	     | Goto {dst, args} =>
		  seq [Label.layout dst, str " ",
		       Vector.layout Operand.layout args]
	     | Raise xs => seq [str "Raise", Vector.layout Operand.layout xs]
	     | Return xs => seq [str "Return ", Vector.layout Operand.layout xs]
	     | Switch {test, cases, default} =>
		  seq [str "Switch ",
		       tuple [Operand.layout test,
			      Cases.layout cases,
			      Option.layout Label.layout default]]
	     | SwitchIP {test, int, pointer} =>
		  seq [str "SwitchIP ", tuple [Operand.layout test,
					       Label.layout int,
					       Label.layout pointer]]
	 end

      val bug =
	 CCall {args = (Vector.new1
			(Operand.Const
			 (Const.fromString "control shouldn't reach here"))),
		func = CFunction.bug,
		return = NONE}

      fun 'a foldDefLabelUse (t, a: 'a, {def: Var.t * Type.t * 'a -> 'a,
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
	     | Switch {cases, default, test, ...} =>
		  let
		     val a = useOperand (test, a)
		     val a = Option.fold (default, a, label)
		     val a = Cases.fold (cases, a, label)
		  in
		     a
		  end
	     | SwitchIP {int, pointer, test, ...} =>
		  label (int, label (pointer, useOperand (test, a)))
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

      fun iff (test, {falsee, truee}) =
	 Switch {cases = Cases.Int [(0, falsee), (1, truee)],
		 default = NONE,
		 test = test}
   end

structure Kind =
   struct
      datatype t =
	 Cont of {handler: Label.t option}
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
		       record [("handler", Option.layout Label.layout handler)]]
	     | CReturn {func} =>
		  seq [str "CReturn ",
		       record [("func", CFunction.layout func)]]
	     | Handler => str "Handler"
	     | Jump => str "Jump"
	 end
   end

structure Block =
   struct
      datatype t =
	 T of {args: (Var.t * Type.t) vector,
	       kind: Kind.t,
	       label: Label.t,
	       statements: Statement.t vector,
	       profileInfo: {ssa: {func: string, label: string}},
	       transfer: Transfer.t}

      local
	 fun make f (T r) = f r
      in
	 val args = make #args
	 val kind = make #kind
	 val label = make #label
	 val statements = make #statements
	 val profileInfo = make #profileInfo
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

      fun allocTooLarge newBlocks =
	 let
	    val r: Label.t option ref = ref NONE
	 in
	    (fn () => r := NONE,
	     fn () =>
	     case !r of
		SOME l => l
	      | NONE =>
		   let
		      val l = Label.newNoname ()
		      val _ = r := SOME l
		      val profileInfo =
			 {ssa = {func = "AllocTooLarge",
				 label = "AllocTooLarge"}}
		      val cfunc =
			 CFunction.T {bytesNeeded = NONE,
				      ensuresBytesFree = false,
				      mayGC = false,
				      maySwitchThreads = false,
				      modifiesFrontier = false,
				      modifiesStackTop = false,
				      name = "MLton_allocTooLarge",
				      needsArrayInit = false,
				      returnTy = NONE}
		      val _ =
			 newBlocks :=
			 T {args = Vector.new0 (),
			    kind = Kind.Jump,
			    label = l,
			    profileInfo = profileInfo,
			    statements = Vector.new0 (),
			    transfer =
			    Transfer.CCall {args = Vector.new0 (),
					    func = cfunc,
					    return = NONE}}
			 :: !newBlocks
		   in
		      l
		   end)
	 end
   end

structure Function =
   struct
      datatype t = T of {args: (Var.t * Type.t) vector,
			 blocks: Block.t vector,
			 name: Func.t,
			 start: Label.t}

      local
	 fun make f (T r) = f r
      in
	 val args = make #args
	 val blocks = make #blocks
	 val name = make #name
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

      fun layout (T {args, blocks, name, start}): Layout.t =
	 let
	    open Layout
	 in
	    align
	    [seq [Func.layout name,
		  Vector.layout (Layout.tuple2 (Var.layout, Type.layout)) args,
		  str " = ",
		  Label.layout start,
		  str " ()"],
	     indent (align (Vector.toListMap (blocks, Block.layout)),
		     2)]
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
      datatype t = T of {functions: Function.t list,
			 main: Function.t}

      fun clear (T {functions, main, ...}) =
	 (List.foreach (functions, Function.clear)
	  ; Function.clear main)

      fun hasPrim (T {functions, main}, pred) =
	 let
	    fun has f = Function.hasPrim (f, pred)
	 in
	    has main orelse List.exists (functions, has)
	 end

      fun handlesSignals p =
	 hasPrim (p, fn p => Prim.name p = Prim.Name.MLton_installSignalHandler)
	 
      fun layouts (T {functions, main}, output': Layout.t -> unit): unit =
	 let
	    open Layout
	    val output = output'
	 in
	    output (str "Main:")
	    ; output (Function.layout main)
	    ; output (str "\n\nFunctions:")
	    ; List.foreach (functions, output o Function.layout)
	 end
	    
      fun checkScopes (program as T {functions, main}): unit =
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

      fun typeCheck (p as T {functions, main}) =
	 let
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
			 ArrayHeader {numBytesNonPointers = nbnp, numPointers = np} =>
			    nbnp >= 0 andalso np >= 0
			    
		       | ArrayOffset {base, index, ty} =>
			    Type.equals (varType base, Type.pointer)
			    andalso Type.equals (varType index, Type.int)
		       | CastInt z => Type.equals (Operand.ty z, Type.pointer)
		       | CastWord z =>
			    Type.equals (Operand.ty z, Type.pointer)
			    orelse Type.equals (Operand.ty z, Type.int)
		       | Const _ => true
		       | EnsuresBytesFree => true
		       | File => true
		       | GCState => true
		       | Line => true
		       | Offset {base, ...} =>
			    Type.equals (varType base, Type.pointer)
		       | Pointer n => 0 < Int.rem (n, Runtime.wordSize)
		       | Runtime _ => true
		       | Var {ty, var} => Type.equals (ty, varType var)
		in
		   Err.check ("operand", ok, fn () => Operand.layout x)
		end
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
		   | Move {dst, src} =>
			(checkOperand dst
			 ; checkOperand src
			 ; (Type.equals (Operand.ty dst, Operand.ty src)
			    andalso Operand.isLocation dst))
		   | Object {dst, numPointers, numWordsNonPointers, stores} =>
			 (Vector.foreach (stores, fn {offset, value} =>
					  checkOperand value)
			  ; (numPointers >= 0
			     andalso numWordsNonPointers >= 0))
		   | PrimApp {args, ...} =>
			(Vector.foreach (args, checkOperand)
			 ; true)
		   | SetExnStackLocal => true
		   | SetExnStackSlot => true
		   | SetHandler l =>
			(case labelKind l of
			    Kind.Handler => true
			  | _ => false)
		   | SetSlotExnStack => true
	       end
	    fun goto {dst, args} =
	       let
		  val Block.T {args = formals, kind, ...} = labelBlock dst
	       in
		  Vector.equals (args, formals, fn (z, (_, t)) =>
				 Type.equals (t, Operand.ty z))
		  andalso (case kind of
			      Kind.Jump => true
			    | _ => false)
	       end
	    fun labelIsNullaryJump l = goto {dst = l, args = Vector.new0 ()}
	    fun transferOk (t: Transfer.t): bool =
	       let
		  datatype z = datatype Transfer.t
	       in
		  case t of
		     Arith {args, dst, overflow, prim, success, ty} =>
			Prim.mayOverflow prim
			andalso labelIsNullaryJump overflow
			andalso labelIsNullaryJump success
			andalso
			Vector.forall (args, fn x =>
				       Type.equals (ty, Operand.ty x))
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
			   val Function.T {args = formals, ...} = funcInfo func
			in
			   Vector.equals (args, formals, fn (z, (_, t)) =>
					  Type.equals (t, Operand.ty z))
			   andalso
			   (case return of
			       Return.Dead => true
			     | Return.HandleOnly => true
			     | Return.NonTail {cont, handler = h} =>
				  (case labelKind cont of
				      Kind.Cont {handler = h'} =>
					 (case (h, h') of
					     (Handler.CallerHandler, NONE) =>
						true
					   | (Handler.None, NONE) => true
					   | (Handler.Handle l, SOME l') =>
						Label.equals (l, l')
					   | _ => false)
				    | _ => false)
			     | Return.Tail => true)
			end
		   | Goto z => goto z
		   | Raise _ => true
		   | Return _ => true
		   | Switch {cases, default, test} =>
			(Cases.forall (cases, labelIsNullaryJump)
			 andalso Option.forall (default, labelIsNullaryJump)
			 andalso (Type.equals
				  (Operand.ty test,
				   case cases of
				      Cases.Char _ => Type.char
				    | Cases.Int _ => Type.int
				    | Cases.Word _ => Type.uint)))
		   | SwitchIP {int, pointer, test} =>
			(checkOperand test
			 ; (labelIsNullaryJump pointer
			    andalso labelIsNullaryJump int
			    andalso Type.equals (Type.pointer,
						 Operand.ty test)))
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
		      check' (s, "statement", statementOk, Statement.layout))
		  val _ = check' (transfer, "transfer", transferOk,
				  Transfer.layout)
	       in
		  true
	       end
	    fun checkFunction (Function.T {args, blocks, start, ...}) =
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
		  val _ = 
		     Vector.foreach
		     (blocks, fn b => check' (b, "block", blockOk, Block.layout))
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
