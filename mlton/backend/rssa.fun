functor Rssa (S: RSSA_STRUCTS): RSSA =
struct

open S

structure Operand =
   struct
      datatype t =
	 ArrayOffset of {base: Var.t,
			 index: Var.t,
			 ty: Type.t}
       | CastInt of Var.t
       | Const of Const.t
       | Offset of {base: Var.t,
		    bytes: int,
		    ty: Type.t}
       | Var of {var: Var.t,
		 ty: Type.t}
    
      val int = Const o Const.fromInt

      val toString =
	 fn ArrayOffset {base, index, ty} =>
	       concat ["X", Type.name ty, 
		       "(", Var.toString base, ",", Var.toString index, ")"]
	  | CastInt x => concat [ "CastInt ", Var.toString x]
	  | Const c => Const.toString c
	  | Offset {base, bytes, ty} =>
	       concat ["O", Type.name ty,
		       "(", Var.toString base, ",", Int.toString bytes, ")"]
	  | Var {var, ...} => Var.toString var

      val layout: t -> Layout.t = Layout.str o toString

      val ty =
	 fn ArrayOffset {ty, ...} => ty
	  | CastInt _ => Type.int
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
	  | Offset {ty, ...} => ty
	  | Var {ty, ...} => ty

      fun 'a foldVars (z: t, a: 'a, f: Var.t * 'a -> 'a): 'a =
	 case z of
	    ArrayOffset {base, index, ...} => f (index, f (base, a))
	  | CastInt x => f (x, a)
	  | Const _ => a
	  | Offset {base, ...} => f (base, a)
	  | Var {var, ...} => f (var, a)

      fun foreachVar (z: t, f: Var.t -> unit): unit =
	 foldVars (z, (), f o #1)
   end

structure Statement =
   struct
      datatype t =
	 Array of {dst: Var.t,
		   numBytesNonPointers: int,
		   numElts: Var.t,
		   numPointers: int}
       | Move of {dst: Operand.t,
		  src: Operand.t}
       | Object of {dst: Var.t,
		    numPointers: int,
		    numWordsNonPointers: int,
		    stores: {offset: int,
			     value: Operand.t} vector}
       | PrimApp of {dst: (Var.t * Type.t) option,
		     prim: Prim.t,
		     args: Var.t vector}
       | SetExnStackLocal
       | SetExnStackSlot
       | SetHandler of Label.t
       | SetSlotExnStack

      fun foldDef (s, a, f) =
	 case s of
	    Array {dst, ...} => f ({var = dst, ty = Type.pointer}, a)
	  | Move {dst, ...} =>
	       (case dst of
		   Operand.Var x => f (x, a)
		 | _ => a)
	  | Object {dst, ...} => f ({var = dst, ty = Type.pointer}, a)
	  | PrimApp {dst, ...} =>
	       Option.fold (dst, a, fn ((x, t), a) =>
			    f ({var = x, ty = t}, a))
	  | _ => a

      fun foreachDef (s, f) = foldDef (s, (), fn (x, ()) => f x)

      fun foldUses (s, a, f) =
	 let
	    fun oper (z, a) = Operand.foldVars (z, a, f)
	 in
	    case s of
	       Array {numElts, ...} => f (numElts, a)
	     | Move {dst, src} =>
		  oper (src,
			case dst of
			   Operand.Var _ => a
			 | _ => oper (dst, a))
	     | Object {stores, ...} =>
		  Vector.fold (stores, a, fn ({value, ...}, a) =>
			       oper (value, a))
	     | PrimApp {args, ...} => Vector.fold (args, a, f)
	     | _ => a
	 end

      fun foreachUse (s, f) = foldUses (s, (), fn (x, ()) => f x)

      val layout =
	 let
	    open Layout
	 in
	    fn Array {dst, ...} => seq [Var.layout dst, str " = Array"]
	     | Move {dst, src} =>
		  seq [Operand.layout dst, str " = ", Operand.layout src]
	     | Object {dst, ...} => seq [Var.layout dst, str " = Object"]
	     | PrimApp {dst, prim, args, ...} =>
		  seq [(case dst of
			   NONE => empty
			 | SOME (x, _) => seq [Var.layout x, str " = "]),
		       Prim.layout prim, str " ",
		       Vector.layout Var.layout args]
	     | SetExnStackLocal => str "SetExnStackLocal"
	     | SetExnStackSlot => str "SetExnStackSlot "
	     | SetHandler l => seq [str "SetHandler ", Label.layout l]
	     | SetSlotExnStack => str "SetSlotExnStack "
	 end

      fun clear (s: t) =
	 foreachDef (s, Var.clear o #var)
   end

structure LimitCheck =
   struct
      datatype t =
	 Array of {bytesPerElt: int,
		   extraBytes: int,
		   numElts: Var.t,
		   stackToo: bool}

       | Heap of {bytes: int,
		  stackToo: bool}
       | Signal
       | Stack

      fun foldVar (l, a, f) =
	 case l of
	    Array {numElts, ...} => f (numElts, a)
	  | _ => a

      fun foreachVar (l, f) = foldVar (l, (), fn (x, ()) => f x)

      fun layout (l: t): Layout.t =
	 let
	    open Layout
	 in
	    case l of
	       Array {bytesPerElt, extraBytes, numElts, stackToo} =>
		  seq [str "Array ",
		       record [("bytesPerElt", Int.layout bytesPerElt),
			       ("extraBytes", Int.layout extraBytes),
			       ("numElts", Var.layout numElts),
			       ("stackToo", Bool.layout stackToo)]]
	     | Heap {bytes, stackToo} =>
		  seq [str "Heap ",
		       record [("bytes", Int.layout bytes),
			       ("stackToo", Bool.layout stackToo)]]
	     | Signal => str "Signal"
	     | Stack => str "Stack"
	 end
   end

structure Transfer =
   struct
      datatype t =
	 Arith of {dst: Var.t,
		   prim: Prim.t,
		   args: Var.t vector,
		   overflow: Label.t,
		   success: Label.t
		   }
       | Bug
       | CCall of {args: Operand.t vector,
		   prim: Prim.t,
		   return: Label.t,
		   returnTy: Type.t option}
       | Call of {func: Func.t,
		  args: Operand.t vector,
		  return: Return.t}
       | Goto of {dst: Label.t,
		  args: Operand.t vector}
       | LimitCheck of {failure: Label.t,
			kind: LimitCheck.t,
			success: Label.t}
       | Raise of Operand.t vector
       | Return of Operand.t vector
       | Runtime of {args: Operand.t vector,
		     prim: Prim.t,
		     return: Label.t}
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
	       Arith {prim, args, dst, overflow, success} =>
		  seq [str "Arith ",
		       record [("prim", Prim.layout prim),
			       ("args", Vector.layout Var.layout args),
			       ("dst", Var.layout dst),
			       ("overflow", Label.layout overflow),
			       ("success", Label.layout success)]]
	     | Bug => str "Bug"
	     | CCall {args, prim, return, returnTy} =>
		  seq [str "CCall ",
		       record [("args", Vector.layout Operand.layout args),
			       ("prim", Prim.layout prim),
			       ("return", Label.layout return),
			       ("returnTy", Option.layout Type.layout returnTy)]]
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
	     | LimitCheck {failure, kind, success} =>
		  seq [str "LimitCheck ",
		       record [("failure", Label.layout failure),
			       ("kind", LimitCheck.layout kind),
			       ("success", Label.layout success)]]
	     | Raise xs => seq [str "Raise", Vector.layout Operand.layout xs]
	     | Return xs => seq [str "Return ", Vector.layout Operand.layout xs]
	     | Runtime {args, prim, return} =>
		  seq [str "Runtime ",
		       record [("args", Vector.layout Operand.layout args),
			       ("prim", Prim.layout prim),
			       ("return", Label.layout return)]]
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

      fun foreachLabel (t, f) =
	 case t of
	    Arith {overflow, success, ...} =>
	       (f overflow; f success)
	  | Bug => ()
	  | CCall {return, ...} => f return
	  | Call {return, ...} => Return.foreachLabel (return, f)
	  | Goto {dst, ...} => f dst
	  | LimitCheck {failure, success, ...} => (f failure; f success)
	  | Raise _ => ()
	  | Return _ => ()
	  | Runtime {return, ...} => f return
	  | Switch {cases, default, ...} =>
	       (Option.app (default, f)
		; Cases.foreach (cases, f))
	  | SwitchIP {int, pointer, ...} =>
	       (f int; f pointer)

      fun foldDef (t, a, f) =
	 case t of
	    Arith {dst, ...} => f ({var = dst, ty = Type.int}, a)
	  | _ => a

      fun foreachDef (t, f) = foldDef (t, (), fn (x, ()) => f x)

      fun foldUses (t, a, f) =
	 let
	    fun oper (z, a) = Operand.foldVars (z, a, f)
	    fun opers zs = Vector.fold (zs, a, oper)
	 in
	    case t of
	       Arith {args, ...} => Vector.fold (args, a, f)
	     | Bug => a
	     | CCall {args, ...} => opers args
	     | Call {args, ...} => opers args
	     | Goto {args, ...} => opers args
	     | LimitCheck {kind, ...} => LimitCheck.foldVar (kind, a, f)
	     | Raise zs => opers zs
	     | Return zs => opers zs
	     | Runtime {args, ...} => opers args
	     | Switch {test, ...} => oper (test, a)
	     | SwitchIP {test, ...} => oper (test, a)
	 end

      fun foreachUse (t, f) = foldUses (t, (), fn (x, ()) => f x)

      fun clear (t: t): unit =
	 foreachDef (t, Var.clear o #var)
   end

structure Kind =
   struct
      datatype t =
	 Cont of {handler: Label.t option}
       | CReturn of {prim: Prim.t}
       | Handler
       | Jump
       | Runtime of {prim: Prim.t}

      fun layout k =
	 let
	    open Layout
	 in
	    case k of
	       Cont {handler} =>
		  seq [str "Cont ",
		       record [("handler", Option.layout Label.layout handler)]]
	     | CReturn {prim} =>
		  seq [str "CReturn ",
		       record [("prim", Prim.layout prim)]]
	     | Handler => str "Handler"
	     | Jump => str "Jump"
	     | Runtime {prim} =>
		  seq [str "Runtime ",
		       record [("prim", Prim.layout prim)]]
	 end
      
      val isOnStack =
	 fn Cont _ => true
	  | CReturn _ => false
	  | Handler => true
	  | Jump => false
	  | Runtime _ => true
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

      fun layout (T {args, kind, label, statements, transfer}) =
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

      fun dominatorTree (T {args, blocks, name, start, ...}): Block.t Tree.t =
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
	       (blocks, fn b as Block.T {label, transfer, ...} =>
		let
		   val from = labelNode label
		   val _ = setNodeInfo (from, {block = b})
		   val _ =
		      Transfer.foreachLabel
		      (transfer, fn to =>
		       (Graph.addEdge (g, {from = from, to = labelNode to})
			; ()))
		in
		   ()
		end)
	    val root = labelNode start
	 in
	    Graph.dominatorTree (g, {root = root,
				     nodeValue = #block o nodeInfo})
	 end
   end

structure Program =
   struct
      datatype t = T of {functions: Function.t list,
			 main: Func.t}

      fun clear (T {functions, ...}) =
	 List.foreach (functions, Function.clear)
	 
      fun layouts (T {functions, main}, output': Layout.t -> unit): unit =
	 let
	    open Layout
	    val output = output'
	 in
	    output (seq [str "\n\nMain: ", Func.layout main])
	    ; output (str "\n\nFunctions:")
	    ; List.foreach (functions, output o Function.layout)
	 end
	    
      fun checkScopes (program as T {functions, main}): unit =
	 let
	    datatype status =
	       Undefined
	     | InScope
	     | Defined
	    fun make (layout, plist) =
	       let
		  val {get, set, ...} =
		     Property.getSet (plist, Property.initConst Undefined)
		  fun bind x =
		     case get x of
			Undefined => set (x, InScope)
		      | _ => Error.bug ("duplicate definition of "
					^ (Layout.toString (layout x)))
		  fun reference x =
		     case get x of
			InScope => ()
		      | _ => Error.bug (concat
					["reference to ",
					 Layout.toString (layout x),
					 " not in scope"])

		  fun unbind x = set (x, Defined)
	       in (bind, reference, unbind)
	       end
	    val (bindVar, getVar, unbindVar) = make (Var.layout, Var.plist)
	    val (bindFunc, getFunc, _) = make (Func.layout, Func.plist)
	    val (bindLabel, getLabel, unbindLabel) =
	       make (Label.layout, Label.plist)
	    fun getVars xs = Vector.foreach (xs, getVar)
	    fun loopFunc (f: Function.t, isMain) =
	       let
		  val {name, args, start, blocks, ...} = Function.dest f
		  (* Descend the dominator tree, verifying that variable
		   * definitions dominate variable uses.
		   *)
		  val _ = Vector.foreach (args, bindVar o #1)
		  val _ = Vector.foreach (blocks, bindLabel o Block.label)
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
			      ; Statement.foreachDef (s, bindVar o #var)))
			 val _ = Transfer.foreachUse (transfer, getVar)
			 val _ = Transfer.foreachDef (transfer, bindVar o #var)
			 val _ = Transfer.foreachLabel (transfer, getLabel)
		      in
			 fn () =>
			 if isMain
			    then ()
			 else
			    let
			       val _ =
				  Vector.foreach
				  (statements, fn s =>
				   Statement.foreachDef (s, unbindVar o #var))
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
	    val mainF =
	       case List.peek (functions, fn f =>
			       Func.equals (main, Function.name f)) of
		  NONE => Error.bug "missing main"
		| SOME f => f
	    val _ = loopFunc (mainF, true)
	    val _ = List.foreach (functions, fn f =>
				  if Func.equals (main, Function.name f)
				     then ()
				  else loopFunc (f, false))
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
	    fun operandType (x: Operand.t): Type.t =
	       let
		  datatype z = datatype Operand.t
	       in
		  case x of
		     ArrayOffset {ty, ...} => ty
		   | CastInt x => Type.int
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
		   | Offset {ty, ...} => ty
		   | Var {var, ...} => varType var
	       end
	    fun checkOperand (x: Operand.t): unit =
		let
		   datatype z = datatype Operand.t
		   fun ok () =
		      case x of
			 ArrayOffset {base, index, ty} =>
			    Type.equals (varType base, Type.pointer)
			    andalso Type.equals (varType index, Type.int)
		       | CastInt x => Type.equals (varType x, Type.pointer)
		       | Const c => true
		       | Offset {base, ...} =>
			    Type.equals (varType base, Type.pointer)
		       | Var _ => true
		in
		   Err.check ("operand", ok, fn () => Operand.layout x)
		end
	    fun checkOperands v = Vector.foreach (v, checkOperand)
	    fun check' (x, name, isOk, layout) =
	       Err.check (name, fn () => isOk x, fn () => layout x)
	    fun statementOk (s: Statement.t): bool =
	       let
		  datatype z = datatype Statement.t
	       in
		  case s of
		     Array {dst, numElts, numPointers, numBytesNonPointers} =>
			(Type.equals (varType dst, Type.pointer)
			 andalso Type.equals (varType numElts, Type.int)
			 andalso
			 Runtime.isValidArrayHeader
			 {numPointers = numPointers,
			  numBytesNonPointers = numBytesNonPointers})
		   | Move {dst, src} =>
			(checkOperand dst
			 ; checkOperand src
			 ; Type.equals (Operand.ty dst, Operand.ty src))
		   | Object {dst, numPointers, numWordsNonPointers, stores} =>
			 (Vector.foreach (stores, fn {offset, value} =>
					  checkOperand value)
			  ; (Runtime.isValidObjectHeader
			     {numPointers = numPointers,
			      numWordsNonPointers = numWordsNonPointers}))
		   | PrimApp {args, dst, prim} => true
		   | SetExnStackLocal => true
		   | SetExnStackSlot => true
		   | SetHandler _ => true
		   | SetSlotExnStack => true
	       end
	    val labelKind = Block.kind o labelBlock
	    fun labelIsJump (l: Label.t): bool =
	       case labelKind l of
		  Kind.Jump => true
		| _ => false
	    fun goto {dst, args} =
	       let
		  val Block.T {args = formals, kind, ...} = labelBlock dst
	       in
		  Vector.equals (args, formals, fn (z, (_, t)) =>
				 Type.equals (t, operandType z))
		  andalso (case kind of
			      Kind.Jump => true
			    | _ => false)
	       end
	    fun labelIsJump l = goto {dst = l, args = Vector.new0 ()}
	    fun labelIsRuntime (l: Label.t, p: Prim.t): bool =
	       case labelKind l of
		  Kind.Runtime {prim, ...} => Prim.equals (p, prim)
		| _ => false
	    fun transferOk (t: Transfer.t): bool =
	       let
		  datatype z = datatype Transfer.t
	       in
		  case t of
		     Arith {args, dst, overflow, prim, success} =>
			Prim.mayOverflow prim
			andalso labelIsJump overflow
			andalso labelIsJump success
			andalso Vector.forall (args, fn x =>
					       Type.equals (Type.int, varType x))
		   | Bug => true
		   | CCall {args, prim = p, return, returnTy} =>
			let
			   val _ = checkOperands args
			   val Block.T {kind, ...} = labelBlock return
			in
			   case labelKind return of
			      Kind.CReturn {prim = p'} => Prim.equals (p, p')
			    | _ => false
			   end
		   | Call {args, func, return} =>
			let
			   val Function.T {args = formals, ...} = funcInfo func
			in
			   Vector.equals (args, formals, fn (z, (_, t)) =>
					  Type.equals (t, operandType z))
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
		   | LimitCheck {failure, kind, success} =>
			labelIsRuntime (failure, Prim.gcCollect)
			andalso labelIsJump success
			andalso let
				   datatype z = datatype LimitCheck.t
				in
				   case kind of
				      Array {bytesPerElt, extraBytes, numElts,
					     ...} =>
					 bytesPerElt > 0
					 andalso extraBytes >= 0
					 andalso Type.equals (Type.int,
							      varType numElts)
				    | Heap {bytes, ...} => bytes >= 0
				    | Signal => true
				    | Stack => true
				end
		      | Raise _ => true
		      | Return _ => true
		      | Runtime {args, prim, return} =>
			   (Prim.entersRuntime prim
			    andalso labelIsRuntime (return, prim))
		      | Switch {cases, default, test} =>
			   (Cases.forall (cases, labelIsJump)
			    andalso Option.forall (default, labelIsJump)
			    andalso (Type.equals
				     (operandType test,
				      case cases of
					 Cases.Char _ => Type.char
				       | Cases.Int _ => Type.int
				       | Cases.Word _ => Type.uint)))
		      | SwitchIP {int, pointer, test} =>
			   (checkOperand test
			    ; (labelIsJump pointer
			       andalso labelIsJump int
			       andalso Type.equals (Type.pointer,
						    operandType test)))
	       end
	    fun blockOk (Block.T {args, kind, label, statements,
				  transfer}): bool =
	       let
		  fun kindOk (k: Kind.t): bool =
		     let
			datatype z = datatype Kind.t
			val _ =
			   case k of
			      Cont {handler} => true
			    | CReturn {prim} => true
			    | Handler => true
			    | Jump => true
			    | Runtime {prim} => 0 = Vector.length args
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
	    val _ =
	       List.foreach
	       (functions, fn f as Function.T {name, ...} =>
		setFuncInfo (name, f))
	    val _ =
	       List.foreach
	       (functions, fn Function.T {args, blocks, start, ...} =>
		(Vector.foreach (args, setVarType)
		 ; (Vector.foreach
		    (blocks, fn b as Block.T {args, label, statements,
					      transfer, ...} =>
		     (setLabelBlock (label, b)
		      ; Vector.foreach (args, setVarType)
		      ; Vector.foreach (statements, fn s =>
					Statement.foreachDef
					(s, fn {var, ty} =>
					 setVarType (var, ty)))
		      ; Transfer.foreachDef (transfer, fn {var, ty} =>
					     setVarType (var, ty)))))
		 ; Vector.foreach (blocks, fn b =>
				   check' (b, "block", blockOk, Block.layout))))
	 in
	    ()
	 end handle Err.E e => (Layout.outputl (Err.layout e, Out.error)
				; Error.bug "Machine type error")
   end

end
