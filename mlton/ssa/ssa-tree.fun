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
       | HandlerPop of Label.t
       | HandlerPush of Label.t
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

      val mayAllocate =
	 fn ConApp {args, ...} => Vector.length args > 0
	  | PrimApp {prim, ...} => Prim.mayAllocate prim
	  | Tuple vs => Vector.length vs > 1
	  | _ => false

      val unit = Tuple (Vector.new0 ())
	 
      fun foreachLabelVar (e, j, v) =
	 let
	    fun vs xs = Vector.foreach (xs, v)
	 in
	    case e of
	       ConApp {args, ...} => vs args
	     | Const _ => ()
	     | HandlerPop l => j l
	     | HandlerPush l => j l
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

      fun replaceLabelVar (e, fl, fx) =
	 let
	    fun fxs xs = Vector.map (xs, fx)
	 in
	    case e of
	       ConApp {con, args} => ConApp {con = con, args = fxs args}
	     | Const _ => e
	     | HandlerPop l => HandlerPop (fl l)
	     | HandlerPush l => HandlerPush (fl l)
	     | PrimApp {prim, targs, args} =>
		  PrimApp {prim = prim, targs = targs, args = fxs args}
	     | Select {tuple, offset} =>
		  Select {tuple = fx tuple, offset = offset}
	     | SetExnStackLocal => e
	     | SetExnStackSlot => e
	     | SetHandler h => SetHandler (fl h)
	     | SetSlotExnStack => e
	     | Tuple xs => Tuple (fxs xs)
	     | Var x => Var (fx x)
	 end

      fun replaceVar (e, f) = replaceLabelVar (e, fn l => l, f)
      fun replaceLabel (e, f) = replaceLabelVar (e, f, fn x => x)

      fun layout e =
	 let
	    open Layout
	 in
	    case e of
	       ConApp {con, args} =>
		  seq [Con.layout con, str " ", layoutTuple args]
	     | Const c => Const.layout c
	     | HandlerPop l => seq [str "HandlerPop ", Label.layout l]
	     | HandlerPush l => seq [str "HandlerPush ", Label.layout l]
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
	  | HandlerPop _ => false
	  | HandlerPush _ => false
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
	  | HandlerPop _ => true
	  | HandlerPush _ => true
	  | PrimApp {prim,...} => Prim.maySideEffect prim
	  | Select _ => false
	  | SetExnStackLocal => true
	  | SetExnStackSlot => true
	  | SetHandler _ => true
	  | SetSlotExnStack => true
	  | Tuple _ => false
	  | Var _ => false

      fun varsEquals (xs, xs') = Vector.equals (xs, xs', Var.equals)

      fun equals (e: t, e': t): bool =
	 case (e, e') of
	    (ConApp {con, args}, ConApp {con = con', args = args'}) =>
	       Con.equals (con, con') andalso varsEquals (args, args')
	  | (Const c, Const c') => Const.equals (c, c')
	  | (HandlerPop l, HandlerPop l') => Label.equals (l, l')
	  | (HandlerPush l, HandlerPush l') => Label.equals (l, l')
	  | (PrimApp {prim, args, ...}, PrimApp {prim = prim', args = args', ...}) =>
	       Prim.equals (prim, prim') andalso varsEquals (args, args')
	  | (Select {tuple, offset}, Select {tuple = tuple', offset = offset'}) =>
	       Var.equals (tuple, tuple') andalso offset = offset'
	  | (SetExnStackLocal, SetExnStackLocal) => true
	  | (SetExnStackSlot, SetExnStackslot) => true
	  | (SetHandler l, SetHandler l') => Label.equals (l, l')
	  | (SetSlotExnStack, SetSlotExnStack) => true
	  | (Tuple xs, Tuple xs') => varsEquals (xs, xs')
	  | (Var x, Var x') => Var.equals (x, x')
	  | _ => false

      local
	 val newHash = Random.word
	 val conApp = newHash ()
	 val handlerPop = newHash ()
	 val handlerPush = newHash ()
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
	     | HandlerPop l => Word.xorb (handlerPop, Label.hash l)
	     | HandlerPush l => Word.xorb (handlerPush, Label.hash l)
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
	  | HandlerPop l => concat ["HandlerPop ", Label.toString l]
	  | HandlerPush l => concat ["HandlerPush ", Label.toString l]
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
      datatype t = T of {var: Var.t option,
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
	    seq [seq [case var of
			 NONE => empty
		       | SOME var =>
			    seq [Var.layout var,
				 if !Control.showTypes
				    then seq [str ": ", Type.layout ty]
				 else empty,
				 str " = "]],
                 Exp.layout exp]
	 end

      local
	 fun make (e: Exp.t) =
	    T {var = NONE,
	       ty = Type.unit,
	       exp = e}
      in
	 val setExnStackLocal = make Exp.SetExnStackLocal
	 val setExnStackSlot = make Exp.SetExnStackSlot
	 val setSlotExnStack = make Exp.SetSlotExnStack
	 fun setHandler h = make (Exp.SetHandler h)
	 fun handlerPop h = make (Exp.HandlerPop h)
	 fun handlerPush h = make (Exp.HandlerPush h)
      end

      val mayAllocate = Exp.mayAllocate o exp
	 
      fun clear s = Option.app (var s, Var.clear)

      fun prettifyGlobals (v: t vector): Var.t -> string option =
	 let
	    val {get = global: Var.t -> string option, set = setGlobal, ...} =
	       Property.getSet (Var.plist, Property.initConst NONE)
	    val _ = 
	       Vector.foreach
	       (v, fn T {var, exp, ...} =>
		Option.app
		(var, fn var =>
		 let
		    fun set s =
		       let
			  val maxSize = 10
			  val s = 
			     if String.size s > maxSize
				then concat [String.prefix (s, maxSize), "..."]
			     else s
		       in
			  setGlobal (var, SOME s)
		       end
		 in
		    case exp of
		       Const c => set (Layout.toString (Const.layout c))
		     | ConApp {con, args, ...} =>
			  if Vector.isEmpty args
			     then set (Con.toString con)
			  else set (concat [Con.toString con, "(...)"])
		     | _ => ()
		 end))
	 in
	    global
	 end
   end

structure Handler =
   struct
      datatype t =
	 CallerHandler
       | Handle of Label.t
       | None

      fun layout h =
	 let
	    open Layout
	 in
	    case h of
	       CallerHandler => str "CallerHandler"
	     | Handle l => seq [str "Handle ", Label.layout l]
	     | None => str "None"
	 end

      val equals =
	 fn (CallerHandler, CallerHandler) => true
	  | (None, None) => true
	  | (Handle l, Handle l') => Label.equals (l, l')
	  | _ => false

      fun foreachLabel (h, f) =
	 case h of
	    Handle l => f l
	  | _ => ()

      fun map (h, f) =
	 case h of
	    Handle l => Handle (f l)
	  | _ => h
   end

structure Return =
   struct
      datatype t =
	 Dead
       | HandleOnly
       | NonTail of {cont: Label.t,
		     handler: Handler.t}
       | Tail

      val layout =
	 let
	    open Layout
	 in
	    fn Dead => str "Dead"
	     | HandleOnly => str "HandleOnly"
	     | NonTail {cont, handler} =>
		  seq [str "NonTail ",
		       record [("cont", Label.layout cont),
			       ("handler", Handler.layout handler)]]
	     | Tail => str "Tail"
	 end

      val isNonTail = fn NonTail _ => true | _ => false
	 
      fun map (r, f) =
	 case r of
	    NonTail {cont, handler} =>
	       NonTail {cont = f cont,
			handler = Handler.map (handler, f)}
	  | _ => r
	       
      fun foreachLabel (r, f) =
	 case r of
	    NonTail {cont, handler} =>
	       (f cont
		; Handler.foreachLabel (handler, f))
	  | _ => ()

      fun compose (c: t, r: t): t =
	 case r of
	    Dead => Dead
	  | HandleOnly =>
	       (case c of
		   Dead => Dead
		 | HandleOnly => HandleOnly
		 | NonTail _ => c
		 | Tail => HandleOnly)
	  | NonTail {cont, handler, ...} =>
	       (case (handler, c) of
		   (Handler.CallerHandler, NonTail {handler = h1, ...}) =>
		      NonTail {cont = cont, handler = h1}
		 | _ => r)
	  | Tail => c
   end

structure Transfer =
   struct

      datatype t =
	 Bug
       | Call of {func: Func.t,
		  args: Var.t vector,
		  return: Return.t}
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

      fun foreachFuncLabelVar (t, func, label: Label.t -> unit, var) =
	 let
	    fun vars xs = Vector.foreach (xs, var)
	 in
	    case t of
	       Bug => ()
	     | Call {func = f, args, return, ...} =>
		  (func f
		   ; Return.foreachLabel (return, label)
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

      fun replaceLabelVar (t, fl, fx) =
	 let
	    fun fxs xs = Vector.map (xs, fx)
	 in
	    case t of
	       Bug => Bug
	     | Call {func, args, return} =>
		  Call {func = func, 
			args = fxs args,
			return = Return.map (return, fl)}
	     | Case {test, cases, default} =>
		  Case {test = fx test, 
			cases = Cases.map(cases, fl),
			default = Option.map(default, fl)}
	     | Goto {dst, args} => 
		  Goto {dst = fl dst, 
			args = fxs args}
	     | Prim {prim, args, failure, success} =>
		  Prim {prim = prim,
			args = fxs args,
			failure = fl failure,
			success = fl success}
	     | Raise xs => Raise (fxs xs)
	     | Return xs => Return (fxs xs)
	 end

      fun replaceVar (t, f) = replaceLabelVar (t, fn l => l, f)
      fun replaceLabel (t, f) = replaceLabelVar (t, f, fn x => x)

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
	     | Case arg => layoutCase arg
	     | Goto {dst, args} =>
		  seq [Label.layout dst, str " ", layoutTuple args]
	     | Prim {prim, args, failure, success} =>
		  seq [Label.layout success,
		       tuple [Prim.layoutApp (prim, args, Var.layout)],
		       str " Overflow => ",
		       Label.layout failure, str "()"]
	     | Raise xs => seq [str "raise ", layoutTuple xs]
	     | Return xs => if 1 = Vector.length xs
			       then Var.layout (Vector.sub (xs, 0))
			    else layoutTuple xs
      end
   end
datatype z = datatype Transfer.t

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
   
      fun layout (T {label, args, statements, transfer}) =
	 let
	    open Layout
	 in
	    align [seq [Label.layout label, str " ",
			Vector.layout (fn (x, t) =>
				       if !Control.showTypes
					  then seq [Var.layout x, str ": ",
						    Type.layout t]
				       else Var.layout x) args],
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
      structure CPromise = ClearablePromise
	 
      type dest = {args: (Var.t * Type.t) vector,
		   blocks: Block.t vector,
		   mayRaise: bool,
		   name: Func.t,
		   returns: Type.t vector option,
		   start: Label.t}

      (* There is a messy interaction between the laziness used in controlFlow
       * and the property lists on labels because the former stores
       * stuff on the property lists.  So, if you force the laziness, then
       * clear the property lists, then try to use the lazy stuff, you will
       * get screwed with undefined properties.  The right thing to do is reset
       * the laziness when the properties are cleared.
       *)
      datatype t =
	 T of {controlFlow:
	       {dfsTree: unit -> Block.t Tree.t,
		dominatorTree: unit -> Block.t Tree.t,
		graph: DirectedGraph.t,
		labelNode: Label.t -> DirectedGraph.Node.t,
		nodeBlock: DirectedGraph.Node.t -> Block.t} CPromise.t,
	       dest: dest}

      local
	 fun make f (T {dest, ...}) = f dest
      in
	 val blocks = make #blocks
	 val dest = make (fn d => d)
	 val mayRaise = make #mayRaise
	 val name = make #name
	 val returns = make #returns
	 val start = make #start
      end

      fun foreachVar (f: t, fx: Var.t * Type.t -> unit): unit =
	 let
	    val {args, blocks, ...} = dest f
	    val _ = Vector.foreach (args, fx)
	    val _ =
	       Vector.foreach
	       (blocks, fn Block.T {args, statements, ...} =>
		(Vector.foreach (args, fx)
		 ; Vector.foreach (statements, fn Statement.T {var, ty, ...} => 
				   Option.app (var, fn x => fx (x, ty)))))
	 in
	    ()
	 end

      fun controlFlow (T {controlFlow, ...}) =
	 let
	    val {graph, labelNode, nodeBlock, ...} = CPromise.force controlFlow
	 in
	    {graph = graph, labelNode = labelNode, nodeBlock = nodeBlock}
	 end

      local
	 fun make sel =
	    fn T {controlFlow, ...} => sel (CPromise.force controlFlow) ()
      in
	 val dfsTree = make #dfsTree
	 val dominatorTree = make #dominatorTree
      end

      fun inferHandlers (f: t): Label.t list option array =
	 let
	    val {blocks, name, start, ...} = dest f
	    val {get = labelIndex: Label.t -> int, set = setLabelIndex, ...} =
	       Property.getSetOnce (Label.plist,
				    Property.initRaise ("index", Label.layout))
	    val _ =
	       Vector.foreachi
	       (blocks, fn (i, Block.T {label, ...}) =>
		setLabelIndex (label, i))
	    val numBlocks = Vector.length blocks
	    val handlerStack = Array.array (numBlocks, NONE)
	    val visited = Array.array (numBlocks, false)
	    (* Do a dfs from the start, figuring out the handler stack at
	     * each label.
	     *)
	    fun visit (l: Label.t, hs: Label.t list): unit =
	       let
		  val i = labelIndex l
		  val Block.T {statements, transfer, ...} =
		     Vector.sub (blocks, i)
	       in
		  if Array.sub (visited, i)
		     then ()
		  else
		     let
			val _ = Array.update (visited, i, true)
			fun bug msg =
			   (Layout.outputl
			    (Vector.layout
			     (fn Block.T {label, ...} =>
			      let open Layout
			      in seq [Label.layout label,
				      str " ",
				      Option.layout (List.layout Label.layout)
				      (Array.sub (handlerStack,
						  labelIndex label))]
			      end)
			     blocks,
			     Out.error)
			    ; (Error.bug
			       (concat
				["inferHandlers bug found ", Label.toString l,
				 ": ", msg])))
			val _ =
			   case Array.sub (handlerStack, i) of
			      NONE => Array.update (handlerStack, i, SOME hs)
			    | SOME hs' =>
				 if List.equals (hs, hs', Label.equals)
				    then ()
				 else bug "handler stack mismatch"
			val hs =
			   Vector.fold
			   (statements, hs, fn (s, hs) =>
			    let
			       val Statement.T {var, ty, exp, ...} = s
			    in
			       case Statement.exp s of
				  HandlerPop _ =>
				     (case hs of
					 [] => bug "pop of empty handler stack"
				       | _ :: hs => hs)
				| HandlerPush h => h :: hs
				| _ => hs
			    end)
			fun empty () =
			   if List.isEmpty hs
			      then ()
			   else bug "nonempty stack"
			fun top l =
			   case hs of
			      l' :: _ =>
				 if Label.equals (l, l')
				    then ()
				 else bug "wrong handler on top"
			    | _ => bug "empty stack"
			val _ =
			   case transfer of
			      Call {return, ...} =>
				 (case return of
				     Return.Dead => ()
				   | Return.HandleOnly => empty ()
				   | Return.NonTail {handler, ...} =>
					(case handler of
					    Handler.CallerHandler => empty ()
					  | Handler.Handle l => top l
					  | Handler.None => ())
				   | Return.Tail => empty ())
			    | Raise _ => empty ()
			    | Return _ => empty ()
			    | _ => ()
			val _ = 
			   Transfer.foreachLabel (transfer, fn l =>
						  visit (l, hs))
		     in
			()
		     end
	       end
	    val _ = visit (start, [])
	 in
	    handlerStack
	 end

      fun checkHandlers (f: t): unit =
	 let
	    val {name, start, blocks, ...} = dest f
	    val {get = labelInfo: Label.t -> HandlerInfo.t,
		 rem = remLabelInfo, 
		 set = setLabelInfo} =
	       Property.getSetOnce
	       (Label.plist, Property.initRaise ("info", Label.layout))
	    val _ =
	       Vector.foreach
	       (blocks, fn b => setLabelInfo (Block.label b, HandlerInfo.new b))
	    (* Do a DFS of the control-flow graph. *)
	    fun visitLabel l = visitInfo (labelInfo l)
	    and visitInfo
	       (hi as HandlerInfo.T {block, global, handler, slot, visited, ...})
	       : unit =
	       if !visited
		  then ()
	       else
	       let
		  val _ = visited := true
		  val Block.T {label, statements, transfer, ...} = block
		  datatype z = datatype ExnStack.t
		  val {global, handler, slot} =
		     Vector.fold
		     (statements,
		      {global = global, handler = handler, slot = slot},
		      fn (Statement.T {exp, ...}, {global, handler, slot}) =>
		      case exp of
			 SetExnStackLocal => {global = ExnStack.me,
					      handler = handler,
					      slot = slot}
		       | SetExnStackSlot => {global = slot,
					     handler = handler,
					     slot = slot}
		       | SetSlotExnStack => {global = global,
					     handler = handler,
					     slot = slot}
		       | SetHandler l => {global = global,
					  handler = HandlerLat.point l,
					  slot = slot}
		       | _ => {global = global, handler = handler, slot = slot})
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
				 ("handler", HandlerLat.layout handler)]],
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
		  fun tail name =
		     assert (name,
			     ExnStack.forcePoint
			     (global, ExnStack.Point.Caller))
		  fun caller () =
		     ExnStack.forcePoint (global, ExnStack.Point.Caller)
		in
		   case transfer of
		      Bug => ()
		    | Call {return, ...} =>
			 assert
			 ("return",
			  case return of
			     Return.Dead => true
			   | Return.HandleOnly => caller ()
			   | Return.NonTail {cont, handler = h, ...} =>
				(goto cont
				 ; (case h of
				       Handler.CallerHandler => caller ()
				     | Handler.Handle l =>
					  let
					     val res =
						ExnStack.forcePoint
						(global, ExnStack.Point.Me)
						andalso (HandlerLat.forcePoint
							 (handler, l))
					     val _ = goto l
					  in
					     res
					  end
				     | Handler.None => true))
			   | Return.Tail => caller ())
		    | Case {cases, default, ...} =>
			 (Cases.foreach (cases, goto)
			  ; Option.app (default, goto))
		    | Goto {dst, ...} => goto dst
		    | Prim {failure, success, ...} =>
			 (goto failure; goto success)
		    | Raise _ => tail "raise"
		    | Return _ => tail "return"
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
		       display (seq [Label.layout label,
				     str " ",
				     HandlerInfo.layout (labelInfo label)]))
		in
		   ()
		end)
	    val _ = Vector.foreach (blocks, fn b => remLabelInfo (Block.label b))
	 in
	    ()
	 end
			    
      local
	 structure Graph = DirectedGraph
	 structure Node = Graph.Node
	 structure Edge = Graph.Edge
      in
	 fun determineControlFlow ({args, blocks, name, start, ...}: dest) =
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
		  (blocks, fn b as Block.T {label, statements, transfer, ...} =>
		   let
		      val from = labelNode label
		      val _ = setNodeInfo (from, {block = b})
		      fun edge (to: Label.t): unit =
			(Graph.addEdge (g, {from = from, to = labelNode to})
			 ; ())
		      val _ =
			 case transfer of
			   Bug => ()
			 | Call {return, ...} =>
			      Return.foreachLabel (return, edge)
			 | Case {cases, default, ...} =>
			      (Cases.foreach (cases, edge)
			       ; Option.app (default, edge))
			 | Goto {dst, ...} => edge dst
			 | Prim {failure, success, ...} =>
			      (edge failure; edge success)
			 | Raise _ => ()
			 | Return _ => ()
		   in
		      ()
		   end)
	       val root = labelNode start
	       val dfsTree =
		  Promise.lazy
		  (fn () =>
		   Graph.dfsTree (g, {root = root,
				      nodeValue = #block o nodeInfo})
		   handle exn => Error.bug (concat ["dfsTree: ",
						    Func.toString name,
						    ":",
						    case exn
						      of Fail s => s
						       | _ => "???"]))
	       val dominatorTree =
		  Promise.lazy
		  (fn () =>
		   Graph.dominatorTree (g, {root = root,
					    nodeValue = #block o nodeInfo})
		   handle exn => Error.bug (concat ["dominatorTree: ",
						    Func.toString name,
						    ":",
						    case exn
						       of Fail s => s
						     | _ => "???"]))
	    in
	       {dfsTree = dfsTree,
		dominatorTree = dominatorTree,
		graph = g,
		labelNode = labelNode,
		nodeBlock = #block o nodeInfo}
	    end

	 fun layoutDot (f, global: Var.t -> string option) =
	    let
	       val {name, args, start, blocks, ...} = dest f
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
	       open Dot
	       val graph = Graph.new ()
	       val {get = nodeOptions, ...} =
		  Property.get (Node.plist, Property.initFun (fn _ => ref []))
	       fun setNodeText (n: Node.t, l): unit =
		  List.push (nodeOptions n, NodeOption.Label l)
	       fun newNode () = Graph.newNode graph
	       val {destroy, get = labelNode} =
		  Property.destGet (Label.plist,
				    Property.initFun (fn _ => newNode ()))
	       val {get = edgeOptions, set = setEdgeOptions, ...} =
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
				  val call = [f, " ", args]
			       in
				  case return of
				     Return.Dead => "Dead " :: call
				   | Return.HandleOnly =>
					"HandleOnly " :: call
				   | Return.NonTail {cont, handler} =>
					(edge (cont, "", Dotted)
					 ; (case handler of
					       Handler.CallerHandler => call
					     | Handler.Handle l =>
						  (edge (l, "", Dashed)
						   ; call)
					     | Handler.None => call @ [" None"]))
				   | Return.Tail => call
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
			  | Raise xs => ["raise ", Var.prettys (xs, global)]
			  | Return xs => ["return ", Var.prettys (xs, global)]
		      val lab =
			 Vector.foldr
			 (statements, [(concat rest, Left)],
			  fn (Statement.T {var, ty, exp, ...}, ac) =>
			  let
			     val exp = Exp.toPretty (exp, global)
			     val s =
				if Type.isUnit ty
				   then exp
				else
				   case var of
				      NONE => exp
				    | SOME var =>
					 concat [Var.toString var,
						 if !Control.showTypes
						    then concat [": ",
								 Layout.toString
								 (Type.layout ty)]
						 else "",
						    " = ", exp]
			  in
			     (s, Left) :: ac
			  end)
		      val name = makeName (Label.toString label, args)
		      val _ = setNodeText (from, (name, Left) :: lab)
		   in
		      ()
		   end)
	       val root = labelNode start
	       val graphLayout =
		  Graph.layoutDot
		  (graph,
		   {title = concat [makeName (Func.toString name, args),
				    " control-flow graph"],
		    options = [GraphOption.Rank (Min, [(*root*)])],
		    edgeOptions = edgeOptions,
		    nodeOptions =
		    fn n => let
			       val l = ! (nodeOptions n)
			       open NodeOption
			    in FontColor Black :: Shape Box :: l
			    end})
	       fun treeLayout () =
		  let
		     val _ =
			Vector.foreach
			(blocks, fn Block.T {label, ...} =>
			 nodeOptions (labelNode label)
			 := [NodeOption.label (Label.toString label)])
		     val treeLayout =
			Tree.layoutDot
			(Graph.dominatorTree (graph,
					      {root = root,
					       nodeValue = ! o nodeOptions}),
			 {title = concat [Func.toString name, " dominator tree"],
			  options = [],
			  nodeOptions = fn z => z})
		     val _ = destroy ()
		  in
		     treeLayout
		  end
	    in
	       {graph = graphLayout,
		tree = treeLayout}
	    end
      end

      fun new (dest: dest) =
	 let
	    val controlFlow = CPromise.delay (fn () => determineControlFlow dest)
	 in
	    T {controlFlow = controlFlow,
	       dest = dest}
	 end

      fun clear (T {controlFlow, dest, ...}) =
	 let
	    val {name, args, blocks, ...} = dest
	    val _ = (Func.clear name
		     ; Vector.foreach (args, Var.clear o #1)
		     ; Vector.foreach (blocks, Block.clear))
	    val _ = CPromise.clear controlFlow
	 in
	    ()
	 end

      fun layout (f: t, global: Var.t -> string option) =
	 let
	    val {args, blocks, mayRaise, name, returns, start, ...} = dest f
	    open Layout
	 in
	    align [seq [str "fun ",
			Func.layout name,
			str " ",
			layoutFormals args,
			if !Control.showTypes
			   then seq [str ": ",
				     Option.layout
				     (Vector.layout Type.layout) returns,
				     str " {mayRaise = ",
				     Bool.layout mayRaise,
				     str "}"]
			else empty,
			str " = ", Label.layout start, str "()"],
		   indent (align (Vector.toListMap (blocks, Block.layout)),
			   2)]
	 end
      
      fun layouts (fs: t list, global,
		   output: Layout.t -> unit): unit =
	 List.foreach
	 (fs, fn f =>
	  let
	     val name = name f
	     val _ = output (layout (f, global))
	     val _ =
		if not (!Control.keepDot)
		   then ()
		else
		   let
		      val {graph, tree} = layoutDot (f, global)
		      val name = Func.toString name
		      fun doit (s, g) =
			 let
			    open Control
			 in
			    saveToFile
			    ({suffix = concat [name, ".", s, ".dot"]},
			     Dot, (), Layout (fn () => g))
			 end
		      val _ = doit ("cfg", graph)
			      handle _ => Error.warning "couldn't layout cfg"
		      val _ = doit ("dom", tree ())
			      handle _ => Error.warning "couldn't layout dom"
		   in
		      ()
		   end
	  in
	     ()
	  end)

      fun alphaRename f =
	 let
	    local
	       fun make (new, plist, layout) =
		  let
		     val {get, set, destroy} = 
		        Property.destGetSetOnce (plist, Property.initConst NONE)
		     fun bind x = let val x' = new x
				  in set (x, SOME x'); x'
				  end
		     fun lookup x =
		        case get x of
			   NONE => x
			 | SOME y => y
		  in (set, bind, lookup, destroy)
		  end
	    in
	       val (setVar, bindVar, lookupVar, destroyVar) =
		  make (Var.new, Var.plist, Var.layout)
	       val (setLabel, bindLabel, lookupLabel, destroyLabel) =
		  make (Label.new, Label.plist, Label.layout)
	    end
	    fun lookupVars xs = Vector.map (xs, lookupVar)

(*
	    val {name, args, start, blocks, returns, ...} = dest f
	    val args = Vector.map (args, fn (x, ty) => (bindVar x, ty))
	    val _ = 
	       Vector.foreach
	       (blocks, fn Block.T {label, ...} => 
		ignore (bindLabel label))
	    val newBlocks = ref []
	    fun loop (Tree.T (Block.T {label, args, statements, transfer},
			      children)) =
	       let
		  val label = lookupLabel label
		  val args = Vector.map (args, fn (x, ty) => (bindVar x, ty))
		  val statements = 
		     Vector.map
		     (statements, fn Statement.T {var, ty, exp} =>
		      Statement.T {var = Option.map (var, bindVar),
				   ty = ty,
				   exp = Exp.replaceLabelVar 
				         (exp, lookupLabel, lookupVar)})
		  val transfer = Transfer.replaceLabelVar
		                 (transfer, lookupLabel, lookupVar)
	       in
		  List.push (newBlocks, 
			     Block.T {label = label,
				      args = args,
				      statements = statements,
				      transfer = transfer})
		  ; Vector.foreach (children, loop)
	       end
	    val _ = loop (dominatorTree f)
	    val blocks = Vector.fromList (!newBlocks)
*)

	    val {args, blocks, mayRaise, name, returns, start, ...} = dest f
	    val args = Vector.map (args, fn (x, ty) => (bindVar x, ty))
	    val bindLabel = ignore o bindLabel
	    val bindVar = ignore o bindVar
	    val _ = 
	       Vector.foreach
	       (blocks, fn Block.T {label, args, statements, ...} => 
		(bindLabel label
		 ; Vector.foreach (args, fn (x, _) => bindVar x)
		 ; Vector.foreach (statements, 
				   fn Statement.T {var, ...} => 
				   Option.app (var, bindVar))))
	    val blocks = 
	       Vector.map
	       (blocks, fn Block.T {label, args, statements, transfer} =>
		Block.T {label = lookupLabel label,
			 args = Vector.map (args, fn (x, ty) => (lookupVar x, ty)),
			 statements = Vector.map
			              (statements, 
				       fn Statement.T {var, ty, exp} =>
				       Statement.T 
				       {var = Option.map (var, lookupVar),
					ty = ty,
					exp = Exp.replaceLabelVar
					      (exp, lookupLabel, lookupVar)}),
			 transfer = Transfer.replaceLabelVar
			            (transfer, lookupLabel, lookupVar)})
	 in
	    new {name = name,
		 args = args,
		 start = lookupLabel start,
		 mayRaise = mayRaise,
		 blocks = blocks,
		 returns = returns}
	 end
   end

structure Program =
   struct
      datatype t =
	 T of {
	       datatypes: Datatype.t vector,
	       globals: Statement.t vector,
	       functions: Function.t list,
	       main: Func.t
	       }
   end

structure Program =
   struct
      open Program

      fun checkHandlers (T {functions, ...}) =
	 List.foreach (functions, Function.checkHandlers)
	 
      local
	 structure Graph = DirectedGraph
	 structure Node = Graph.Node
	 structure Edge = Graph.Edge
      in
	 fun layoutCallGraph (T {functions, main, ...},
			      title: string): Layout.t =
	    let
	       open Dot
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
	       val {get = edgeOptions, set = setEdgeOptions, ...} =
		  Property.getSetOnce (Edge.plist, Property.initConst [])
	       val _ =
		  List.foreach
		  (functions, fn f =>
		   let
		      val {name, blocks, ...} = Function.dest f
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
				   val is =
				      (case return of
					  Return.NonTail _ => true
					| _ => false)
				   val r = if is
					      then nontail
					   else tail
				in
				   if !r
				      then ()
				   else (r := true
					 ; (setEdgeOptions
					    (Graph.addEdge
					     (graph, {from = from, to = to}),
					     if is
						then []
					     else [EdgeOption.Style Dotted])))
				end
			   | _ => ())
		      val _ = destroy ()
		   in
		      ()
		   end)
	       val l =
		  Graph.layoutDot
		  (graph, {title = title,
			   options = [],
			   edgeOptions = edgeOptions,
			   nodeOptions = nodeOptions})
	       val _ = destroy ()
	    in
	       l
	    end
      end
	 
      fun layouts (p as T {datatypes, globals, functions, main},
		   output': Layout.t -> unit) =
	 let
	    val global = Statement.prettifyGlobals globals
	    open Layout
	    val output = output'
	 in output (align (Vector.toListMap (datatypes, Datatype.layout)))
	    ; output (str "\n\nGlobals:")
	    ; Vector.foreach (globals, output o Statement.layout)
	    ; output (str "\n\nFunctions:")
	    ; Function.layouts (functions, global, output)
	    ; output (seq [str "\n\nMain: ", Func.layout main])
	    ; if not (!Control.keepDot)
		 then ()
	      else
		 let
		    open Control
		 in
		    saveToFile
		    ({suffix = "call-graph.dot"},
		     Dot, (), Layout (fn () =>
				     layoutCallGraph (p, !Control.inputFile)))
		 end
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
	       List.foreach
	       (functions, fn f =>
		let
		   val {args, blocks, ...} = Function.dest f
		in
		   Vector.foreach (args, countType o #2)
		   ; (Vector.foreach
		      (blocks, fn Block.T {statements, ...} =>
		       (Int.inc numBlocks
			; (Vector.foreach
			   (statements, fn Statement.T {ty, ...} =>
			    (countType ty
			     ; Int.inc numStatements))))))
		end)
	    val numFunctions = List.length functions
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
      fun clear (T {datatypes, globals, functions, ...}) =
	 ((* Can't do Type.clear because it clears out the info needed for
	   * Type.dest.
	   *)
	  Vector.foreach (datatypes, Datatype.clear)
	  ; Vector.foreach (globals, Statement.clear)
	  ; List.foreach (functions, Function.clear))

      fun clearGlobals (T {globals, ...}) =
	 Vector.foreach (globals, Statement.clear)

      fun clearTop (p as T {datatypes, functions, ...}) =
	 (Vector.foreach (datatypes, Datatype.clear)
	  ; List.foreach (functions, Func.clear o Function.name)
	  ; clearGlobals p)

      fun foreachVar (T {globals, functions, ...}, f) =
	 (Vector.foreach (globals, fn Statement.T {var, ty, ...} =>
			  f (valOf var, ty))
	  ; List.foreach (functions, fn g => Function.foreachVar (g, f)))

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
		List.foreach
		(functions, fn f =>
		 Vector.foreach
		 (Function.blocks f, fn Block.T {statements, ...} =>
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
	    fun loopTransfer (t, handlers) =
	       let
		  datatype z = datatype Cps.Transfer.t
	       in
		  case t of
		     Bug => Transfer.Bug
		   | Call {func, args, cont} =>
			let
			   val return =
			      case cont of
				 NONE => Return.Tail
			       | SOME l =>
				    Return.NonTail
				    {cont = jumpToLabel l,
				     handler =
				     (case jumpHandlers l of
					 [] => Handler.CallerHandler
				       | h :: _ =>
					    Handler.Handle (jumpToLabel h))}
			in
			   Transfer.Call {func = funcToFunc func,
					  args = args,
					  return = return}
			end
		   | Case {test, cases, default, ...} =>
			Transfer.Case {test = test,
				       cases = cases,
				       default = default}
		   | Jump {dst, args} =>
			Transfer.Goto {dst = dst, args = args}
		   | Raise xs =>
			(case handlers of
			    [] => Transfer.Raise xs
			  | h :: _ => Transfer.Goto {dst = h, args = xs})
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
			   [] => addBlock (loopTransfer (transfer, handlers))
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
				       loopDecs
				       (decs,
					Statement.handlerPush (jumpToLabel h)
					:: statements,
					l, args, h :: handlers, transfer)
				  | HandlerPop => 
				       (case handlers of
					   [] =>
					      Error.bug
					      "pop of empty handler stack"
					 | h :: handlers =>
					      loopDecs
					      (decs,
					       Statement.handlerPop h
					       :: statements,
					       l, args, handlers, transfer))
			      end
		     end
		  val start = Label.newNoname ()
		  val _ = loop (body, start, Vector.new0 (), [])
	       in
		  Function.new {args = args,
				blocks = Vector.fromList (!blocks),
				mayRaise = true,
				name = funcToFunc name,
				returns = SOME returns,
				start = start}
	       end
	    val program =
	       T {datatypes = Vector.map (datatypes, Datatype.T),
		  globals = (Vector.map
			     (globals, fn b =>
			      case bindToStatement b of
				 Normal s => s
			       | Overflow _ => Error.bug "overflow in globals")),
		  functions = Vector.toListMap (functions, loopFunc),
		  main = funcToFunc main}
	    val _ = clear program
	 in
	    program
	 end
   end

end
