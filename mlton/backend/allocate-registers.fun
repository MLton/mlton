(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor AllocateRegisters (S: ALLOCATE_REGISTERS_STRUCTS): ALLOCATE_REGISTERS = 
struct

open S
structure R = Rssa
structure M = Machine

local
   open Rssa
in
   structure Block = Block
   structure Func = Func
   structure Function = Function
   structure Kind = Kind
   structure Label = Label
   structure Var = Var
   structure Type = Type
end

local
   open Machine
in
   structure Operand = Operand
   structure Register = Register
end

val traceForceStack =
   Trace.trace ("Allocate.forceStack", Var.layout, Unit.layout)
   
(* If a handler is stored in a stack frame, then we need both a uint for
 * the old handler and space for the handler itself
 *)
local
   open Type
in
   val labelSize = size label
   val handlerSize = labelSize + size uint
end

structure Live = Live (open Rssa)

structure Stack:
   sig
      type t

      val empty: t
      val get: t * Type.t -> t * {offset: int}
(*      val getAt: t * {offset: int, size: int} -> t option *)
      val layout: t -> Layout.t
      (* new takes a list of already allocated nonoverlapping spots *)
      val new: {offset: int, ty: Type.t} list -> t
      val size: t -> int
   end =
   struct
      (* Keep a list of allocated slots sorted in increasing order of offset.
       *)
      datatype t = T of {offset: int, size: int} list

      fun layout (T alloc) =
	 List.layout (fn {offset, size} =>
		      Layout.record [("offset", Int.layout offset),
				     ("size", Int.layout size)])
	 alloc
	 
      val empty = T []

      fun size (T alloc) =
	 case alloc of
	    [] => 0
	  | _ => let
		    val {offset, size} = List.last alloc
		 in
		    offset + size
		 end

      fun new (alloc): t =
	 let
	    val a = Array.fromListMap (alloc, fn {offset, ty} =>
				       {offset = offset,
					size = Type.size ty})
	    val _ = QuickSort.sort (a, fn (r, r') => #offset r <= #offset r')
	 in
	    T (Array.toList a)
	 end

      fun get (T alloc, ty) =
	 let
	    val slotSize = Type.size ty
	 in
	    case alloc of
	       [] => (T [{offset = 0, size = slotSize}],
		      {offset = 0})
	     | a :: alloc =>
		  let
		     fun loop (alloc, a as {offset, size}, ac) =
			let
			   val prevEnd = offset + size
			   val begin = Type.align (ty, prevEnd)
			   fun coalesce () =
			      if prevEnd = begin
				 then ({offset = offset,
					size = size + slotSize},
				       ac)
			      else ({offset = begin, size = slotSize},
				    {offset = offset, size = size} :: ac)
			in
			   case alloc of
			      [] =>
				 let
				    val (a, ac) = coalesce ()
				 in
				    (T (rev (a :: ac)), {offset = begin})
				 end
			    | (a' as {offset, size}) :: alloc =>
				 if begin + slotSize > offset
				    then loop (alloc, a', a :: ac)
				 else
				     let
					val (a'' as {offset = o', size = s'}, ac)
					   = coalesce ()
					val alloc =
					   List.appendRev
					   (ac,
					    if o' + s' = offset
					       then
						  {offset = o', size = size + s'}
						  :: alloc
					    else a'' :: a' :: alloc)
				     in
					(T alloc, {offset = begin})
				     end
			end
		  in
		     loop (alloc, a, [])
		  end
	 end
   end

structure Info =
   struct
      type t = {
		live: Operand.t vector,
		liveNoFormals: Operand.t vector,
		size: int,
		adjustSize: int -> {size: int, shift: int}
		}

      fun layout ({live, liveNoFormals, size, ...}: t) =
	 Layout.record
	 [("live", Vector.layout Operand.layout live),
	  ("liveNoFormals", Vector.layout Operand.layout liveNoFormals),
	  ("size", Int.layout size)]
   end

(* ------------------------------------------------- *)
(*                     allocate                      *)
(* ------------------------------------------------- *)

fun allocate {argOperands: Machine.Operand.t vector,
	      function = f: Rssa.Function.t,
	      newRegister,
	      varInfo: Var.t -> {operand: Machine.Operand.t option ref option,
				 ty: Machine.Type.t}} =
   let
      fun diagnostics f =
	 Control.diagnostics
	 (fn display =>
	  let
	     open Layout
	     fun diagVar (x: Var.t): unit =
		display (seq
			 [Var.layout x, str " ",
			  Option.layout
			  (fn r => Option.layout Operand.layout (!r))
			  (#operand (varInfo x))])
	     fun diagStatement (s: R.Statement.t): unit =
		R.Statement.foreachDef (s, diagVar o #1)
	  in
	     f (display, diagVar, diagStatement)
	  end)
      val _ =
	 Control.diagnostic (fn () =>
			     let open Layout
			     in seq [str "Function allocs for ",
				     Func.layout (Function.name f)]
			     end)
      val {get = labelInfo: R.Label.t -> Info.t, set = setLabelInfo, ...} =
	 Property.getSetOnce (R.Label.plist,
			      Property.initRaise ("labelInfo", R.Label.layout))
      val setLabelInfo =
	 Trace.trace2
	 ("Allocate.setLabelInfo", R.Label.layout, Info.layout, Unit.layout)
	 setLabelInfo
      val labelLive =
	 Live.live (f, {shouldConsider = isSome o #operand o varInfo})
      val {args, blocks, name, start, ...} = Function.dest f
      (*
       * Decide which variables will live in stack slots and which
       * will live in registers.
       * Initially,
       *   - all formals are put in stack slots
       *   - everything else is put everything in a register.
       * Variables get moved to the stack if they are
       *   - live at the beginning of a basic block (i.e. Fun dec)
       *   - live at a primitive that enters the runtime system
       *)
      datatype place = Stack | Register
      val {get = place: Var.t -> place, set = setPlace, ...} =
	 (* FIXME: could use destGetSetOnce? *)
	 Property.getSet (Var.plist, Property.initConst Register)
      (* !hasHandler = true iff handlers are installed in this function. *)
      val hasHandler: bool ref = ref false
      fun forceStack (x: Var.t): unit = setPlace (x, Stack)
      val forceStack = traceForceStack forceStack
      val _ = Vector.foreach (args, forceStack o #1)
      val _ =
	 Vector.foreach
	 (blocks,
	  fn R.Block.T {args, kind, label, statements, transfer, ...} =>
	  let
	     val {begin, beginNoFormals, ...} = labelLive label
	     val _ =
		case kind of
		   Kind.Cont _ =>
		      (Vector.foreach (args, forceStack o #1)
		       ; List.foreach (beginNoFormals, forceStack))
		 | Kind.Handler =>
		      List.foreach (beginNoFormals, forceStack)
		 | Kind.Runtime _ =>
		      List.foreach (beginNoFormals, forceStack)
		 | _ => ()
	     val _ =
		Vector.foreach
		(statements, fn s =>
		 let
		    datatype z = datatype R.Statement.t
		 in
		    case s of
		       SetHandler h => hasHandler := true
		     | SetExnStackLocal => hasHandler := true
		     | SetExnStackSlot => hasHandler := true
		     | SetSlotExnStack => hasHandler := true
		     | _ => ()
		 end)
	  in
	     ()
	  end)
      (* The next available register number for each type. *)
      val nextReg = Type.memo (fn _ => ref 0)
      fun allocateVar (x: Var.t,
		       l: Label.t option, 
		       force: bool,
		       s: Stack.t): Stack.t =
	 let
	    val {operand, ty} = varInfo x
	 in
	    if force orelse isSome operand
	       then let
		       val (s, oper) =
			  case place x of
			     Stack =>
				let
				   val (s, {offset}) = Stack.get (s, ty)
				in
				   (s, Operand.StackOffset {ty = ty,
							    offset = offset})
				end
			   | Register =>
				let
				   val r = nextReg ty
				   val reg = newRegister (l, !r, ty)
				   val _ = Int.inc r
				in
				   (s, Operand.Register reg)
				end
		       val _ = 
			  case operand of
			     NONE => ()
			   | SOME r => r := SOME oper
		    in
		       s
		    end
	    else s
	 end
      val allocateVar =
	 Trace.trace4
	 ("Allocate.allocateVar",
	  Var.layout, Option.layout Label.layout, Bool.layout, Stack.layout,
	  Stack.layout)
	 allocateVar
      val stack = Stack.empty
      (* Create the initial stack and set the stack slots for the formals. *)
      val _ =
	 Stack.new
	 (Vector.foldr2 (args, argOperands, [],
			 fn ((x, t), oper, ac) =>
			 case oper of
			    M.Operand.StackOffset {offset, ...} =>
			       (valOf (#operand (varInfo x)) := SOME oper
				; {offset = offset, ty = t} :: ac)
			  | _ => Error.bug "callReturnOperands"))
      (* Allocate slots for the link and handler, if necessary. *)
      val (stack, handlerLinkOffset) =
	 if !hasHandler
	    then
	       let
		  val (s, {offset = handler, ...}) =
		     Stack.get (stack, Type.label)
		  val (s, {offset = link, ...}) = Stack.get (s, Type.uint)
	       in
		  (s, SOME {handler = handler, link = link})
	       end
	 else (stack, NONE)
      fun getOperands (xs: Var.t list): Operand.t list =
	 List.fold
	 (xs, [], fn (x, operands) =>
	  valOf (! (valOf (#operand (varInfo x)))) :: operands)
      val getOperands =
	 Trace.trace ("Allocate.getOperands",
		      List.layout Var.layout,
		      List.layout Operand.layout)
	 getOperands
      (* Do a DFS of the control-flow graph. *)
      val _ =
	 Function.dfs
	 (f, fn R.Block.T {args, label, kind, statements, transfer, ...} =>
	  let
	     val {begin, beginNoFormals,
		  handlerSlots = (codeLive, linkLive)} = labelLive label
	     fun addHS ops =
		Vector.fromList
		(case handlerLinkOffset of
		    NONE => ops
		  | SOME {handler, link} =>
		       let
			  val ops =
			     if codeLive
				then
				   Operand.StackOffset {offset = handler,
							ty = Type.label}
				   :: ops
			     else ops
			  val ops =
			     if linkLive
				then
				   Operand.StackOffset {offset = link,
							ty = Type.uint}
				   :: ops
			     else ops
		       in
			  ops
		       end)
	     val liveNoFormals = getOperands beginNoFormals
	     val stackInit =
		List.fold (liveNoFormals, [], fn (oper, ac) =>
			   case oper of
			      Operand.StackOffset a => a :: ac
			    | _ => ac)
	     val stackInit =
		case handlerLinkOffset of
		   NONE => stackInit
		 | SOME {handler, link} =>
		      {offset = handler, ty = Type.label}
		      :: {offset = link, ty = Type.uint}
		      :: stackInit
	     val stack = Stack.new stackInit
	     val s =
		Vector.fold (args, stack, fn ((x, _), s) =>
			     allocateVar (x, SOME label, false, s))
	     (* Must compute live after allocateVar'ing the args, since that
	      * sets the operands for the args.
	      *)
	     val live = getOperands begin
	     fun one (var, ty, stack) =
		allocateVar (var, SOME label, false, stack)
	     val s =
		Vector.fold (statements, s, fn (s, stack) =>
			     R.Statement.foldDef (s, stack, one))
	     val s = R.Transfer.foldDef (transfer, stack, one)
	     val size = Runtime.labelSize + Type.wordAlign (Stack.size stack)
	     fun adjustSize _ = Error.unimplemented "adjustSize"
	     val _ =
		setLabelInfo (label, {live = addHS live,
				      liveNoFormals = addHS liveNoFormals,
				      size = size,
				      adjustSize = adjustSize})
	  in
	     fn () => ()
	  end)
      val _ =
	 diagnostics
	 (fn (display, diagVar, diagStatement) =>
	  let
	     open Layout
	     val _ =
		display (seq [str "function ", Func.layout name,
			      str " handlerLinkOffset ",
			      Option.layout
			      (fn {handler, link} =>
			       record [("handler", Int.layout handler),
				       ("link", Int.layout link)])
			      handlerLinkOffset])
	     val _ = Vector.foreach (args, diagVar o #1)
	     val _ =
		Vector.foreach
		(blocks, fn R.Block.T {label, args, statements, ...} =>
		 (display (R.Label.layout label)
		  ; Vector.foreach (args, diagVar o #1)
		  ; Vector.foreach (statements, diagStatement)))
	  in ()
	  end)
   in
      {handlerLinkOffset = handlerLinkOffset,
       labelInfo = labelInfo}
   end

val allocate = 
   Trace.trace
   ("Allocate.allocate",
    fn {function, ...} => Func.layout (Function.name function),
    Layout.ignore)
   allocate
   
end
