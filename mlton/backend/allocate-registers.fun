(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
   structure CFunction = CFunction
   structure Func = Func
   structure Function = Function
   structure Kind = Kind
   structure Label = Label
   structure Type = Type
   structure Var = Var
end

local
   open Machine
in
   structure Operand = Operand
   structure Register = Register
   structure Runtime = Runtime
end

val traceForceStack =
   Trace.trace ("Allocate.forceStack", Var.layout, Unit.layout)
   
(* If a handler is stored in a stack frame, then we need both a word for
 * the old handler and space for the handler itself
 *)
local
   open Type
in
   val labelSize = size label
   val handlerSize = labelSize + size word
end

structure Live = Live (open Rssa)

structure Allocation:
   sig
      structure Stack:
         sig
            type t
            val empty: t
            val get: t * Type.t -> t * {offset: int}
            val layout: t -> Layout.t
            val new: {offset: int, ty: Type.t} list -> t
            val size: t -> int
         end
      structure Registers:
         sig
            type t
            val empty: t
            val get: t * Type.t -> t * Register.t
            val layout: t -> Layout.t
            val new: Register.t list -> t
         end

      type t
      val empty: t
      val getRegister: t * Type.t -> t * Register.t
      val getStack: t * Type.t -> t * {offset: int}
      val layout: t -> Layout.t
      val new: {offset: int, ty: Type.t} list * Register.t list -> t
      val registers: t -> Registers.t
      val stack: t -> Stack.t
      val stackSize: t -> int
   end =
   struct
       structure Stack =
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
	     T (Array.toList
		(QuickSort.sortArray
		 (Array.fromListMap (alloc, fn {offset, ty} =>
				     {offset = offset,
				      size = Type.size ty}),
		  fn (r, r') => #offset r <= #offset r')))

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
					  val (a'' as {offset = o', size = s'}, ac) = 
					     coalesce ()
					  val alloc =
					     List.appendRev
					     (ac,
					      if o' + s' = offset
						 then {offset = o', size = size + s'}
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
       structure Registers =
       struct
	  (* A register allocation keeps track of the registers that have
	   * already been allocated, for each runtime type.  The reason that
	   * we associate them with runtime types rather than Rssa types is
	   * that the register indices that the codegens use are based on
	   * runtime types.
	   *)
	  datatype t = T of Runtime.Type.t -> {alloc: Register.t list,
					       next: int}

	  val empty = T (Runtime.Type.memo (fn _ => {alloc = [],
						     next = 0}))

	  fun layout (T f) =
	     List.layout
	     (fn t =>
	      let
		 val {alloc, next} = f t
	      in
		 Layout.record [("ty", Runtime.Type.layout t),
				("next", Int.layout next),
				("alloc", List.layout Register.layout alloc)]
	      end)
	     Runtime.Type.all

	  fun compress {next, alloc} =
	     let
		fun loop (next, alloc) =
		   let
		      fun done () = {alloc = alloc,
				     next = next}
		   in
		      case alloc of
			 [] => done ()
		       | r :: alloc =>
			    if next = Register.index r
			       then loop (next + 1, alloc)
			    else done ()
		   end
	     in
		loop (next, alloc)
	     end
	     
	  fun new (rs: Register.t list): t =
	     let
		fun sameType (r, r') =
		   Runtime.Type.equals
		   (Type.toRuntime (Register.ty r),
		    Type.toRuntime (Register.ty r'))
	        val rss = List.equivalence (rs, sameType)
	     in
		T (Runtime.Type.memo
		   (fn t =>
		    case List.peek (rss, fn rs =>
				    case rs of
				       [] => false
				     | r :: _ => 
					  Runtime.Type.equals
					  (t, Type.toRuntime (Register.ty r))) of
		       NONE => {alloc = [], next = 0}
		     | SOME rs =>
			  compress
			  {next = 0,
			   alloc =
			   Array.toList
			   (QuickSort.sortArray
			    (Array.fromList rs, fn (r, r') =>
			     Register.index r <= Register.index r'))}))
	     end

	  fun get (T f, ty: Type.t) =
	     let
		val t = Type.toRuntime ty
		val {alloc, next} = f t
		val r = Register.new (ty, SOME next)
		val new = compress {alloc = alloc,
				    next = next + 1}
	     in
		(T (Runtime.Type.memo
		    (fn t' => if Runtime.Type.equals (t, t')
				 then new
			      else f t')),
		 r)
	     end
       end
    
       datatype t = T of {registers: Registers.t,
			  stack: Stack.t}

       local
	  fun make s (T x) = s x
       in
	  val stack = make #stack
	  val stackSize = Stack.size o stack
	  val registers = make #registers
       end
    
       val empty = T {stack = Stack.empty,
		      registers = Registers.empty}

       fun layout (T {stack, registers}) =
	  Layout.record
	  [("stack", Stack.layout stack),
	   ("registers", Registers.layout registers)]

       fun getStack (T {stack, registers}, ty) =
	  let
	     val (stack, offset) = Stack.get (stack, ty)
	  in
	     (T {stack = stack, registers = registers}, offset)
	  end
       fun getRegister (T {stack, registers}, ty) =
	  let
	     val (registers, reg) = Registers.get (registers, ty)
	  in
	     (T {registers = registers,
		 stack = stack},
	      reg)
	  end

       fun new (stack, registers) = 
	  T {registers = Registers.new registers,
	     stack = Stack.new stack}
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

fun allocate {argOperands,
	      function = f: Rssa.Function.t,
	      varInfo: Var.t -> {operand: Machine.Operand.t option ref option,
				 ty: Type.t}} =
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
		 | Kind.CReturn {func = CFunction.T {mayGC, ...}} =>
		      if mayGC
			 then List.foreach (beginNoFormals, forceStack)
		      else ()
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
      fun allocateVar (x: Var.t,
		       l: Label.t option, 
		       force: bool,
		       a: Allocation.t): Allocation.t =
	 let
	    val {operand, ty} = varInfo x
	 in
	    if force orelse isSome operand
	       then let
		       val (a, oper) =
			  case place x of
			     Stack =>
				let
				   val (a, {offset}) =
				      Allocation.getStack (a, ty)
				in
				   (a, Operand.StackOffset {ty = ty,
							    offset = offset})
				end
			   | Register =>
				let
				   val (a, r) = Allocation.getRegister (a, ty)
				in
				   (a, Operand.Register r)
				end
		       val _ = 
			  case operand of
			     NONE => ()
			   | SOME r => r := SOME oper
		    in
		       a
		    end
	    else a
	 end
      val allocateVar =
	 Trace.trace4
	 ("Allocate.allocateVar",
	  Var.layout,
	  Option.layout Label.layout,
	  Bool.layout,
	  Allocation.layout,
	  Allocation.layout)
	 allocateVar
      (* Create the initial stack and set the stack slots for the formals. *)
      val stack =
	 Allocation.Stack.new
	 (Vector.foldr2
	  (args, argOperands, [],
	   fn ((x, t), z, ac) =>
	   case z of
	      Operand.StackOffset {offset, ...} =>
		 (valOf (#operand (varInfo x)) := SOME z
		  ; {offset = offset, ty = t} :: ac)
	    | _ => Error.bug "strange argOperand"))
      (* Allocate slots for the link and handler, if necessary. *)
      val (stack, handlerLinkOffset) =
	 if !hasHandler
	    then
	       let
		  val (stack, {offset = handler, ...}) =
		     Allocation.Stack.get (stack, Type.label)
		  val (stack, {offset = link, ...}) = 
		     Allocation.Stack.get (stack, Type.word)
	       in
		  (stack, SOME {handler = handler, link = link})
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
							ty = Type.word}
				   :: ops
			     else ops
		       in
			  ops
		       end)
	     val liveNoFormals = getOperands beginNoFormals
	     val (stackInit, registersInit) =
	        List.fold
		(liveNoFormals, ([],[]), fn (oper, (stack, registers)) =>
		 case oper of
		    Operand.StackOffset a => (a::stack, registers)
		  | Operand.Register r => (stack, r::registers)
		  | _ => (stack, registers))
	     val stackInit =
		case handlerLinkOffset of
		   NONE => stackInit
		 | SOME {handler, link} =>
		      {offset = handler, ty = Type.label}
		      :: {offset = link, ty = Type.word}
		      :: stackInit
	     val a = Allocation.new (stackInit, registersInit)
	     val size =
		Runtime.labelSize
		+ Runtime.wordAlignInt (Allocation.stackSize a)
	     val a =
		Vector.fold (args, a, fn ((x, _), a) =>
			     allocateVar (x, SOME label, false, a))
	     (* Must compute live after allocateVar'ing the args, since that
	      * sets the operands for the args.
	      *)
	     val live = getOperands begin
	     fun one (var, ty, a) = allocateVar (var, SOME label, false, a)
	     val a =
		Vector.fold (statements, a, fn (statement, a) =>
			     R.Statement.foldDef (statement, a, one))
	     val a = R.Transfer.foldDef (transfer, a, one)
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
