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
   structure Var = Var
   structure Type = Type
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
            val get: t * Type.t -> t * {index: int}
            val layout: t -> Layout.t
            val new: {index: int, ty: Type.t} list -> t
         end
      type t
      val empty: t
      val getRegister: t * Type.t -> t * {index: int}
      val getStack: t * Type.t -> t * {offset: int}
      val layout: t -> Layout.t
      val new: {offset: int, ty: Type.t} list * {index: int, ty: Type.t} list -> t
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
	  datatype t = T of {ty: Type.t, alloc: {index: int} list} list

	  val empty = T (List.map (Type.all, fn ty => {ty = ty, alloc = []}))

	  fun layout (T allocs) =
	     List.layout (fn {ty, alloc} =>
			  Layout.record [("ty", Type.layout ty),
					 ("alloc", List.layout
					           (fn {index} =>
						    Layout.record
						    [("index", Int.layout index)])
						   alloc)])
	                 allocs

	  fun new (allocs): t =
	     let
	        val allocs = List.equivalence (allocs, fn ({ty = ty1, ...},
							   {ty = ty2, ...}) =>
					       Type.equals (ty1, ty2))
		val allocs =
		   List.revMap
		   (allocs, fn alloc =>
		    let
		       val a = Array.fromListMap (alloc, fn {ty, index} =>
						  {index = index})
		       val _ = QuickSort.sort (a, fn (r, r') => #index r <= #index r')
		    in
		       {ty = #ty (hd alloc),
			alloc = Array.toList a}
		    end)
	     in
	        T allocs
	     end

	  fun get (T allocs, ty') =
	     let
	        val (allocs, index) =
		   case List.partition (allocs, fn {ty, ...} => 
					Type.equals (ty', ty)) of
		      {yes = [], no = allocs} => 
			 ({ty = ty', alloc = [{index = 0}]}::allocs, 
			  {index = 0})
		    | {yes = [{ty, alloc}], no = allocs} =>
			 let
			    fun loop (i, [], alloc') = 
			       (List.appendRev 
				(alloc', [{index = i}]), 
				{index = i})
			      | loop (i, index::alloc, alloc') =
			       if i = #index index
				  then loop (i + 1, alloc, index::alloc')
			       else (List.appendRev 
				     ({index = i}::alloc', index::alloc),
				     {index = i})
			    val (alloc, index) = loop (0, alloc, [])
			 in
			    ({ty = ty, alloc = alloc}::allocs, index)
			 end
		    | _ => Error.bug "AllocateRegisters.Allocation.Registers.get"
	     in
	        (T allocs, index)
	     end
       end
       
       datatype t = T of {stack: Stack.t, registers: Registers.t}
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
	     val (registers, index) = Registers.get (registers, ty)
	  in
	     (T {stack = stack, registers = registers}, index)
	  end

       fun new (stack, registers) = 
	  T {stack = Stack.new stack, registers = Registers.new registers}
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
      val nextReg = Type.memo (fn _ => ref 0)
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
				   val (a, {offset}) = Allocation.getStack (a, ty)
				in
				   (a, Operand.StackOffset {ty = ty,
							    offset = offset})
				end
			   | Register =>
				let
(*
                                   val r = nextReg ty
				   val reg = newRegister (l, !r, ty)
				   val _ = Int.inc r
*)
				   val (a, {index}) = Allocation.getRegister (a, ty)
				   val reg = newRegister (l, index, ty)
				in
				   (a, Operand.Register reg)
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
	  Var.layout, Option.layout Label.layout, Bool.layout, Allocation.layout,
	  Allocation.layout)
	 allocateVar
      (* Create the initial stack and set the stack slots for the formals. *)
      val stack =
	 Allocation.Stack.new
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
		  val (stack, {offset = handler, ...}) =
		     Allocation.Stack.get (stack, Type.label)
		  val (stack, {offset = link, ...}) = 
		     Allocation.Stack.get (stack, Type.uint)
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
							ty = Type.uint}
				   :: ops
			     else ops
		       in
			  ops
		       end)
	     val liveNoFormals = getOperands beginNoFormals
	     val (stackInit, registersInit) =
	        List.fold (liveNoFormals, ([],[]), fn (oper, (stack, registers)) =>
			   case oper of
			      Operand.StackOffset a => (a::stack, registers)
			    | Operand.Register (Register.T a) => (stack, a::registers)
			    | _ => (stack, registers))
	     val stackInit =
		case handlerLinkOffset of
		   NONE => stackInit
		 | SOME {handler, link} =>
		      {offset = handler, ty = Type.label}
		      :: {offset = link, ty = Type.uint}
		      :: stackInit
	     val a = Allocation.new (stackInit, registersInit)
	     val size = Runtime.labelSize + Type.wordAlign (Allocation.stackSize a)
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
