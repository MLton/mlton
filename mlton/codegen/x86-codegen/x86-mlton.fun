(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor x86MLtonBasic(S: X86_MLTON_BASIC_STRUCTS): X86_MLTON_BASIC =
struct

  open S
  open x86

  (*
   * x86.Size.t equivalents
   *)
  val wordSize = Size.LONG
  val wordBytes = Size.toBytes wordSize
  val wordScale = Scale.Four
  val pointerSize = Size.LONG
  val pointerBytes = Size.toBytes pointerSize
  val pointerScale = Scale.Four
  val floatSize = Size.DBLE
  val floatBytes = Size.toBytes floatSize
  val objectHeaderBytes = wordBytes
  val arrayHeaderBytes = wordBytes + wordBytes
  val intInfOverheadBytes = arrayHeaderBytes + wordBytes
   
  local
    open MachineOutput.Type
  in
    fun toX86Size t
      = case dest t
	  of Char => x86.Size.BYTE
	   | Double => x86.Size.DBLE
	   | Int => x86.Size.LONG
	   | Pointer => x86.Size.LONG
	   | Uint => x86.Size.LONG
	   | Void => Error.bug "toX86Size: Void"
    fun toX86Scale t
      = case dest t
	  of Char => x86.Scale.One
	   | Double => x86.Scale.Eight
	   | Int => x86.Scale.Four
	   | Pointer => x86.Scale.Four
	   | Uint => x86.Scale.Four
	   | Void => Error.bug "toX86Scale: Void"
  end

  (*
   * Memory classes
   *)
  structure Classes =
    struct
      local
	fun new s = MemLoc.Class.new {name = s}
      in
	val Heap = new "Heap"
	val Stack = new "Stack"
	val Locals = new "Locals"
	val Globals = new "Globals"
	  
	val Temp = MemLoc.Class.Temp
	val CStack = MemLoc.Class.CStack
	val Code = MemLoc.Class.Code
	  
	val CStatic = new "CStatic"
	val StaticTemp = new "StaticTemp"
	val StaticNonTemp = new "StaticNonTemp"
	  
	val GCState = new "GCState"
	val GCStateHold = new "GCStateHold"
	  
	val IntInfRes = new "IntInfRes"
	val ThreadStack = new "ThreadStack"
      end

      val allClasses = ref x86.ClassSet.empty 
      val livenessClasses = ref x86.ClassSet.empty 
      val holdClasses = ref x86.ClassSet.empty 
      val runtimeClasses = ref x86.ClassSet.empty 
      val cstaticClasses = ref x86.ClassSet.empty 

      fun initClasses ()
	= let
	    val _ = allClasses :=	
	            x86.ClassSet.fromList
		    (
		     Heap::
		     Stack::
		     Locals::
		     Globals::
		     Temp::
		     CStack::
		     Code::
		     CStatic::
		     StaticTemp::
		     StaticNonTemp::
		     GCState::
		     GCStateHold::
		     IntInfRes::
		     ThreadStack::
		     nil)

	    val _ = livenessClasses :=
	            (if !Control.Native.liveStack
		       then x86.ClassSet.fromList
			    (
			     Temp::
			     Locals::
			     StaticTemp::
			     Stack::
			     nil)
		       else x86.ClassSet.fromList
			    (
			     Temp::
			     Locals::
			     StaticTemp::
			     nil))

	    val _ = holdClasses :=
	            x86.ClassSet.fromList
		    (
		     GCStateHold::
		     nil)

	    val _ = runtimeClasses :=
	            x86.ClassSet.fromList
		    (
		     Heap::
		     Stack::
		     Globals::
		     GCState::
		     GCStateHold::
		     ThreadStack::
		     nil)

	    val _ = cstaticClasses :=
	            x86.ClassSet.fromList
		    (
		     CStatic::
		     nil)
	  in
	    ()
	  end
    end

  (*
   * Static memory locations
   *)
  fun makeContents {base, size, class}
    = MemLoc.imm {base = base,
		  index = Immediate.const_int 0,
		  scale = wordScale,
		  size = size,
		  class = class}

  val c_stackP = Label.fromString "c_stackP"
  val c_stackPContents 
    = makeContents {base = Immediate.label c_stackP,
		    size = pointerSize,
		    class = Classes.StaticNonTemp}
  val c_stackPContentsOperand 
    = Operand.memloc c_stackPContents
  val c_stackPDeref
    = MemLoc.simple {base = c_stackPContents,
		     index = Immediate.const_int 0,
		     scale = wordScale,
		     size = pointerSize,
		     class = Classes.CStack}
  val c_stackPDerefOperand
    = Operand.memloc c_stackPDeref
  val c_stackPDerefDouble
    = MemLoc.simple {base = c_stackPContents,
		     index = Immediate.const_int 0,
		     scale = wordScale,
		     size = Size.DBLE,
		     class = Classes.CStack}
  val c_stackPDerefDoubleOperand
    = Operand.memloc c_stackPDerefDouble

  val limitCheckTemp = Label.fromString "limitCheckTemp"
  val limitCheckTempContents 
    = makeContents {base = Immediate.label limitCheckTemp,
		    size = pointerSize,
		    class = Classes.StaticTemp}
  val limitCheckTempContentsOperand 
    = Operand.memloc limitCheckTempContents
     
  val arrayAllocateTemp = Label.fromString "arrayAllocateTemp"
  val arrayAllocateTempContents 
    = makeContents {base = Immediate.label arrayAllocateTemp,
		    size = pointerSize,
		    class = Classes.StaticTemp}
  val arrayAllocateTempContentsOperand 
    = Operand.memloc arrayAllocateTempContents

  val arrayAllocateLoopTemp = Label.fromString "arrayAllocateLoopTemp"
  val arrayAllocateLoopTempContents 
    = makeContents {base = Immediate.label arrayAllocateLoopTemp,
		    size = pointerSize,
		    class = Classes.StaticTemp}
  val arrayAllocateLoopTempContentsOperand 
    = Operand.memloc arrayAllocateLoopTempContents
  val arrayAllocateLoopTempDeref
    = MemLoc.simple {base = arrayAllocateLoopTempContents, 
		     index = Immediate.const_int 0,
		     scale = wordScale,
		     size = pointerSize,
		     class = Classes.Heap}
  val arrayAllocateLoopTempDerefOperand 
    = Operand.memloc arrayAllocateLoopTempDeref

  val overflowCheckTemp = Label.fromString "overflowCheckTemp"
  val overflowCheckTempContents 
    = makeContents {base = Immediate.label overflowCheckTemp,
		    size = wordSize,
		    class = Classes.StaticTemp}
  val overflowCheckTempContentsOperand 
    = Operand.memloc overflowCheckTempContents

  val intInfTemp = Label.fromString "intInfTemp"
  val intInfTempContents 
    = makeContents {base = Immediate.label intInfTemp,
		    size = wordSize,
		    class = Classes.StaticTemp}
  val intInfTempContentsOperand
    = Operand.memloc intInfTempContents
  val intInfTempFrontierContents 
    = MemLoc.simple {base = intInfTempContents,
		     index = Immediate.const_int 0,
		     scale = wordScale,
		     size = pointerSize,
		     class = Classes.IntInfRes}
  val intInfTempFrontierContentsOperand
    = Operand.memloc intInfTempFrontierContents 
  val intInfTempValueContents
    = MemLoc.simple {base = intInfTempContents,
		     index = Immediate.const_int 1,
		     scale = wordScale,
		     size = pointerSize,
		     class = Classes.IntInfRes}
  val intInfTempValueContentsOperand
    = Operand.memloc intInfTempValueContents
				 
  val threadTemp = Label.fromString "threadTemp"
  val threadTempContents 
    = makeContents {base = Immediate.label threadTemp,
		    size = wordSize,
		    class = Classes.StaticTemp}
  val threadTempContentsOperand
    = Operand.memloc threadTempContents
  val threadTemp_stackContents 
    = MemLoc.simple {base = threadTempContents,
		     index = Immediate.const_int 1,
		     size = pointerSize,
		     scale = wordScale,
		     class = Classes.Heap}
  val threadTemp_stackContentsOperand
    = Operand.memloc threadTemp_stackContents
  val threadTemp_stack_reservedContents 
    = MemLoc.simple {base = threadTemp_stackContents,
		     index = Immediate.const_int 0,
		     size = pointerSize,
		     scale = wordScale,
		     class = Classes.ThreadStack}
  val threadTemp_stack_reservedContentsOperand
    = Operand.memloc threadTemp_stack_reservedContents
  val threadTemp_stack_usedContents 
    = MemLoc.simple {base = threadTemp_stackContents,
		     index = Immediate.const_int 1,
		     size = pointerSize,
		     scale = wordScale,
		     class = Classes.ThreadStack}
  val threadTemp_stack_usedContents
    = Operand.memloc threadTemp_stack_usedContents
    
  val statusTemp = Label.fromString "statusTemp"
  val statusTempContents 
    = makeContents {base = Immediate.label statusTemp,
		    size = wordSize,
		    class = Classes.StaticTemp}
  val statusTempContentsOperand
    = Operand.memloc statusTempContents

  val fileTemp = Label.fromString "fileTemp"
  val fileTempContents 
    = makeContents {base = Immediate.label fileTemp,
		    size = pointerSize,
		    class = Classes.StaticTemp}
  val fileTempContentsOperand
    = Operand.memloc fileTempContents

  val applyFFTemp = Label.fromString "applyFFTemp"
  val applyFFTempContents 
    = makeContents {base = Immediate.label applyFFTemp,
		    size = wordSize,
		    class = Classes.StaticTemp}
  val applyFFTempContentsOperand
    = Operand.memloc applyFFTempContents

  val realTemp1 = Label.fromString "realTemp1"
  val realTemp1Contents 
    = makeContents {base = Immediate.label realTemp1,
		    size = floatSize,
		    class = Classes.StaticTemp}
  val realTemp1ContentsOperand
    = Operand.memloc realTemp1Contents

  val realTemp2 = Label.fromString "realTemp2"
  val realTemp2Contents 
    = makeContents {base = Immediate.label realTemp2,
		    size = floatSize,
		    class = Classes.StaticTemp}
  val realTemp2ContentsOperand
    = Operand.memloc realTemp2Contents 

  val realTemp3 = Label.fromString "realTemp3"
  val realTemp3Contents 
    = makeContents {base = Immediate.label realTemp3,
		    size = floatSize,
		    class = Classes.StaticTemp}
  val realTemp3ContentsOperand
    = Operand.memloc realTemp3Contents

  val fpswTemp = Label.fromString "fpswTemp"
  val fpswTempContents 
    = makeContents {base = Immediate.label fpswTemp,
		    size = Size.WORD,
		    class = Classes.StaticTemp}
  val fpswTempContentsOperand
    = Operand.memloc fpswTempContents

  local
    open MachineOutput.Type
    val localC_base = Label.fromString "localuchar"
    val localD_base = Label.fromString "localdouble"
    val localI_base = Label.fromString "localint"
    val localP_base = Label.fromString "localpointer"
    val localU_base = Label.fromString "localuint"
  in
    fun local_base ty
      = case dest ty
	  of Char    => localC_base
	   | Double  => localD_base
	   | Int     => localI_base
	   | Pointer => localP_base
	   | Uint    => localU_base
	   | Void    => Error.bug "local_base: Void"
  end

  local
    open MachineOutput.Type
    val globalC_base = Label.fromString "globaluchar"
    val globalC_num = Label.fromString "num_globaluchar"
    val globalD_base = Label.fromString "globaldouble"
    val globalD_num = Label.fromString "num_globaldouble"
    val globalI_base = Label.fromString "globalint"
    val globalI_num = Label.fromString "num_globalint"
    val globalP_base = Label.fromString "globalpointer"
    val globalP_num = Label.fromString "num_globalpointer"
    val globalU_base = Label.fromString "globaluint"
    val globalU_num = Label.fromString "num_globaluint"
  in
    fun global_base ty
      = case dest ty
	  of Char    => globalC_base
	   | Double  => globalD_base
	   | Int     => globalI_base
	   | Pointer => globalP_base
	   | Uint    => globalU_base
	   | Void    => Error.bug "global_base: Void"
  end

  val globalPointerNonRoot_base = Label.fromString "globalpointerNonRoot"

  val saveGlobals = Label.fromString "saveGlobals"
  val loadGlobals = Label.fromString "loadGlobals"

  val fileNameLabel = Label.fromString "fileName"
  val fileName = Operand.immediate_label fileNameLabel
  (* This is a hack: The line number needs to be pushed, but the actual
   *  call to GC_gc is about 7 lines further (push 4 more arguments,
   *  save gcState.frontier and gcState.stackTop, make call).
   * However, there are probably cases where this is different.
   *)
  val fileLineLabel = Label.fromString "__LINE__"
  val fileLine
    = fn () => if !Control.debug
		 then Operand.immediate (Immediate.const_int 0)
		 else Operand.immediate (Immediate.binexp
					 {oper = Immediate.Addition,
					  exp1 = Immediate.label fileLineLabel,
					  exp2 = Immediate.const_int 7})

  val gcState = Label.fromString "gcState"
  val gcState_limit 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 8}
  val gcState_limitContents 
    = makeContents {base = gcState_limit,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_limitContentsOperand
    = Operand.memloc gcState_limitContents

  val gcState_frontier
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 12}
  val gcState_frontierContents 
    = makeContents {base = gcState_frontier,
		    size = pointerSize,
		    class = Classes.GCStateHold}
  val gcState_frontierContentsOperand
    = Operand.memloc gcState_frontierContents
  val gcState_frontierDeref
    = MemLoc.simple {base = gcState_frontierContents, 
		     index = Immediate.const_int 0,
		     scale = wordScale,
		     size = pointerSize,
		     class = Classes.Heap}
  val gcState_frontierDerefOperand
    = Operand.memloc gcState_frontierDeref

  val gcState_currentThread 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 32}
  val gcState_currentThreadContents 
    = makeContents {base = gcState_currentThread,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_currentThreadContentsOperand
    = Operand.memloc gcState_currentThreadContents
  val gcState_currentThread_exnStackContents 
    = MemLoc.simple {base = gcState_currentThreadContents,
		     index = Immediate.const_int 0,
		     size = pointerSize,
		     scale = wordScale,
		     class = Classes.Heap}
  val gcState_currentThread_exnStackContentsOperand
    = Operand.memloc gcState_currentThread_exnStackContents
  val gcState_currentThread_stackContents 
    = MemLoc.simple {base = gcState_currentThreadContents,
		     index = Immediate.const_int 1,
		     size = pointerSize,
		     scale = wordScale,
		     class = Classes.Heap}
  val gcState_currentThread_stackContentsOperand
    = Operand.memloc gcState_currentThread_stackContents
  val gcState_currentThread_stack_reservedContents 
    = MemLoc.simple {base = gcState_currentThread_stackContents,
		     index = Immediate.const_int 0,
		     size = pointerSize,
		     scale = wordScale,
		     class = Classes.ThreadStack}
  val gcState_currentThread_stack_reservedContentsOperand
    = Operand.memloc gcState_currentThread_stack_reservedContents
  val gcState_currentThread_stack_usedContents 
    = MemLoc.simple {base = gcState_currentThread_stackContents,
		     index = Immediate.const_int 1,
		     size = pointerSize,
		     scale = wordScale,
		     class = Classes.ThreadStack}
  val gcState_currentThread_stack_usedContentsOperand
    = Operand.memloc gcState_currentThread_stack_usedContents

  val gcState_stackTop 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 40}
  val gcState_stackTopContents 
    = makeContents {base = gcState_stackTop,
		    size = pointerSize,
		    class = Classes.GCStateHold}
  val gcState_stackTopContentsOperand
    = Operand.memloc gcState_stackTopContents
  val gcState_stackTopDeref
    = MemLoc.simple {base = gcState_stackTopContents, 
		     index = Immediate.const_int 0,
		     scale = wordScale,
		     size = pointerSize,
		     class = Classes.Stack}
  val gcState_stackTopDerefOperand
    = Operand.memloc gcState_stackTopDeref

  val gcState_stackBottom 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 44}
  val gcState_stackBottomContents 
    = makeContents {base = gcState_stackBottom,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_stackBottomContentsOperand
    = Operand.memloc gcState_stackBottomContents 

  val gcState_stackLimit 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 48}
  val gcState_stackLimitContents 
    = makeContents {base = gcState_stackLimit,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_stackLimitContentsOperand
    = Operand.memloc gcState_stackLimitContents 

  val gcState_maxFrameSize
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 52}
  val gcState_maxFrameSizeContents 
    = makeContents {base = gcState_maxFrameSize,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_maxFrameSizeContentsOperand
    = Operand.memloc gcState_maxFrameSizeContents 

  val gcState_canHandle
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 128}
  val gcState_canHandleContents
    = makeContents {base = gcState_canHandle,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_canHandleContentsOperand
    = Operand.memloc gcState_canHandleContents


  (*
   * GC related constants and functions
   *)
  val WORD_SIZE = 4 : int
  val POINTER_SIZE = WORD_SIZE
  val GC_OBJECT_HEADER_SIZE = WORD_SIZE
  val GC_ARRAY_HEADER_SIZE = WORD_SIZE + GC_OBJECT_HEADER_SIZE

  (*
   * Array lengths can't have the top two bits set because the high bit is
   * used to distinguish between objects and arrays, and the next bit is the
   * mark bit.  They also can't be 30 one bits, because that is reserved for 
   * the stack object header, which must be distinguishable from an array 
   * length.
   *)
  val GC_MAX_ARRAY_LENGTH = 0x3FFFFFFF : int

  (*
   * High bit in an object identifies its type.
   * Normal objects have a high bit of 1.
   * Arrays and stacks have a high bit of 0.
   *)
  val HIGH_BIT = 0wx80000000 : word
    
  (*
   * The mark bit in an object is used by runtime utilities that need to
   * perform a depth first search of objects.
   *)
  val MARK_BIT = 0wx40000000 : word

  (*
   * Number of bits specifying the number of nonpointers in an object.
   *)
  val NON_POINTER_BITS = 15 : int
  (*
   * Number of bits specifying the number of pointers in an object.
   *)
  val POINTER_BITS = 15 : int
  val NON_POINTERS_SHIFT = POINTER_BITS

  fun two_power n = Word.toInt (Word.<<(0w1, Word.fromInt n))

  (*
   * Build the one word header for an object,
   * given the number of words of nonpointers and the number of pointers.
   *)
  fun gcObjectHeader {nonPointers, pointers}
    = let
	val _ = Assert.assert
	        ("gcObjectHeader: too many nonPointers",
		 fn () => nonPointers < two_power(NON_POINTER_BITS))
	val _ = Assert.assert
	        ("gcObjectHeader: too many pointers",
	         fn () => pointers < two_power(POINTER_BITS))
	val w 
	  = Word.orb(HIGH_BIT,
		     Word.orb(Word.fromInt pointers,
			      Word.<<(Word.fromInt nonPointers,
				      Word.fromInt NON_POINTERS_SHIFT)))
      in
	Immediate.const_word w
      end

  (*
   * Build the one word header for an object,
   * given the number of bytes of nonpointers and the number of pointers.
   *)
  fun gcArrayHeader {nonPointers, pointers}
    = let
	(* 
	 * Arrays are allowed one fewer nonpointer bit, because the top
	 * nonpointer bit is used for the continuation header word.
	 *)
	val _ = Assert.assert
	        ("gcArrayHeader: too many nonPointers",
		 fn () => nonPointers < two_power(NON_POINTER_BITS - 1))
	val _ = Assert.assert
	        ("gcArrayHeader: too many pointers",
	         fn () => pointers < two_power(POINTER_BITS))
	val w 
	  = Word.orb(Word.fromInt pointers,
		     Word.<<(Word.fromInt nonPointers,
			     Word.fromInt NON_POINTERS_SHIFT))
      in
	Immediate.const_word w
      end

  (* init *)
  fun init () = let
		  val _ = Classes.initClasses ()
		in
		  ()
		end
end

functor x86MLton(S: X86_MLTON_STRUCTS): X86_MLTON =
struct

  open S
  open x86MLtonBasic
  open x86

  val wordAlign : int -> int
    = fn p => let
		val wp = Word.fromInt p
		val wp' 
		  = Word.<<(Word.+(Word.>>(Word.-(wp, 
						  0wx1), 
					   0wx2),
				   0wx1),
			    0wx2)
		val p' = Word.toInt wp'
	      in
		p'
	      end

  fun applyFF {target: Label.t,
	       args: (Operand.t * Size.t) list,
	       dst: (Operand.t * Size.t) option,
	       live: Operand.t list,
	       liveInfo: x86Liveness.LiveInfo.t} : Block.t' AppendList.t
    = let
	val return = Label.newString "creturn"
	val _ = x86Liveness.LiveInfo.setLiveOperands
	        (liveInfo, 
		 return,
		 case dst
		   of SOME (dst,_) => dst::live
		    | NONE => live)

	val (comment_begin,
	     comment_end)
	  = if !Control.Native.commented > 0
	      then ([x86.Assembly.comment "begin applyFF"],
		    [x86.Assembly.comment "end applyFF"])
	      else ([],[])		 
      in
	AppendList.fromList
	[Block.T'
	 {entry = NONE,
	  profileInfo = ProfileInfo.none,
	  statements = comment_begin,
	  transfer = SOME (Transfer.ccall {target = target,
					   args = args,
					   dst = dst,
					   live = [],
					   return = return})},
	 Block.T'
	 {entry = SOME (Entry.jump {label = return}),
	  profileInfo = ProfileInfo.none,
	  statements = comment_end,
	  transfer = NONE}]
      end

  fun invokeRuntime {target: Label.t, 
		     args: (Operand.t * Size.t) list, 
		     info as {frameSize: int, 
			      live: Operand.t list,
			      return: Label.t},
		     frameLayouts: Label.t -> {size: int, 
					       frameLayoutsIndex: int} option,
		     liveInfo: x86Liveness.LiveInfo.t} : Block.t' AppendList.t
    = let
	val (comment_begin,
	     comment_end)
	  = if !Control.Native.commented > 0
	      then ([x86.Assembly.comment "begin invokeRuntime"],
		    [x86.Assembly.comment "end invokeRuntime"])
	      else ([],[])
	val _ = x86Liveness.LiveInfo.setLiveOperands
	        (liveInfo, return, live)
	val frameInfo = case frameLayouts return
			  of NONE => Error.bug "invokeRuntime, frameInfo"
			   | SOME {size, frameLayoutsIndex}
			   => let
				val _
				  = Assert.assert
				    ("invokeRuntime, frame size",
				     fn () => size = frameSize)
			      in 
				x86.Entry.FrameInfo.frameInfo
			        {size = size, frameLayoutsIndex = frameLayoutsIndex}
			      end
      in
	AppendList.fromList
	[Block.T'
	 {entry = NONE,
	  profileInfo = ProfileInfo.none,
	  statements = comment_begin,
	  transfer = SOME (Transfer.runtime {target = target,
					     args = args,
					     live = [],
					     return = return,
					     size = frameSize})},
	 Block.T'
	 {entry = SOME (Entry.runtime {label = return,
				       frameInfo = frameInfo}),
	  profileInfo = ProfileInfo.none,
	  statements = comment_end,
	  transfer = NONE}]
      end

  structure Prim = MachineOutput.Prim

  structure PrimInfo =
    struct
      datatype t
	= None
        | Overflow of x86.Label.t * x86.Operand.t list
        | Runtime of {frameSize: int, 
		      live: x86.Operand.t list,
		      return: x86.Label.t}
        | Normal of x86.Operand.t list
    end

  fun applyPrim {oper: Prim.t,
		 args: (Operand.t * Size.t) list,
		 dst: (Operand.t * Size.t) option,
(*
		 info: {frameSize: int,
			live: Operand.t list,
			return: Label.t} option,
*)
		 pinfo: PrimInfo.t,
		 frameLayouts: Label.t -> {size: int, frameLayoutsIndex: int} option,
		 liveInfo: x86Liveness.LiveInfo.t} : Block.t' AppendList.t
    = let
	val primName = Prim.toString oper
	datatype z = datatype Prim.Name.t

	fun getDst ()
	  = case dst
	      of SOME dst => dst
	       | NONE => Error.bug "applyPrim: getDst"
	fun getSrc1 ()
	  = case args
	      of [src] => src
	       | _ => Error.bug "applyPrim: getSrc1"
	fun getSrc2 ()
	  = case args
	      of [src1,src2] => (src1,src2)
	       | _ => Error.bug "applyPrim: getSrc2"
	fun getSrc3 ()
	  = case args
	      of [src1,src2,src3] => (src1,src2,src3)
	       | _ => Error.bug "applyPrim: getSrc3"
	fun getRuntimeInfo ()
	  = case pinfo
	      of PrimInfo.Runtime gcInfo => gcInfo
	       | _ => Error.bug "applyPrim: getRuntimeInfo"
	fun getPrimInfoOverflow ()
	  = case pinfo
	      of PrimInfo.Overflow (label, live) => (label, live)
	       | _ => Error.bug "applyPrim: getPrimInfoOverflow"
	fun getPrimInfoNormal ()
	  = case pinfo
	      of PrimInfo.Normal live => live
	       | _ => Error.bug "applyPrim: getPrimInfoOverflow"

	fun unimplemented s
	  = AppendList.fromList
	    [Block.T'
	     {entry = NONE,
	      profileInfo = ProfileInfo.none,
	      statements = [Assembly.comment ("UNIMPLEMENTED PRIM: " ^ s)],
	      transfer = NONE}]
		
	fun lengthArrayVectorString ()
	  = let
	      val (dst,dstsize) = getDst ();
	      val _ 
		= Assert.assert
		  ("applyPrim: lengthArrayVectorString, dstsize",
		   fn () => dstsize = wordSize)
	      val (src,srcsize) = getSrc1 ();
	      val _ 
		= Assert.assert
		  ("applyPrim: lengthArrayVectorString, srcsize",
		   fn () => srcsize = wordSize)

	      val memloc
		= case (Operand.deMemloc src)
		    of SOME base
		     => MemLoc.simple 
		        {base = base,
			 index = Immediate.const_int ~2,
			 scale = wordScale,
			 size = wordSize,
			 class = Classes.Heap}
		     | NONE => Error.bug 
                               "applyPrim: lengthArrayVectorString, src"
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = Operand.memloc memloc,
		    size = wordSize}],
		transfer = NONE}]
	    end

	fun subWord8ArrayVector ()
	  = let
	      val (dst,dstsize) = getDst ()
	      val _ 
		= Assert.assert
		  ("applyPrim: subWord8ArrayVector, dstsize", 
		   fn () => dstsize = Size.LONG)
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: subWord8ArrayVector, src1size",
		   fn () => src1size = pointerSize)
	      val _ 
		= Assert.assert
		  ("applyPrim: subWord8ArrayVector, src2size",
		   fn () => src2size = pointerSize)

	      val base 
		= case (Operand.deMemloc src1)
		    of SOME base => base
		     | NONE => Error.bug "applyPrim: subWord8ArrayVector, src1"
	      val memloc
		= case (Operand.deImmediate src2,
			Operand.deMemloc src2)
		    of (SOME index, _)
		     => MemLoc.simple 
		        {base = base,
			 index = index,
			 scale = Scale.Four,
			 size = Size.LONG,
			 class = Classes.Heap}
		     | (_, SOME index)
		     => MemLoc.complex 
		        {base = base,
			 index = index,
			 scale = Scale.Four,
			 size = Size.LONG,
			 class = Classes.Heap}
		     | _ => Error.bug "applyPrim: subWord8ArrayVector, src2"
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = Operand.memloc memloc,
		    size = dstsize}],
		transfer = NONE}]
	    end	  

	fun updateWord8Array ()
	  = let
	      val ((src1,src1size),
		   (src2,src2size),
		   (src3,src3size)) = getSrc3 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: updateWord8Array, src1size", 
		   fn () => src1size = pointerSize)
	      val _ 
		= Assert.assert
		  ("applyPrim: updateWord8Array, src2size", 
		   fn () => src2size = wordSize)
	      val _ 
		= Assert.assert
		  ("applyPrim: updateWord8Array, src3size", 
		   fn () => src3size = wordSize)

	      val base 
		= case (Operand.deMemloc src1)
		    of SOME base => base
		     | NONE => Error.bug "applyPrim: updateWord8Array, src1"
	      val memloc
		= case (Operand.deImmediate src2,
			Operand.deMemloc src2)
		    of (SOME index, _)
		     => MemLoc.simple 
		        {base = base,
			 index = index,
			 scale = Scale.Four,
			 size = Size.LONG,
			 class = Classes.Heap}
		     | (_, SOME index)
		     => MemLoc.complex 
			{base = base,
			 index = index,
			 scale = Scale.Four,
			 size = Size.LONG,
			 class = Classes.Heap}
		     | _ => Error.bug "applyPrim: updateWord8Array, src2"
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_mov
		   {dst = Operand.memloc memloc,
		    src = src3,
		    size = src3size}],
		transfer = NONE}]
	    end	  

	fun thread f
	  = let
	      val (thread,threadsize) = getSrc1 ()
	      val info = getRuntimeInfo ()
	      val _ 
		= Assert.assert
		  ("applyPrim: thread",
		   fn () => threadsize = pointerSize)
	    in
	      AppendList.appends
	      [AppendList.fromList
	       [(* thread might be of the form SX(?),
		 *  and invoke runtime will change the stackTop,
		 *  so copy the thread to a local location.
		 *)
		Block.T'
		{entry = NONE,
		 profileInfo = ProfileInfo.none,
		 statements 
		 = [Assembly.instruction_mov
		    {dst = threadTempContentsOperand,
		     src = thread,
		     size = threadsize}],
		 transfer = NONE}],
	       (invokeRuntime {target = Label.fromString f,
			       args = [(Operand.immediate_label gcState, pointerSize),
				       (threadTempContentsOperand, threadsize)],
			       info = info,
			       frameLayouts = frameLayouts,
			       liveInfo = liveInfo})]
	    end

(*
	fun thread_switchTo ()
	  = let
	      val (thread,threadsize) = getSrc1 ()
	      val {frameSize, return} = getInfo ()
	      val _ = set(return, [])
	      val _ 
		= Assert.assert
		  ("applyPrim: thread_switchTo",
		   fn () => threadsize = pointerSize)
	    in
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [(* thread might be of the form SX(?),
		    *  so copy the thread to a local location.
		    *)
		   Assembly.instruction_mov
		   {dst = threadTempContentsOperand
		    src = thread,
		    size = threadsize},
		   (* gcState.stackTop += frameSize *)
		   Assembly.instruction_binal
		   {oper = Instruction.ADD,
		    dst = gcState_stackTopContentsOperand,
		    src = Operand.immediate_const_int frameSize,
		    size = pointerSize},
		   (* *(gcState.stackTop) = return *)
		   Assembly.instruction_mov
		   {dst = gcState_stackTopDerefOperand,
		    src = Operand.immediate_label return,
		    size = pointerSize},
		   (* gcState.currentThread->stack->used
		    *   = gcState.stackTop + WORD_SIZE - gcState.stackBottom
		    *)
		   Assembly.instruction_mov
		   {dst 
		    = gcState_currentThread_stack_usedContentsOperand,
		    src = gcState_stackTopContentsOperand
		    size = wordSize},
		   Assembly.instruction_binal
		   {oper = Instruction.ADD,
		    dst
		    = gcState_currentThread_stack_usedContentsOperand,
		    src = Operand.immediate_const_int WORD_SIZE,
		    size = pointerSize},
		   Assembly.instruction_binal
		   {oper = Instruction.SUB,
		    dst
		    = gcState_currentThread_stack_usedContentsOperand,
		    src = gcState_stackBottomContentsOperand,
		    size = pointerSize},
		   (* gcState.currentThread = threadTemp *)
		   Assembly.instruction_mov
		   {dst = gcState_currentThreadContentsOperand,
		    src = threadTempContentsOperand,
		    size = pointerSize},
		   (* gcState.stackBottom = threadTemp->stack + 8 *)
		   Assembly.instruction_mov
		   {dst = gcState_stackBottomContentsOperand,
		    src = threadTemp_stackContentsOperand,
		    size = pointerSize},
		   Assembly.instruction_binal
		   {oper = Instruction.ADD,
		    dst = gcState_stackBottomContentsOperand,
		    src = Operand,.immediate_const_int 8,
		    size = pointerSize},		   
		   (* gcState.stackTop
		    *    = gcState.stackBottom + threadTemp->stack->used 
		    *                          - WORD_SIZE 
		    *)
		   Assembly.instruction_mov
		   {dst = gcState_stackTopContentsOperand
		    src = gcState_stackBottomContentsOperand
		    size = pointerSize},
		   Assembly.instruction_binal
		   {oper = Instruction.ADD,
		    dst = gcState_stackTopContentsOperand
		    src = threadTemp_stack_usedContentsOperand
		    size = pointerSize},
		   Assembly.instruction_binal
		   {oper = Instruction.SUB,
		    dst = gcState_stackTopContentsOperand
		    src = Operand.immediate_const_int WORD_SIZE,
		    size = pointerSize},
		   (* gcState.stackLimit
		    *    = gcState.stackBottom + threadTemp->stack->reserved
		    *                          - 2 * gcState.maxFrameSize
		    *)
		   Assembly.instruction_mov
		   {dst = gcState_stackLimitContentsOperand
		    src = gcState_stackBottomContentsOperand
		    size = pointerSize},
		   Assembly.instruction_binal
		   {oper = Instruction.ADD,
		    dst = gcState_stackLimitContentsOperand
		    src = threadTemp_stack_reservedContentsOperand
		    size = pointerSize},
		   Assembly.instruction_binal
		   {oper = Instruction.SUB,
		    dst = gcState_stackLimitContentsOperand
		    src = gcState_maxFrameSizeContentsOperand
		    size = pointerSize},
		   Assembly.instruction_binal
		   {oper = Instruction.SUB,
		    dst = gcState_stackLimitContentsOperand
		    src = gcState_maxFrameSizeContentsOperand
		    size = pointerSize},
		   (* gcState.canHandle-- *)
		   Assembly.instruction_unal
		   {oper = Instruction.DEC,
		    dst = gcState_canHandleContentsOperand
		    size = wordSize}],
		transfer = NONE},
	       (* thread changed,
		* so jump to current thread's return.
		*)	 
	       Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements = [],
		transfer 
		= SOME (Transfer.assembly
			[Assembly.instruction_jmp
			 {target = gcState_stackTopDerefOperand
			  absolute = true}])},
	       (* return: *)
	       Block.T'
	       {entry = SOME return,
		profileInfo = ProfileInfo.none,
		statements 
		= [(* *(gcState.stackTop) -= framesize *)
		   Assembly.instruction_binal
		   {oper = Instruction.SUB,
		    dst = gcState_stackTopContentsOperand
		    src = Operand.immediate_const_int frameSize,
		    size = pointerSize}],
		transfer = NONE}]
	    end
*)

	fun intInf_comp f
	  = let
	      val (dst,dstsize) = getDst ()
	      val _
		= Assert.assert
		  ("applyPrim: intInf_comp, dstsize",
		   fn () => dstsize = wordSize)
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: intInf_comp, src1size",
		   fn () => src1size = pointerSize)
	      val _ 
		= Assert.assert
		  ("applyPrim: intInf_comp, src2size",
		   fn () => src2size = pointerSize)
	      val live = getPrimInfoNormal ()
	    in
	      (applyFF {target = f,
			args = [(src1,src2size),
				(src2,src2size)],
			dst = SOME (dst,dstsize),
			live = live,
			liveInfo = liveInfo})
	    end

	fun intInf_binop f
	  = let
	      val (dst,dstsize) = getDst ()
	      val _ 
		= Assert.assert
		  ("applyPrim: intInf_binop, dstsize",
		   fn () => dstsize = pointerSize)
	      val ((src1,src1size),
		   (src2,src2size),
		   (src3,src3size)) = getSrc3 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: intInf_binop, src1size",
		   fn () => src1size = pointerSize)
	      val _ 
		= Assert.assert
		  ("applyPrim: intInf_binop, src2size",
		   fn () => src2size = pointerSize)
	      val _ 
		= Assert.assert
		  ("applyPrim: intInf_binop, src3size",
		   fn () => src3size = pointerSize)

	      val live = getPrimInfoNormal ()
	    in
	      AppendList.appends
	      [(* intInfTemp = f(src1,src2,src3,frontier) *)
	       applyFF {target = f,
			args = [(src1,src2size),
				(src2,src2size),
				(src3,src3size),
				(gcState_frontierContentsOperand, pointerSize)],
			dst = SOME (intInfTempContentsOperand, pointerSize),
			live = live,
			liveInfo = liveInfo},
	       AppendList.fromList
	       [Block.T'
		{entry = NONE,
		 profileInfo = ProfileInfo.none,
		 statements 
		 = [(* gcState.frontier = intInfTemp->frontier *)
		    Assembly.instruction_mov
		    {dst = gcState_frontierContentsOperand,
		     src = intInfTempFrontierContentsOperand,
		     size = pointerSize},
		    (* dst = intInfTemp->value *)
		    Assembly.instruction_mov
		    {dst = dst,
		     src = intInfTempValueContentsOperand,
		     size = dstsize}],
		 transfer = NONE}]]
	    end

	fun intInf_unop f
	  = let
	      val (dst,dstsize) = getDst ()
	      val _ 
		= Assert.assert
		  ("applyPrim: intInf_unop, dstsize",
		   fn () => dstsize = pointerSize)
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: intInf_unop, src1size",
		   fn () => src1size = pointerSize)
	      val _ 
		= Assert.assert
		  ("applyPrim: intInf_unop, src2size",
		   fn () => src2size = pointerSize)

	      val live = getPrimInfoNormal ()
	    in
	      AppendList.appends
	      [(* intInfTemp = f(src1,src2,frontier) *)
	       applyFF {target = f,
			args = [(src1,src2size),
				(src2,src2size),
				(gcState_frontierContentsOperand, pointerSize)],
			dst = SOME (intInfTempContentsOperand, pointerSize),
			live = live,
			liveInfo = liveInfo},
	       AppendList.fromList
	       [Block.T'
		{entry = NONE,
		 profileInfo = ProfileInfo.none,
		 statements 
		 = [(* gcState.frontier = intInfTemp->frontier *)
		    Assembly.instruction_mov
		    {dst = gcState_frontierContentsOperand,
		     src = intInfTempFrontierContentsOperand,
		     size = pointerSize},
		    (* dst = intInfTemp->value *)
		    Assembly.instruction_mov
		    {dst = dst,
		     src = intInfTempValueContentsOperand,
		     size = dstsize}],
		 transfer = NONE}]]
	    end

	fun mov ()
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: mov, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src,
		    size = srcsize}],
		transfer = NONE}]
	    end
	  
	fun movx oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: movx, dstsize/srcsize",
		   fn () => Size.lt(srcsize,dstsize))
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_movx
		   {oper = oper,
		    dst = dst,
		    src = src,
		    dstsize = dstsize,
		    srcsize = srcsize}],
		transfer = NONE}]
	    end

	fun xvom ()
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: xvom, dstsize/srcsize",
		   fn () => Size.lt(dstsize,srcsize))
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_xvom
		   {dst = dst,
		    src = src,
		    dstsize = dstsize,
		    srcsize = srcsize}],
		transfer = NONE}]
	    end

	fun binal oper
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val (dst,dstsize)
		= case dst
		    of NONE 
		     => (overflowCheckTempContentsOperand, src1size)
		     | SOME (dst,dstsize) => (dst,dstsize)
	      val _ 
		= Assert.assert
		  ("applyPrim: binal, dstsize/src1size/src2size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize)

	      (* Reverse src1/src2 when src1 and src2 are temporaries
	       * and the oper is commutative. 
	       *)
	      val (src1,src2)
		= if (oper = Instruction.ADD)
		     orelse
		     (oper = Instruction.ADC)
		     orelse
		     (oper = Instruction.AND)
		     orelse
		     (oper = Instruction.OR)
		     orelse
		     (oper = Instruction.XOR)
		    then case (Operand.deMemloc src1, Operand.deMemloc src2)
			   of (SOME memloc_src1, SOME memloc_src2)
			    => if x86Liveness.track memloc_src1
			          andalso
				  x86Liveness.track memloc_src2
				 then (src2,src1)
				 else (src1,src2)
			    | _ => (src1,src2)
		    else (src1,src2)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_binal
		   {oper = oper,
		    dst = dst,
		    src = src2,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun binal_check oper
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val (dst,dstsize)
		= case dst
		    of NONE => (NONE, src1size)
		     | SOME (dst,dstsize) => (SOME dst,dstsize)
	      val _ 
		= Assert.assert
		  ("applyPrim: binal_check, dstsize/src1size/src2size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize)
	      val _ 
		= Assert.assert
		  ("applyPrim: binal_check, oper",
		   fn () => oper = Instruction.ADD orelse
		            oper = Instruction.SUB)
	      val (OverflowLabel, 
		   liveNoOverflow)
		= getPrimInfoOverflow ()
	      val noOverflowLabel = Label.newString "noOverflow"

	      val _ = x86Liveness.LiveInfo.setLiveOperands
	              (liveInfo, 
		       noOverflowLabel, 
		       case dst
			 of SOME _
			  => overflowCheckTempContentsOperand::liveNoOverflow
			  | NONE => liveNoOverflow)
		
	      (* Reverse src1/src2 when src1 and src2 are temporaries
	       * and the oper is commutative. 
	       *)
	      val (src1,src2)
		= if (oper = Instruction.ADD)
		    then case (Operand.deMemloc src1, Operand.deMemloc src2)
			   of (SOME memloc_src1, SOME memloc_src2)
			    => if x86Liveness.track memloc_src1
			          andalso
				  x86Liveness.track memloc_src2
				 then (src2,src1)
				 else (src1,src2)
			    | _ => (src1,src2)
		    else (src1,src2)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = overflowCheckTempContentsOperand,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_binal
		   {oper = oper,
		    dst = overflowCheckTempContentsOperand,
		    src = src2,
		    size = dstsize}],
		transfer
		= SOME (Transfer.iff {condition = Instruction.O,
				      truee = OverflowLabel,
				      falsee = noOverflowLabel})},
	       Block.T'
	       {entry = SOME (Entry.jump {label = noOverflowLabel}),
		profileInfo = ProfileInfo.none,
		statements 
		= case dst
		    of NONE => []
		     | SOME dst
		     => [Assembly.instruction_mov
			 {dst = dst,
			  src = overflowCheckTempContentsOperand,
			  size = dstsize}],
		transfer = NONE}]
	    end

	fun pmd oper
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val (dst,dstsize)
		= case dst
		    of NONE 
		     => (overflowCheckTempContentsOperand, src1size)
		     | SOME (dst,dstsize) => (dst,dstsize)
	      val _ 
		= Assert.assert
		  ("applyPrim: pmd, dstsize/src1size/src2size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize)

	      (* Reverse src1/src2 when src1 and src2 are temporaries
	       * and the oper is commutative. 
	       *)
	      val (src1,src2)
		= if (oper = Instruction.IMUL)
		     orelse
		     (oper = Instruction.MUL)
		    then case (Operand.deMemloc src1, Operand.deMemloc src2)
			   of (SOME memloc_src1, SOME memloc_src2)
			    => if x86Liveness.track memloc_src1
			          andalso
				  x86Liveness.track memloc_src2
				 then (src2,src1)
				 else (src1,src2)
			    | _ => (src1,src2)
		    else (src1,src2)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_pmd
		   {oper = oper,
		    dst = dst,
		    src = src2,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun pmd_check oper
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val (dst,dstsize)
		= case dst
		    of NONE => (NONE, src1size)
		     | SOME (dst,dstsize) => (SOME dst, dstsize)
	      val _ 
		= Assert.assert
		  ("applyPrim: pmd_check, dstsize/src1size/src2size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize)
	      val _ 
		= Assert.assert
		  ("applyPrim: pmd_check, oper",
		   fn () => oper = Instruction.IMUL)
	      val (OverflowLabel, 
		   liveNoOverflow)
		= getPrimInfoOverflow ()
	      val noOverflowLabel = Label.newString "noOverflow"

	      val _ = x86Liveness.LiveInfo.setLiveOperands
	              (liveInfo, 
		       noOverflowLabel, 
		       case dst
			 of SOME _ 
			  => overflowCheckTempContentsOperand::liveNoOverflow
			  | NONE => liveNoOverflow)

	      (* Reverse src1/src2 when src1 and src2 are temporaries
	       * and the oper is commutative. 
	       *)
	      val (src1,src2)
		= if (oper = Instruction.IMUL)
		    then case (Operand.deMemloc src1, Operand.deMemloc src2)
			   of (SOME memloc_src1, SOME memloc_src2)
			    => if x86Liveness.track memloc_src1
			          andalso
				  x86Liveness.track memloc_src2
				 then (src2,src1)
				 else (src1,src2)
			    | _ => (src1,src2)
		    else (src1,src2)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,	
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = overflowCheckTempContentsOperand,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_pmd
		   {oper = oper,
		    dst = overflowCheckTempContentsOperand,
		    src = src2,
		    size = dstsize}],
		transfer 
		= SOME (Transfer.iff {condition = Instruction.O,
				      truee = OverflowLabel,
				      falsee = noOverflowLabel})},
	       Block.T'
	       {entry = SOME (Entry.jump {label = noOverflowLabel}),
		profileInfo = ProfileInfo.none,
		statements 
		= case dst
		    of NONE => []
		     | SOME dst
		     => [Assembly.instruction_mov
			 {dst = dst,
			  src = overflowCheckTempContentsOperand,
			  size = dstsize}],
		transfer = NONE}]
	    end

	fun unal oper
	  = let
	      val (src,srcsize) = getSrc1 ()
	      val (dst,dstsize) 
		= case dst
		    of NONE
		     => (overflowCheckTempContentsOperand, srcsize)
		     | SOME (dst,dstsize) => (dst,dstsize)
	      val _ 
		= Assert.assert
		  ("applyPrim: unal, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src,
		    size = srcsize},
		   Assembly.instruction_unal
		   {oper = oper,
		    dst = dst,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun unal_check oper
	  = let
	      val (src,srcsize) = getSrc1 ()
	      val (dst,dstsize)
		= case dst
		    of NONE => (NONE, srcsize)
		     | SOME (dst,dstsize) => (SOME dst,dstsize)
	      val _ 
		= Assert.assert
		  ("applyPrim: unal_check, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	      val _
		= Assert.assert
		  ("applyPrim: unal_check, oper",
		   fn () => oper = Instruction.NEG)
	      val (OverflowLabel, 
		   liveNoOverflow)
		= getPrimInfoOverflow ()
	      val noOverflowLabel = Label.newString "noOverflow"

	      val _ = x86Liveness.LiveInfo.setLiveOperands
	              (liveInfo, 
		       noOverflowLabel, 
		       case dst
			 of SOME _
			  => overflowCheckTempContentsOperand::liveNoOverflow
			  | NONE => liveNoOverflow)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = overflowCheckTempContentsOperand,
		    src = src,
		    size = srcsize},
		   Assembly.instruction_unal
		   {oper = oper,
		    dst = overflowCheckTempContentsOperand,
		    size = dstsize}],
		transfer
		= SOME (Transfer.iff {condition = Instruction.O,
				      truee = OverflowLabel,
				      falsee = noOverflowLabel})},
	       Block.T'
	       {entry = SOME (Entry.jump {label = noOverflowLabel}),
		profileInfo = ProfileInfo.none,
		statements 
		= case dst
		    of NONE => []
		     | SOME dst
		     => [Assembly.instruction_mov
			 {dst = dst,
			  src = overflowCheckTempContentsOperand,
			  size = dstsize}],
		transfer = NONE}]
	    end

	fun sral oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: sral, dstsize/src1size",
		   fn () => src1size = dstsize)
	      val _ 
		= Assert.assert
		  ("applyPrim: sral, src2size",
		   fn () => src2size = wordSize)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src1,
		    size = dstsize},
		   Assembly.instruction_sral
		   {oper = oper,
		    dst = dst,
		    count = src2,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun cmp condition
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: cmp, src1size/src2size",
		   fn () => src1size = src2size)
	    in
	      (* Can't have an immediate in src1 position,
	       * so reverse the srcs and reverse the condition.
	       *
	       * This won't fix an immediate in both positions.
	       * Either constant folding eliminated it
	       * or the register allocator will raise an error.
	       *)
	      case Operand.deImmediate src1
		of SOME _ => AppendList.fromList
		             [Block.T'
			      {entry = NONE,
			       profileInfo = ProfileInfo.none,
			       statements
			       = [Assembly.instruction_cmp
				  {src1 = src2,
				   src2 = src1,
				   size = src1size},
				  Assembly.instruction_setcc
				  {condition 
				   = Instruction.condition_reverse condition,
				   dst = dst,
				   size = dstsize}],
			       transfer = NONE}]
		 | NONE => AppendList.fromList
			   [Block.T'
			    {entry = NONE,	
			     profileInfo = ProfileInfo.none,
			     statements
			     = [Assembly.instruction_cmp
				{src1 = src1,
				 src2 = src2,
				 size = src1size},
				Assembly.instruction_setcc
				{condition = condition,
				 dst = dst,
				 size = dstsize}],
			     transfer = NONE}]
	    end

	fun test condition
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: cmp, src1size/src2size",
		   fn () => src1size = src2size)
	    in
	      (* Can't have an immediate in src1 position,
	       * so reverse the srcs and reverse the condition.
	       *
	       * This won't fix an immediate in both positions.
	       * Either constant folding eliminated it
	       * or the register allocator will raise an error.
	       *)
	      case Operand.deImmediate src1
		of SOME _ => AppendList.fromList
		             [Block.T'
			      {entry = NONE,
			       profileInfo = ProfileInfo.none,
			       statements
			       = [Assembly.instruction_test
				  {src1 = src2,
				   src2 = src1,
				   size = src1size},
				  Assembly.instruction_setcc
				  {condition 
				   = Instruction.condition_reverse condition,
				   dst = dst,
				   size = dstsize}],
			       transfer = NONE}]
		 | NONE => AppendList.fromList
			   [Block.T'
			    {entry = NONE,
			     profileInfo = ProfileInfo.none,
			     statements
			     = [Assembly.instruction_test
				{src1 = src1,
				 src2 = src2,
				 size = src1size},
				Assembly.instruction_setcc
				{condition = condition,
				 dst = dst,
				 size = dstsize}],
			     transfer = NONE}]
	    end
	  
	fun fbina oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: fbina, dstsize/src1size/src2size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize)

	      (* Reverse src1/src2 when src1 and src2 are temporaries.
	       *)
	      val (oper,src1,src2)
		= case (Operand.deMemloc src1, Operand.deMemloc src2)
		    of (SOME memloc_src1, SOME memloc_src2) 
                     => if x86Liveness.track memloc_src1
		           andalso
			   x86Liveness.track memloc_src2
			  then (Instruction.fbina_reverse oper,src2,src1)
			  else (oper,src1,src2)
		     | _ => (oper,src1,src2)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_pfmov
		   {dst = dst,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_pfbina
		   {oper = oper,
		    dst = dst,
		    src = src2,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun fbina_fmul oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size),
		   (src3,src3size)) = getSrc3 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: fbina_fmul, dstsize/src1size/src2size/src3size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize andalso
			    src3size = dstsize)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_pfmov
		   {dst = dst,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_pfbina
		   {oper = Instruction.FMUL,
		    dst = dst,
		    src = src2,
		    size = dstsize},
		   Assembly.instruction_pfbina
		   {oper = oper,
		    dst = dst,
		    src = src3,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun funa oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: funa, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_pfmov
		   {dst = dst,
		    src = src,
		    size = srcsize},
		   Assembly.instruction_pfuna
		   {oper = oper,
		    dst = dst,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun flogarithm oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: logarithm, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in	
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_pfldc
		   {oper = oper,
		    dst = dst,
		    size = dstsize},
		   Assembly.instruction_pfbinasp
		   {oper = Instruction.FYL2X,
		    src = src,
		    dst = dst,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun real_ff1 f
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val live = getPrimInfoNormal ()
	    in 
	      applyFF {target = Label.fromString f,
		       args = [(src,srcsize)],
		       dst = SOME (dst,dstsize),
		       live = live,
		       liveInfo = liveInfo}
	    end 

	fun real_ff2 f
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val live = getPrimInfoNormal ()
	    in 
	      applyFF {target = Label.fromString f,
		       args = [(src1,src1size),
			       (src2,src2size)],
		       dst = SOME (dst,dstsize),
		       live = live,
		       liveInfo = liveInfo}
	    end 

	val (comment_begin,
	     comment_end)
	  = if !Control.Native.commented > 0
	      then let
		     val comment = primName
		   in 
		     (AppendList.single
		      (x86.Block.T'
		       {entry = NONE,
			profileInfo = x86.ProfileInfo.none,
			statements 
			= [x86.Assembly.comment 
			   ("begin applyPrim: " ^ comment)],
			transfer = NONE}),
		      AppendList.single
		      (x86.Block.T'
		       {entry = NONE,
			profileInfo = x86.ProfileInfo.none,
			statements 
			= [x86.Assembly.comment 
			   ("end applyPrim: " ^ comment)],
			transfer = NONE}))
		   end
	      else (AppendList.empty,AppendList.empty)
      in
	AppendList.appends
	[comment_begin,
	 (case Prim.name oper
	    of Array_length => lengthArrayVectorString ()
	     | Byte_byteToChar => mov ()
	     | Byte_charToByte => mov ()
	     | C_CS_charArrayToWord8Array => mov ()
	     | Char_lt => cmp Instruction.B
	     | Char_le => cmp Instruction.BE
	     | Char_gt => cmp Instruction.A
	     | Char_ge => cmp Instruction.AE
	     | Char_chr => xvom ()
	     | Char_ord => movx Instruction.MOVZX
	     | Cpointer_isNull 
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_cmp
		       {src1 = src,
			src2 = Operand.immediate_const_int 0,
			size = srcsize},
		       Assembly.instruction_setcc
		       {condition = Instruction.E,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | FFI s 
	     => (case Prim.numArgs oper
		   of NONE 
		    => let
			 val (dst,dstsize) = getDst ()

			 val memloc
			   = makeContents 
			     {base = Immediate.label (Label.fromString s),
			      size = dstsize,
			      class = Classes.CStatic}
		       in
			 AppendList.fromList
			 [Block.T'
			  {entry = NONE,
			   profileInfo = ProfileInfo.none,
			   statements
			   = [case Size.class dstsize
				of Size.INT 
				 => Assembly.instruction_mov 
				    {dst = dst,
				     src = Operand.memloc memloc,
				     size = dstsize}
				 | Size.FLT 
				 => Assembly.instruction_pfmov
				    {dst = dst,
				     src = Operand.memloc memloc,
				     size = dstsize}
				 | _ => Error.bug "applyPrim: FFI"],
			   transfer = NONE}]
		       end
 	            | SOME _ => let
				  val live = getPrimInfoNormal ()
				in 
				  applyFF {target = Label.fromString s,
					   args = args,
					   dst = dst,
					   live = live,
					   liveInfo = liveInfo}
				end)
	     | GC_collect 
	     => let
		  val info = getRuntimeInfo ()
		in 
		  invokeRuntime 
		  {target = Label.fromString "GC_gc",
		   args = [(Operand.immediate_label gcState, pointerSize),
			   (Operand.immediate_const_int 0, wordSize),
			   (Operand.immediate_const_int 1, wordSize),
			   (fileName, pointerSize),
			   (fileLine (), wordSize)],
		   info = info,	
		   frameLayouts = frameLayouts,
		   liveInfo = liveInfo}
		end
             | Int_add => binal Instruction.ADD
	     | Int_sub => binal Instruction.SUB
	     | Int_mul => pmd Instruction.IMUL
	     | Int_addCheck => binal_check Instruction.ADD
	     | Int_subCheck => binal_check Instruction.SUB
	     | Int_mulCheck => pmd_check Instruction.IMUL
	     | Int_quot => pmd Instruction.IDIV
	     | Int_rem => pmd Instruction.IMOD
	     | Int_neg => unal Instruction.NEG 
	     | Int_negCheck => unal_check Instruction.NEG 
	     | Int_lt => cmp Instruction.L
	     | Int_le => cmp Instruction.LE
	     | Int_gt => cmp Instruction.G
	     | Int_ge => cmp Instruction.GE
	     | Int_gtu => cmp Instruction.A
	     | Int_geu => cmp Instruction.AE
             | IntInf_compare 
	     => intInf_comp (Label.fromString "IntInf_compare")
	     | IntInf_equal 
	     => intInf_comp (Label.fromString "IntInf_equal")
  	     | IntInf_isSmall 
	     => let
	 	  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _ 
		    = Assert.assert
		      ("applyPrim: IntInf_isSmall, srcsize",
		       fn () => srcsize = wordSize)
	        in 
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_mov
		       {dst = intInfTempContentsOperand,
			src = src,
			size = srcsize},
		       Assembly.instruction_binal
		       {oper = Instruction.AND,
			dst = intInfTempContentsOperand,
			src = Operand.immediate_const_word 0wx1,
			size = srcsize},
		       Assembly.instruction_cmp
		       {src1 = intInfTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx0,
			size = srcsize},
		       Assembly.instruction_setcc
		       {condition = Instruction.NE,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | IntInf_areSmall
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size)) = getSrc2 ()
		  val _ 
		    = Assert.assert
		      ("applyPrim: IntInf_areSmall, src1size/src2size",
		       fn () => src1size = wordSize andalso
		                src2size = wordSize)
		in 
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_mov
		       {dst = intInfTempContentsOperand,
			src = src1,
			size = src1size},
		       Assembly.instruction_binal
		       {oper = Instruction.AND,
			dst = intInfTempContentsOperand,
			src = src2,
			size = src2size},
		       Assembly.instruction_binal
		       {oper = Instruction.AND,
			dst = intInfTempContentsOperand,
			src = Operand.immediate_const_word 0wx1,
			size = src1size},
		       Assembly.instruction_cmp
		       {src1 = intInfTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx0,
			size = src1size},
		       Assembly.instruction_setcc
		       {condition = Instruction.NE,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end 
	     | IntInf_add 
	     => intInf_binop (Label.fromString "IntInf_do_add")
	     | IntInf_sub 
	     => intInf_binop (Label.fromString "IntInf_do_sub")
	     | IntInf_mul 
	     => intInf_binop (Label.fromString "IntInf_do_mul")
	     | IntInf_quot 
	     => intInf_binop (Label.fromString "IntInf_do_quot")
	     | IntInf_rem 
	     => intInf_binop (Label.fromString "IntInf_do_rem")
	     | IntInf_neg 
	     => intInf_unop (Label.fromString "IntInf_do_neg")
	     | IntInf_toString
	     => let
		  val (dst,dstsize) = getDst()
		  val _ 
		    = Assert.assert
		      ("applyPrim: IntInf_toString, dstsize",
		       fn () => dstsize = pointerSize)
		  val ((src1,src1size),
		       (src2,src2size),
		       (src3,src3size)) = getSrc3 ()
		  val _ 
		    = Assert.assert
		      ("applyPrim: IntInf_toString, src1size/src2size/src3size",
		       fn () => src1size = pointerSize andalso
		                src2size = wordSize andalso
				src3size = pointerSize)

		  val live = getPrimInfoNormal ()
		in
		  AppendList.appends
		  [(* intInfTemp 
		    *    = IntInf_do_toString(src1,src2,src3,frontier) 
		    *)
		   applyFF {target = Label.fromString "IntInf_do_toString",
			    args = [(src1,src2size),
				    (src2,src2size),
				    (src3,src3size),
				    (gcState_frontierContentsOperand, pointerSize)],
			    dst = SOME (intInfTempContentsOperand, pointerSize),
			    live = live,
			    liveInfo = liveInfo},
		   AppendList.fromList
		   [Block.T'
		    {entry = NONE,
		     profileInfo = ProfileInfo.none,
		     statements 
		     = [(* gcState.frontier = intInfTemp->frontier *)
			Assembly.instruction_mov
			{dst = gcState_frontierContentsOperand,
			 src = intInfTempFrontierContentsOperand,
			 size = pointerSize},
			(* dst = intInfTemp->value *)
			Assembly.instruction_mov
			{dst = dst,
			 src = intInfTempValueContentsOperand,
			 size = dstsize}],
		     transfer = NONE}]]
		end
	     | IntInf_fromArray => mov ()
	     | IntInf_toVector => mov ()
	     | IntInf_fromWord => mov ()
	     | IntInf_toWord => mov ()
	     | MLton_eq => cmp Instruction.E
	     | MLton_halt 
	     => let
		  val (status,statussize) = getSrc1 ()
		  val info = getRuntimeInfo ()
		  val _ 
		    = Assert.assert
		      ("applyPrim: MLton_halt, statussize",
		       fn () => statussize = wordSize)
		in
		  AppendList.cons
		  ((* status might be of the form SX(?),
		    *  and invoke runtime will change the stackTop,
		    *  so copy the status to a local location.
		    *)
		   Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_mov
		       {dst = statusTempContentsOperand,
			src = status,
			size = statussize}],
		    transfer = NONE},
		   (invokeRuntime 
		    {target = Label.fromString "MLton_exit",
		     args = [(statusTempContentsOperand, statussize)],
		     info = info,	
		     frameLayouts = frameLayouts,
		     liveInfo = liveInfo}))
		end
	     | MLton_serialize => unimplemented primName
	     | MLton_deserialize => unimplemented primName
	     | MLton_size 
	     => let
		  val live = getPrimInfoNormal ()
		in 
		  applyFF {target = Label.fromString "MLton_size",
			   args = args,
			   dst = dst,
			   live = live,
			   liveInfo = liveInfo}
		end
	     | Real_Math_acos 
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_acos, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfmov
		       {dst = realTemp1ContentsOperand,
			src = src,
			size = srcsize},
		       Assembly.instruction_pfmov
		       {dst = realTemp2ContentsOperand,
			src = realTemp1ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FMUL,
			dst = realTemp2ContentsOperand,
			src = realTemp2ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfldc
		       {oper = Instruction.ONE,
			dst = realTemp3ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FSUB,
			dst = realTemp3ContentsOperand,
			src = realTemp2ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfuna
		       {oper = Instruction.FSQRT,
			dst = realTemp3ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfmov
		       {dst = dst,
			src = realTemp3ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfbinasp
		       {oper = Instruction.FPATAN,
			src = realTemp1ContentsOperand,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_Math_asin
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_asin, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfmov
		       {dst = dst,
			src = src,
			size = srcsize},
		       Assembly.instruction_pfmov
		       {dst = realTemp1ContentsOperand,
			src = dst,
			size = srcsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FMUL,
			dst = realTemp1ContentsOperand,
			src = realTemp1ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfldc
		       {oper = Instruction.ONE,
			dst = realTemp2ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FSUB,
			dst = realTemp2ContentsOperand,
			src = realTemp1ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfuna
		       {oper = Instruction.FSQRT,
			dst = realTemp2ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfbinasp
		       {oper = Instruction.FPATAN,
			src = realTemp2ContentsOperand,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_Math_atan 
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_atan, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmov
		       {dst = dst,
			src = src,
			size = srcsize},
		       Assembly.instruction_pfldc
		       {oper = Instruction.ONE,
			dst = realTemp1ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfbinasp
		       {oper = Instruction.FPATAN,
			src = realTemp1ContentsOperand,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_Math_atan2 
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_atan2, dstsize/src1size/src2size",
		       fn () => src1size = dstsize andalso
		                src2size = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmov
		       {dst = dst,
			src = src1,
			size = src1size},
		       Assembly.instruction_pfbinasp
		       {oper = Instruction.FPATAN,
			src = src2,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_Math_cos => funa Instruction.FCOS
	     | Real_Math_cosh => real_ff1 "cosh"
	     | Real_Math_exp 
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_exp, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfldc
		       {oper = Instruction.L2E,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FMUL,
			src = src,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfmov
		       {src = dst,
			dst = realTemp1ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfuna
		       {oper = Instruction.FRNDINT,
			dst = realTemp1ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FSUB,
			src = realTemp1ContentsOperand,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfuna
		       {oper = Instruction.F2XM1,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfldc
		       {oper = Instruction.ONE,
			dst = realTemp2ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FADD,
			src = realTemp2ContentsOperand,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfbinas
		       {oper = Instruction.FSCALE,
			src = realTemp1ContentsOperand,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
 	     | Real_Math_ln => flogarithm Instruction.LN2
	     | Real_Math_log10 => flogarithm Instruction.LG2
	     | Real_Math_pow => real_ff2 "pow"
	     | Real_Math_sin => funa Instruction.FSIN
	     | Real_Math_sinh => real_ff1 "sinh"
	     | Real_Math_sqrt => funa Instruction.FSQRT
	     | Real_Math_tan
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_tan, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmov
		       {src = src,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfptan
		       {dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_Math_tanh => real_ff1 "tanh"
	     | Real_mul => fbina Instruction.FMUL
	     | Real_muladd => fbina_fmul Instruction.FADD
	     | Real_mulsub => fbina_fmul Instruction.FSUB
	     | Real_add => fbina Instruction.FADD
	     | Real_sub => fbina Instruction.FSUB
	     | Real_div => fbina Instruction.FDIV
	     | Real_lt 
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_lt, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfcom
		       {src1 = src2,
			src2 = src1,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_test
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx4500,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.Z,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_le
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_le, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfcom
		       {src1 = src2,
			src2 = src1,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_test
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx500,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.Z,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_equal
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_equal, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfucom
		       {src1 = src2,
			src2 = src1,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_binal
		       {oper = Instruction.AND,
			dst = fpswTempContentsOperand,
			src = Operand.immediate_const_word 0wx4500,
			size = Size.WORD},
		       Assembly.instruction_cmp
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx4000,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.E,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_gt
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_gt, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfcom
		       {src1 = src1,
			src2 = src2,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_test
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx4500,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.Z,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_ge
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_ge, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfcom
		       {src1 = src1,
			src2 = src2,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_test
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx500,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.Z,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_qequal
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_qequal, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfucom
		       {src1 = src2,
			src2 = src1,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_test
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx4400,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.NE,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_abs => funa Instruction.FABS
	     | Real_copysign => real_ff2 "copysign" 
	     | Real_frexp => real_ff2 "frexp"
	     | Real_fromInt 
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmovfi
		       {dst = dst,
			src = src,
			srcsize = srcsize,
			dstsize = dstsize}],
		    transfer = NONE}]
		end 
	     | Real_toInt
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmovti
		       {dst = dst,
			src = src,
			srcsize = srcsize,
			dstsize = dstsize}],
		    transfer = NONE}]
		end 
	     | Real_ldexp 
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size)) = getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_ldexp, dstsize/src1size",
		       fn () => src1size = dstsize)
		  val _
		    = Assert.assert
		      ("applyPrim: Real_qequal, src2size",
		       fn () => src2size = Size.LONG)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmovfi
		       {dst = realTemp1ContentsOperand,
			src = src2,
			srcsize = src2size,
			dstsize = dstsize},
		       Assembly.instruction_pfmov
		       {dst = dst,
			src = src1,
			size = dstsize},
		       Assembly.instruction_pfbinas
		       {oper = Instruction.FSCALE,
			dst = dst,
			src = realTemp1ContentsOperand,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_modf => real_ff2 "modf"
	     | Real_neg => funa Instruction.FCHS
	     | Real_round => funa Instruction.FRNDINT
	     | String_equal 
	     => let
		  val live = getPrimInfoNormal ()
		in 
		  applyFF {target = Label.fromString "String_equal",
			   args = args,
			   dst = dst,
			   live = live,
			   liveInfo = liveInfo}
		end
	     | String_fromCharVector => mov ()
	     | String_fromWord8Vector => mov ()
	     | String_size => lengthArrayVectorString ()
	     | String_toCharVector => mov ()
	     | String_toWord8Vector => mov ()
	     | Thread_copy => thread "GC_copyThread"
	     | Thread_copyShrink => thread "GC_copyThreadShrink"
	     | Thread_current
	     => let
		  val (dst,dstsize) = getDst ()
		  val _
		    = Assert.assert
		      ("applyPrim: Thread_current, dstsize",
		       fn () => dstsize = pointerSize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_mov
		       {dst = dst,
			src = gcState_currentThreadContentsOperand,
			size = wordSize}],
		    transfer = NONE}]
		end
	     | Thread_finishHandler => thread "GC_finishHandler"
(*	     | Thread_switchTo => unimplemented primName *)
(*	     | Thread_switchTo => thread_switchTo () *)
	     | Thread_switchTo => thread "GC_switchToThread"
(*	     | Thread_switchToCont => unimplemented primName *)
(*	     | Thread_switchToCont => thread_switchTo () *)
	     | Thread_switchToCont => thread "GC_switchToThread"
	     | Vector_length => lengthArrayVectorString ()
	     | Word8_toInt => movx Instruction.MOVZX
	     | Word8_toIntX => movx Instruction.MOVSX
	     | Word8_fromInt => xvom ()
	     | Word8_toLargeWord => movx Instruction.MOVZX
	     | Word8_toLargeWordX => movx Instruction.MOVSX
	     | Word8_fromLargeWord => xvom ()
	     | Word8_add => binal Instruction.ADD
	     | Word8_sub => binal Instruction.SUB
	     | Word8_andb => binal Instruction.AND
	     | Word8_orb => binal Instruction.OR
	     | Word8_xorb => binal Instruction.XOR
	     | Word8_mul => pmd Instruction.MUL
	     | Word8_div => pmd Instruction.DIV
	     | Word8_mod => pmd Instruction.MOD
	     | Word8_neg => unal Instruction.NEG
	     | Word8_notb => unal Instruction.NOT
	     | Word8_lt => cmp Instruction.B
	     | Word8_le => cmp Instruction.BE
	     | Word8_gt => cmp Instruction.A
	     | Word8_ge => cmp Instruction.AE
	     | Word8_rol => sral Instruction.ROL
	     | Word8_ror => sral Instruction.ROR
	     | Word8_lshift => sral Instruction.SHL
	     | Word8_rshift => sral Instruction.SHR
	     | Word8_arshift => sral Instruction.SAR
	     | Word8Array_subWord => subWord8ArrayVector ()
	     | Word8Array_updateWord => updateWord8Array ()
	     | Word8Vector_subWord => subWord8ArrayVector ()
	     | Word32_toIntX => mov ()
	     | Word32_fromInt => mov ()
	     | Word32_add => binal Instruction.ADD
	     | Word32_sub => binal Instruction.SUB
	     | Word32_andb => binal Instruction.AND
	     | Word32_orb => binal Instruction.OR
	     | Word32_xorb => binal Instruction.XOR
	     | Word32_mul => pmd Instruction.MUL
	     | Word32_div => pmd Instruction.DIV
	     | Word32_mod => pmd Instruction.MOD
	     | Word32_neg => unal Instruction.NEG
	     | Word32_notb => unal Instruction.NOT
	     | Word32_lt => cmp Instruction.B
	     | Word32_le => cmp Instruction.BE
	     | Word32_gt => cmp Instruction.A
	     | Word32_ge => cmp Instruction.AE
	     | Word32_rol => sral Instruction.ROL
	     | Word32_ror => sral Instruction.ROR
	     | Word32_lshift => sral Instruction.SHL
	     | Word32_rshift => sral Instruction.SHR
	     | Word32_arshift => sral Instruction.SAR
	     | World_save
	     => let
		  val (file,filesize) = getSrc1 ()
		  val info = getRuntimeInfo ()
		  val _
		    = Assert.assert
		      ("applyPrim: World_save, filesize",
		       fn () => filesize = pointerSize)
		in
		  AppendList.cons
		  ((* file might be of the form SX(?),
		    *  and invoke runtime will change the stackTop,
		    *  so copy the file to a local location.
		    *)
		   Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_mov
		       {dst = fileTempContentsOperand,
			src = file,
			size = filesize}],
		    transfer = NONE},
		   (invokeRuntime 
		    {target = Label.fromString "GC_saveWorld",
		     args = [(Operand.immediate_label gcState, pointerSize),
			     (fileTempContentsOperand, filesize),
			     (Operand.immediate_label saveGlobals, 
			      pointerSize)],
		     info = info,	
		     frameLayouts = frameLayouts,
		     liveInfo = liveInfo}))
		end
	     | _ 
	     => Error.bug ("applyPrim: strange Prim.name.t: " ^ primName)),
	 comment_end]
      end

  val bug_msg_label = Label.fromString "MLton_bug_msg"
  fun bug {liveInfo: x86Liveness.LiveInfo.t}
    = let
	val bugLabel = Label.newString "bug"
      in 
	AppendList.appends
	[AppendList.fromList
	 [Block.T'
	  {entry = NONE,
	   profileInfo = ProfileInfo.none,
	   statements = [],
	   transfer = SOME (Transfer.goto {target = bugLabel})},
	  Block.T'
	  {entry = SOME (Entry.jump {label = bugLabel}),
	   profileInfo = ProfileInfo.none,
	   statements = [],
	   transfer = NONE}],
	 (applyFF {target = Label.fromString "MLton_bug",
		   args = [(Operand.label bug_msg_label, 
			    pointerSize)],
		   dst = NONE,
		   live = [],
		   liveInfo = liveInfo}),
	 AppendList.fromList
	 [Block.T'
	  {entry = NONE,
	   profileInfo = ProfileInfo.none,
	   statements = [],
	   transfer = SOME (Transfer.goto {target = bugLabel})}]]
      end
end
