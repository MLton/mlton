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
    open Machine.Type
  in
    fun toX86Size' t
      = case t
	  of Char => x86.Size.BYTE
	   | Double => x86.Size.DBLE
	   | Int => x86.Size.LONG
	   | Pointer => x86.Size.LONG
	   | Uint => x86.Size.LONG
    val toX86Size = fn t => toX86Size' (dest t)
    fun toX86Scale' t
      = case t
	  of Char => x86.Scale.One
	   | Double => x86.Scale.Eight
	   | Int => x86.Scale.Four
	   | Pointer => x86.Scale.Four
	   | Uint => x86.Scale.Four
    val toX86Scale = fn t => toX86Scale' (dest t)
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
      val heapClasses = ref x86.ClassSet.empty
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

	    val _ = heapClasses :=
	            x86.ClassSet.fromList
		    (
		     Heap::
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

  local
    open Machine.Type
    val cReturnTempBYTE = Label.fromString "cReturnTempB"
    val cReturnTempBYTEContents 
      = makeContents {base = Immediate.label cReturnTempBYTE,
		      size = x86.Size.BYTE,
		      class = Classes.StaticTemp}
    val cReturnTempDBLE = Label.fromString "cReturnTempD"
    val cReturnTempDBLEContents 
      = makeContents {base = Immediate.label cReturnTempDBLE,
		      size = x86.Size.DBLE,
		      class = Classes.StaticTemp}
    val cReturnTempLONG = Label.fromString "cReturnTempL"
    val cReturnTempLONGContents 
      = makeContents {base = Immediate.label cReturnTempLONG,
		      size = x86.Size.LONG,
		      class = Classes.StaticTemp}
  in
    fun cReturnTempContents size
      = case size
	  of x86.Size.BYTE => cReturnTempBYTEContents
	   | x86.Size.DBLE => cReturnTempDBLEContents
	   | x86.Size.LONG => cReturnTempLONGContents
	   | _ => Error.bug "cReturnTempContents: size"
    val cReturnTempContentsOperand = Operand.memloc o cReturnTempContents
  end

  val limitCheckTemp = Label.fromString "limitCheckTemp"
  val limitCheckTempContents 
    = makeContents {base = Immediate.label limitCheckTemp,
		    size = pointerSize,
		    class = Classes.StaticTemp}
  val limitCheckTempContentsOperand 
    = Operand.memloc limitCheckTempContents
  val gcFirstAuxTemp = Label.fromString "gcFirstAuxTemp"
  val gcFirstAuxTempContents 
    = makeContents {base = Immediate.label gcFirstAuxTemp,
		    size = pointerSize,
		    class = Classes.StaticTemp}
  val gcFirstAuxTempContentsOperand 
    = Operand.memloc gcFirstAuxTempContents
     
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

  val heapCheckTemp = Label.fromString "heapCheckTemp"
  val heapCheckTempContents =
     makeContents {base = Immediate.label heapCheckTemp,
		   size = pointerSize,
		   class = Classes.StaticTemp}
  val heapCheckTempContentsOperand =
     Operand.memloc heapCheckTempContents

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
    open Machine.Type
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
  end

  local
    open Machine.Type
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


  val gcState_frontier
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 0}
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

  val gcState_limit 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 4}
  val gcState_limitContents 
    = makeContents {base = gcState_limit,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_limitContentsOperand
    = Operand.memloc gcState_limitContents

  val gcState_stackTop 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 8}
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
  val gcState_stackTopMinusWordDeref
    = MemLoc.simple {base = gcState_stackTopContents, 
		     index = Immediate.const_int ~1,
		     scale = wordScale,
		     size = pointerSize,
		     class = Classes.Stack}
  val gcState_stackTopMinusWordDerefOperand
    = Operand.memloc gcState_stackTopMinusWordDeref

  val gcState_stackLimit 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 12}
  val gcState_stackLimitContents 
    = makeContents {base = gcState_stackLimit,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_stackLimitContentsOperand
    = Operand.memloc gcState_stackLimitContents 

  val gcState_currentThread 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 16}
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
		     index = Immediate.const_int 2,
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

  val gcState_base 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 24}
  val gcState_baseContents 
    = makeContents {base = gcState_base,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_baseContentsOperand
    = Operand.memloc gcState_baseContents

  val gcState_limitPlusSlop
     = Immediate.binexp {oper = Immediate.Addition,
			 exp1 = Immediate.label gcState,
			 exp2 = Immediate.const_int 36}
  val gcState_limitPlusSlopContents
     = makeContents {base = gcState_limitPlusSlop,
		     size = pointerSize,
		     class = Classes.GCState}
  val gcState_limitPlusSlopContentsOperand
     = Operand.memloc gcState_limitPlusSlopContents

  val gcState_stackBottom 
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 52}
  val gcState_stackBottomContents 
    = makeContents {base = gcState_stackBottom,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_stackBottomContentsOperand
    = Operand.memloc gcState_stackBottomContents 

  val gcState_maxFrameSize
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 56}
  val gcState_maxFrameSizeContents 
    = makeContents {base = gcState_maxFrameSize,
		    size = pointerSize,
		    class = Classes.GCState}
  val gcState_maxFrameSizeContentsOperand
    = Operand.memloc gcState_maxFrameSizeContents 

  val gcState_canHandle
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 132}
  val gcState_canHandleContents
    = makeContents {base = gcState_canHandle,
		    size = wordSize,
		    class = Classes.GCState}
  val gcState_canHandleContentsOperand
    = Operand.memloc gcState_canHandleContents

  val gcState_signalIsPending
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 268}
  val gcState_signalIsPendingContents
    = makeContents {base = gcState_signalIsPending,
		    size = wordSize,
		    class = Classes.GCState}
  val gcState_signalIsPendingContentsOperand
    = Operand.memloc gcState_signalIsPendingContents

  val gcState_numLCsLow
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 428}
  val gcState_numLCsLowContents
    = makeContents {base = gcState_numLCsLow,
		    size = wordSize,
		    class = Classes.GCState}
  val gcState_numLCsLowContentsOperand
    = Operand.memloc gcState_numLCsLowContents
  val gcState_numLCsHigh
    = Immediate.binexp {oper = Immediate.Addition,
			exp1 = Immediate.label gcState,
			exp2 = Immediate.const_int 432}
  val gcState_numLCsHighContents
    = makeContents {base = gcState_numLCsHigh,
		    size = wordSize,
		    class = Classes.GCState}
  val gcState_numLCsHighContentsOperand
    = Operand.memloc gcState_numLCsHighContents

  (*
   * GC related constants and functions
   *)
  val WORD_SIZE = Runtime.wordSize
  val POINTER_SIZE = Runtime.pointerSize
  val GC_OBJECT_HEADER_SIZE = Runtime.objectHeaderSize
  val GC_ARRAY_HEADER_SIZE = Runtime.arrayHeaderSize

  fun gcObjectHeader {nonPointers, pointers} =
     Immediate.const_word (Runtime.objectHeader
			   {numPointers = pointers,
			    numWordsNonPointers = nonPointers})

  fun gcArrayHeader {nonPointers, pointers} =
     Immediate.const_word (Runtime.arrayHeader
			   {numBytesNonPointers = nonPointers,
			    numPointers = pointers})
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

  val wordAlign : int -> int =
     fn p =>
     (Word.toInt (Runtime.wordAlign (Word.fromInt p))
      handle exn
      => Error.bug ("x86MLton.wordAlign::" ^
		    (case exn
			of Fail s => s
		      | Overflow => "Overflow"
		      | _ => "?")))

  type transInfo = {addData : x86.Assembly.t list -> unit,
		    frameLayouts: x86.Label.t ->
		                  {size: int,
				   frameLayoutsIndex: int} option,
		    live: x86.Label.t -> x86.Operand.t list,
		    liveInfo: x86Liveness.LiveInfo.t}

  fun applyFF {target : Label.t, 
	       args : (Operand.t * Size.t) list,
	       dst : (Operand.t * Size.t) option,
	       live : Operand.t list,
	       transInfo as {liveInfo, ...} : transInfo}
    = let
	val return = Label.newString "creturn"
	val _ = x86Liveness.LiveInfo.setLiveOperands
	        (liveInfo, return, live)

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
					   return = return,
					   dstsize = Option.map (dst, #2)})},
	 Block.T'
	 {entry = SOME (Entry.creturn {label = return, dst = dst}),
	  profileInfo = ProfileInfo.none,
	  statements = comment_end,
	  transfer = NONE}]
      end

  fun prim {prim : Prim.t,
	    args : (Operand.t * Size.t) vector,
	    dst : (Operand.t * Size.t) option,
	    transInfo as {addData, frameLayouts, live, liveInfo} : transInfo}
    = let
	val primName = Prim.toString prim
	datatype z = datatype Prim.Name.t

	fun getDst ()
	  = case dst
	      of SOME dst => dst
	       | NONE => Error.bug "applyPrim: getDst"
	fun getSrc1 ()
	  = Vector.sub (args, 0)
	    handle _ => Error.bug "applyPrim: getSrc1"
	fun getSrc2 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1))
	    handle _ => Error.bug "applyPrim: getSrc2"
	fun getSrc3 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1), Vector.sub (args, 2))
	    handle _ => Error.bug "applyPrim: getSrc3"

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
	      val (dst,dstsize) = getDst ()
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

	fun pmd oper
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val (dst,dstsize) = getDst ()
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

	fun imul2 ()
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val (dst,dstsize) = getDst ()
	      val _ 
		= Assert.assert
		  ("applyPrim: pmd, dstsize/src1size/src2size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize)

	      (* Reverse src1/src2 when src1 and src2 are temporaries
	       * and the oper is commutative. 
	       *)
	      val (src1,src2)
		= case (Operand.deMemloc src1, Operand.deMemloc src2)
		    of (SOME memloc_src1, SOME memloc_src2)
		     => if x86Liveness.track memloc_src1
		           andalso
			   x86Liveness.track memloc_src2
			  then (src2,src1)
			  else (src1,src2)
		     | _ => (src1,src2)
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
		   Assembly.instruction_imul2
		   {dst = dst,
		    src = src2,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun unal oper
	  = let
	      val (src,srcsize) = getSrc1 ()
	      val (dst,dstsize) = getDst ()
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
			   ("begin prim: " ^ comment)],
			transfer = NONE}),
		      AppendList.single
		      (x86.Block.T'
		       {entry = NONE,
			profileInfo = x86.ProfileInfo.none,
			statements 
			= [x86.Assembly.comment 
			   ("end prim: " ^ comment)],
			transfer = NONE}))
		   end
	      else (AppendList.empty,AppendList.empty)
      in
	AppendList.appends
	[comment_begin,
	 (case Prim.name prim
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
	     => (case Prim.numArgs prim
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
				 | _ => Error.bug "prim: FFI"],
			   transfer = NONE}]
		       end
 	            | SOME _ => Error.bug "prim: FFI")
             | Int_add => binal Instruction.ADD
	     | Int_sub => binal Instruction.SUB
	     | Int_mul => imul2 () 
	     | Int_quot => pmd Instruction.IDIV
	     | Int_rem => pmd Instruction.IMOD
	     | Int_neg => unal Instruction.NEG 
	     | Int_lt => cmp Instruction.L
	     | Int_le => cmp Instruction.LE
	     | Int_gt => cmp Instruction.G
	     | Int_ge => cmp Instruction.GE
	     | Int_gtu => cmp Instruction.A
	     | Int_geu => cmp Instruction.AE
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
	     | IntInf_fromArray => mov ()
	     | IntInf_toVector => mov ()
	     | IntInf_fromWord => mov ()
	     | IntInf_toWord => mov ()
	     | MLton_eq => cmp Instruction.E
	     | MLton_serialize => unimplemented primName
	     | MLton_deserialize => unimplemented primName
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
	     | Real_Math_sin => funa Instruction.FSIN
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
	     | Real_neg => funa Instruction.FCHS
	     | Real_round => funa Instruction.FRNDINT
	     | String_fromCharVector => mov ()
	     | String_fromWord8Vector => mov ()
	     | String_size => lengthArrayVectorString ()
	     | String_toCharVector => mov ()
	     | String_toWord8Vector => mov ()
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
(*
	     | Word32_mul => pmd Instruction.MUL
*)
	     | Word32_mul => imul2 ()
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
	     | _ => Error.bug ("prim: strange Prim.Name.t: " ^ primName)),
	 comment_end]
      end

  fun ccall {prim : Prim.t,
	     args : (Operand.t * Size.t) vector,
	     return : Label.t,
	     dstsize : Size.t option,
	     transInfo as {...} : transInfo}
    = let
	val primName = Prim.toString prim
	datatype z = datatype Prim.Name.t

	fun getDstsize ()
	  = case dstsize
	      of SOME dstsize => dstsize
	       | NONE => Error.bug "ccall: getDstsize"
	fun getSrc1 ()
	  = Vector.sub (args, 0)
	    handle _ => Error.bug "ccall: getSrc1"
	fun getSrc2 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1))
	    handle _ => Error.bug "ccall: getSrc2"
	fun getSrc3 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1), Vector.sub (args, 2))
	    handle _ => Error.bug "ccall: getSrc3"

	fun intInf_comp f
	  = let
	      val _
		= Assert.assert
		  ("ccall: intInf_comp, dstsize",
		   fn () => getDstsize () = wordSize)
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("ccall: intInf_comp, src1size",
		   fn () => src1size = pointerSize)
	      val _ 
		= Assert.assert
		  ("ccall: intInf_comp, src2size",
		   fn () => src2size = pointerSize)

	      val args = [(src1,src1size), (src2,src2size)]
	    in
	      AppendList.single
	      (Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements = [],
		transfer = SOME (Transfer.ccall {target = Label.fromString f,
						 args = args,
						 return = return,
						 dstsize = dstsize})})
	    end

	fun intInf_binop f
	  = let
	      val _ 
		= Assert.assert
		  ("ccall: intInf_binop, dstsize",
		   fn () => getDstsize () = pointerSize)
	      val ((src1,src1size),
		   (src2,src2size),
		   (src3,src3size)) = getSrc3 ()
	      val _ 
		= Assert.assert
		  ("ccall: intInf_binop, src1size",
		   fn () => src1size = pointerSize)
	      val _ 
		= Assert.assert
		  ("ccall: intInf_binop, src2size",
		   fn () => src2size = pointerSize)
	      val _ 
		= Assert.assert
		  ("ccall: intInf_binop, src3size",
		   fn () => src3size = pointerSize)

	      val args = [(src1,src2size),
			  (src2,src2size),
			  (src3,src3size),
			  (gcState_frontierContentsOperand, pointerSize)]
	    in
	      AppendList.single
	      ((* intInfTemp = f(src1,src2,src3,frontier) *)
	       Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements = [],
		transfer = SOME (Transfer.ccall {target = Label.fromString f,
						 args = args,
						 return = return,
						 dstsize = dstsize})})
	    end

	fun intInf_unop f
	  = let
	      val _ 
		= Assert.assert
		  ("ccall: intInf_unnop, dstsize",
		   fn () => getDstsize () = pointerSize)
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("ccall: intInf_unnop, src1size",
		   fn () => src1size = pointerSize)
	      val _ 
		= Assert.assert
		  ("ccall: intInf_unnop, src2size",
		   fn () => src2size = pointerSize)

	      val args = [(src1,src2size),
			  (src2,src2size),
			  (gcState_frontierContentsOperand, pointerSize)]
	    in
	      AppendList.single
	      ((* intInfTemp = f(src1,src2,frontier) *)
	       Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements = [],
		transfer = SOME (Transfer.ccall {target = Label.fromString f,
						 args = args,
						 return = return,
						 dstsize = dstsize})})
	    end

	fun real_ff1 f
	  = let
	      val (src,srcsize) = getSrc1 ()
	      val args = [(src,srcsize)]
	    in 
	      AppendList.single
	      (Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements = [],
		transfer = SOME (Transfer.ccall {target = Label.fromString f,
						 args = args,
						 return = return,
						 dstsize = dstsize})})
	    end

	fun real_ff2 f
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val args = [(src1,src1size), (src2,src2size)]
	    in 
	      AppendList.single
	      (Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements = [],
		transfer = SOME (Transfer.ccall {target = Label.fromString f,
						 args = args,
						 return = return,
						 dstsize = dstsize})})
	    end 
	  
	val comment_begin
	  = if !Control.Native.commented > 0
	      then let
		     val comment = primName
		   in
		     AppendList.single
		     (x86.Block.T'
		      {entry = NONE,
		       profileInfo = x86.ProfileInfo.none,
		       statements 
		       = [x86.Assembly.comment 
			  ("begin ccall: " ^ comment)],
		       transfer = NONE})
		   end
	      else AppendList.empty
      in
	AppendList.appends
	[comment_begin,
	 (case Prim.name prim
	    of FFI s
	     => (case Prim.numArgs prim
		   of NONE => Error.bug "ccall: FFI"
		    | SOME _ 
		    => AppendList.single
		       (Block.T'
			{entry = NONE,
			 profileInfo = ProfileInfo.none,
			 statements = [],
			 transfer = SOME (Transfer.ccall 
					  {target = Label.fromString s,
					   args = Vector.toList args,
					   return = return,
					   dstsize = dstsize})}))
	     | IntInf_compare => intInf_comp "IntInf_compare"
	     | IntInf_equal => intInf_comp "IntInf_equal"
	     | IntInf_add => intInf_binop "IntInf_do_add"
	     | IntInf_gcd => intInf_binop "IntInf_do_gcd"
	     | IntInf_mul => intInf_binop "IntInf_do_mul"
	     | IntInf_quot => intInf_binop "IntInf_do_quot"
	     | IntInf_rem => intInf_binop "IntInf_do_rem"
	     | IntInf_sub => intInf_binop "IntInf_do_sub"
	     | IntInf_neg => intInf_unop "IntInf_do_neg"
	     | IntInf_toString
	     => let
		  val _ 
		    = Assert.assert
		      ("ccall: IntInf_toString, dstsize",
		       fn () => getDstsize () = pointerSize)
		  val ((src1,src1size),
		       (src2,src2size),
		       (src3,src3size)) = getSrc3 ()
		  val _ 
		    = Assert.assert
		      ("ccall: IntInf_toString, src1size/src2size/src3size",
		       fn () => src1size = pointerSize andalso
		                src2size = wordSize andalso
				src3size = pointerSize)

		  val args = [(src1,src2size),
			      (src2,src2size),
			      (src3,src3size),
			      (gcState_frontierContentsOperand, pointerSize)]
		in
		  AppendList.single
		  ((* intInfTemp 
		    *    = IntInf_do_toString(src1,src2,src3,frontier) 
		    *)
		   Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements = [],
		    transfer = SOME (Transfer.ccall 
				     {target = Label.fromString "IntInf_do_toString",
				      args = args,
				      return = return,
				      dstsize = dstsize})})
		end
	     | MLton_bug 
	     => AppendList.single
		(Block.T'
		 {entry = NONE,
		  profileInfo = ProfileInfo.none,
		  statements = [],
		  transfer = SOME (Transfer.ccall 
				   {target = Label.fromString "MLton_bug",
				    args = Vector.toList args,
				    return = return,
				    dstsize = dstsize})})
	     | MLton_size
	     => AppendList.single
		(Block.T'
		 {entry = NONE,
		  profileInfo = ProfileInfo.none,
		  statements = [],
		  transfer = SOME (Transfer.ccall 
				   {target = Label.fromString "MLton_size",
				    args = Vector.toList args,
				    return = return,
				    dstsize = dstsize})})
	     | Real_Math_cosh => real_ff1 "cosh"
	     | Real_Math_pow => real_ff2 "pow"
	     | Real_Math_sinh => real_ff1 "sinh"
	     | Real_Math_tanh => real_ff1 "tanh"
	     | Real_copysign => real_ff2 "copysign" 
	     | Real_frexp => real_ff2 "frexp"
	     | Real_modf => real_ff2 "modf"
	     | String_equal
	     => AppendList.single
		(Block.T'
		 {entry = NONE,
		  profileInfo = ProfileInfo.none,
		  statements = [],
		  transfer = SOME (Transfer.ccall 
				   {target = Label.fromString "String_equal",
				    args = Vector.toList args,
				    return = return,
				    dstsize = dstsize})})
	     | _ => Error.bug ("ccall: strange Prim.Name.t: " ^ primName))]
      end

  fun creturn {prim : Prim.t,
	       label : Label.t,
	       dst : (Operand.t * Size.t) option,
	       transInfo as {liveInfo, live, ...} : transInfo}
    = let
	val primName = Prim.toString prim
	datatype z = datatype Prim.Name.t

	fun getDst ()
	  = case dst
	      of SOME dst => dst
	       | NONE => Error.bug "creturn: getDst"

	fun default ()
	  = let
	      val _ = x86Liveness.LiveInfo.setLiveOperands
	              (liveInfo, label, live label)
	    in 
	      AppendList.single
	      (x86.Block.T'
	       {entry = SOME (Entry.creturn {label = label,
					     dst = dst}),
		profileInfo = ProfileInfo.none,
		statements = [],
		transfer = NONE})
	    end

	fun intInf ()
	  = let
	      val (dst,dstsize) = getDst ()

	      val _ = x86Liveness.LiveInfo.setLiveOperands
	              (liveInfo, label, live label)
	    in
	      AppendList.single
	      (Block.T'
	       {entry = SOME (Entry.creturn 
			      {label = label,
			       dst = SOME (intInfTempContentsOperand, pointerSize)}),
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
		transfer = NONE})
	    end

	val comment_end
	  = if !Control.Native.commented > 0
	      then let
		     val comment = primName
		   in
		     AppendList.single
		     (x86.Block.T'
		      {entry = NONE,
		       profileInfo = x86.ProfileInfo.none,
		       statements 
		       = [x86.Assembly.comment 
			  ("end creturn: " ^ comment)],
		       transfer = NONE})
		   end
	      else AppendList.empty
      in
	AppendList.appends
	[(case Prim.name prim
	    of FFI s
	     => (case Prim.numArgs prim
		   of NONE => Error.bug "ccall: FFI"
		    | SOME _ => default ())
	     | IntInf_compare => default ()
	     | IntInf_equal => default ()
	     | IntInf_add => intInf ()
	     | IntInf_gcd => intInf ()
	     | IntInf_sub => intInf ()
	     | IntInf_mul => intInf ()
	     | IntInf_quot => intInf ()
	     | IntInf_rem => intInf ()
	     | IntInf_neg => intInf ()
	     | IntInf_toString => intInf ()
	     | MLton_bug => default ()
	     | MLton_size => default ()
	     | Real_Math_cosh => default ()
	     | Real_Math_pow => default ()
	     | Real_Math_sinh => default ()
	     | Real_Math_tanh => default ()
	     | Real_copysign => default ()
	     | Real_frexp => default ()
	     | Real_modf => default ()
	     | String_equal => default ()
	     | _ => Error.bug ("creturn: strange Prim.Name.t: " ^ primName)),
	comment_end]
      end

  fun runtimecall {prim : Prim.t,
		   args : (Operand.t * Size.t) vector,
		   return : Label.t,
		   transInfo as {frameLayouts, ...} : transInfo}
    = let
    	val primName = Prim.toString prim
	datatype z = datatype Prim.Name.t

	fun getSrc1 ()
	  = Vector.sub (args, 0)
	    handle _ => Error.bug "runtimecall: getSrc1"
	fun getSrc2 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1))
	    handle _ => Error.bug "runtimecall: getSrc2"
	fun getSrc3 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1), Vector.sub (args, 2))
	    handle _ => Error.bug "runtimecall: getSrc3"

	val frameSize = case frameLayouts return
			  of NONE => Error.bug "runtimecall: framesize"
			   | SOME {size, ...} => size

	fun thread ()
	  = let
	      val (thread,threadsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("runtimecall: thread",
		   fn () => threadsize = pointerSize)
	    in
	      AppendList.single
	      ((* thread might be of the form SX(?),
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
		transfer 
		= SOME (Transfer.runtime 
			{prim = prim,
			 args = [(Operand.immediate_label gcState, pointerSize),
				 (threadTempContentsOperand, threadsize)],
			 return = return,
			 size = frameSize})})
	    end

	fun thread_copyCurrent ()
	  = let
	    in
	      AppendList.single
	      (Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements = [],
		transfer 
		= SOME (Transfer.runtime 
			{prim = prim,
			 args = [(Operand.immediate_label gcState, pointerSize)],
			 return = return,
			 size = frameSize})})
	    end

	val comment_begin
	  = if !Control.Native.commented > 0
	      then let
		     val comment = primName
		   in
		     AppendList.single
		     (x86.Block.T'
		      {entry = NONE,
		       profileInfo = x86.ProfileInfo.none,
		       statements 
		       = [x86.Assembly.comment 
			  ("begin runtimecall: " ^ comment)],
		       transfer = NONE})
		   end
	      else AppendList.empty
      in
	AppendList.appends
	[comment_begin,
	 (case Prim.name prim
	    of GC_collect
	     => AppendList.single
	        (Block.T'
		 {entry = NONE,
		  profileInfo = ProfileInfo.none,
		  statements = [],
		  transfer 
		  = SOME (Transfer.runtime 
			  {prim = prim,
			   args = [(Operand.immediate_label gcState, pointerSize),
				   (Operand.immediate_const_int 0, wordSize),
				   (Operand.immediate_const_int 1, wordSize),
				   (fileName, pointerSize),
				   (fileLine (), wordSize)],
			   return = return,
			   size = frameSize})})
	     | MLton_halt
	     => let
		  val (status,statussize) = getSrc1 ()
		  val _ 
		    = Assert.assert
		      ("runtimecall: MLton_halt, statussize",
		       fn () => statussize = wordSize)
		in
		  AppendList.single
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
		    transfer 
		    = SOME (Transfer.runtime 
			    {prim = prim,
			     args = [(statusTempContentsOperand, statussize)],
			     return = return,
			     size = frameSize})})
		end
	     | Thread_copy => thread ()
	     | Thread_copyCurrent => thread_copyCurrent ()
	     | Thread_finishHandler => thread ()
	     | Thread_switchTo => thread ()
	     | World_save
	     => let
		  val (file,filesize) = getSrc1 ()
		  val _ 
		    = Assert.assert
		      ("runtimecall: World_save, filesize",
		       fn () => filesize = pointerSize)
		in
		  AppendList.single
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
		    transfer 
		    = SOME (Transfer.runtime 
			    {prim = prim,
			     args = [(Operand.immediate_label gcState, pointerSize),
				     (fileTempContentsOperand, filesize),
				     (Operand.immediate_label saveGlobals, 
				      pointerSize)],
			     return = return,
			     size = frameSize})})
		end
	     | _ => Error.bug ("runtimecall: strange Prim.Name.t: " ^ primName))]
      end

  fun runtimereturn {prim : Machine.Prim.t,
		     label : Label.t,
		     frameInfo : Entry.FrameInfo.t,
		     transInfo as {frameLayouts, live, liveInfo, ...} : transInfo}
    = let
        val primName = Prim.toString prim
        datatype z = datatype Prim.Name.t
  
        fun default ()
          = let
              val _ = x86Liveness.LiveInfo.setLiveOperands
                      (liveInfo, label, live label)
            in 
              AppendList.single
              (x86.Block.T'
               {entry = SOME (Entry.runtime {label = label,
                                             frameInfo = frameInfo}),
                profileInfo = ProfileInfo.none,
                statements = [],
                transfer = NONE})
            end
  
        val comment_end
          = if !Control.Native.commented > 0
              then let
                     val comment = primName
                   in
                     AppendList.single
                     (x86.Block.T'
                      {entry = NONE,
                       profileInfo = x86.ProfileInfo.none,
                       statements 
                       = [x86.Assembly.comment 
                          ("end runtimereturn: " ^ comment)],
                       transfer = NONE})
                   end
              else AppendList.empty
        in
        AppendList.appends
        [(case Prim.name prim
            of GC_collect => default ()
             | MLton_halt => default ()
             | Thread_copy => default ()
             | Thread_copyCurrent => default ()
             | Thread_finishHandler => default ()
             | Thread_switchTo => default ()
             | World_save => default ()
             | _ => Error.bug ("runtimereturn: strange Prim.Name.t: " ^ primName)),
         comment_end]
        end

  fun arith {prim : Prim.t,
	     args : (Operand.t * Size.t) vector,
	     dst : (Operand.t * Size.t),
	     overflow : Label.t,
	     success : Label.t,
	     transInfo as {addData, frameLayouts, live, liveInfo, ...} : transInfo}
    = let
	val primName = Prim.toString prim
	datatype z = datatype Prim.Name.t

	fun arg i = Vector.sub (args, i)
	  
	val (src1, src1size) = arg 0
	val (dst, dstsize) = dst
	val _ = Assert.assert
	        ("arith: dstsize/srcsize",
		 fn () => src1size = dstsize)
	fun check (src, statement, condition)
	  = AppendList.single
	    (x86.Block.T'
	     {entry = NONE,	
	      profileInfo = x86.ProfileInfo.none,
	      statements = [x86.Assembly.instruction_mov
			    {dst = dst,
			     src = src,
			     size = src1size},
			    statement],
	      transfer = SOME (x86.Transfer.iff
			       {condition = condition,
				truee = overflow,
				falsee = success})})
	fun binal (oper: x86.Instruction.binal, condition)
	  = let
	      val (src2, src2size) = arg 1
	      val _ = Assert.assert
		      ("arith: binal, dstsize/src2size",
		       fn () => src2size = dstsize)
	      (* Reverse src1/src2 when src1 and src2 are
	       * temporaries and the oper is commutative. 
	       *)
	      val (src1,src2)
		= if (oper = x86.Instruction.ADD)
		    then case (x86.Operand.deMemloc src1,
			       x86.Operand.deMemloc src2)
			   of (SOME memloc_src1, SOME memloc_src2)
			    => if x86Liveness.track memloc_src1
			          andalso
				  x86Liveness.track memloc_src2
				 then (src2,src1)
				 else (src1,src2)
			    | _ => (src1,src2)
		    else (src1,src2)
	    in
	      check (src1,
		     x86.Assembly.instruction_binal
		     {oper = oper,
		      dst = dst,
		      src = src2,
		      size = dstsize},
		     condition)
	    end
 	fun pmd (oper: x86.Instruction.md, condition)
  	  = let
 	      val (src2, src2size) = arg 1
 	      val _ = Assert.assert
 		      ("arith: pmd, dstsize/src2size",
 		       fn () => src2size = dstsize)
 	      (* Reverse src1/src2 when src1 and src2 are
 	       * temporaries and the oper is commutative. 
 	       *)
 	      val (src1, src2)
 		= if oper = x86.Instruction.MUL
 		    then case (x86.Operand.deMemloc src1,
 			       x86.Operand.deMemloc src2)
 			   of (SOME memloc_src1, SOME memloc_src2)
 			    => if x86Liveness.track memloc_src1
 			          andalso
 				  x86Liveness.track memloc_src2
 				 then (src2,src1)
 				 else (src1,src2)
 			    | _ => (src1,src2)
 		    else (src1,src2)
 	    in
 	      check (src1,
		     x86.Assembly.instruction_pmd
 		     {oper = oper,
 		      dst = dst,
 		      src = src2,
 		      size = dstsize},
		     condition)
 	    end
	fun unal (oper: x86.Instruction.unal, condition)
	  = let
	    in
	      check (src1,
		     x86.Assembly.instruction_unal 
		     {oper = oper,
		      dst = dst,
		      size = dstsize},
		     condition)
	    end
	fun imul2_check condition
	  = let
	      val (src2, src2size) = arg 1
	      val _ = Assert.assert
		      ("arith: imul2_check, dstsizesrc2size",
		       fn () => src2size = dstsize)
	      (* Reverse src1/src2 when src1 and src2 are
	       * temporaries and the oper is commutative. 
	       *)
	      val (src1, src2)
		= case (x86.Operand.deMemloc src1,
			x86.Operand.deMemloc src2)
		    of (SOME memloc_src1, SOME memloc_src2)
		     => if x86Liveness.track memloc_src1
		           andalso
			   x86Liveness.track memloc_src2
			  then (src2,src1)
			  else (src1,src2)
		     | _ => (src1,src2)
	    in
	      check (src1,
		     x86.Assembly.instruction_imul2
		     {dst = dst,
		      src = src2,
		      size = dstsize},
		     condition)
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
			   ("begin arith: " ^ comment)],
			transfer = NONE}),
		      AppendList.single
		      (x86.Block.T'
		       {entry = NONE,
			profileInfo = x86.ProfileInfo.none,
			statements 
			= [x86.Assembly.comment 
			   ("end arith: " ^ comment)],
			transfer = NONE}))
		   end
	      else (AppendList.empty,AppendList.empty)
      in
	AppendList.appends
	[comment_begin,
	 (case Prim.name prim of
	     Int_addCheck => binal (x86.Instruction.ADD, x86.Instruction.O)
	   | Int_subCheck => binal (x86.Instruction.SUB, x86.Instruction.O)
	   | Int_mulCheck => imul2_check x86.Instruction.O
	   | Int_negCheck => unal (x86.Instruction.NEG, x86.Instruction.O)
	   | Word32_addCheck => binal (x86.Instruction.ADD, x86.Instruction.C)
	   | Word32_mulCheck => pmd (x86.Instruction.MUL, x86.Instruction.C)
	   | _ => Error.bug ("arith: strange Prim.Name.t: " ^ primName))]
      end

  val bug_msg_label = Label.fromString "MLton_bug_msg"
  fun bug {transInfo as {addData, frameLayouts, live, liveInfo, ...} : transInfo}
    = let
	val bugLabel = Label.newString "bug"
	val _ = x86Liveness.LiveInfo.setLiveOperands
	        (liveInfo, bugLabel, [])
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
		   transInfo = transInfo}),
	 AppendList.fromList
	 [Block.T'
	  {entry = NONE,
	   profileInfo = ProfileInfo.none,
	   statements = [],
	   transfer = SOME (Transfer.goto {target = bugLabel})}]]
      end
end
