(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor x86MLtonBasic(S: X86_MLTON_BASIC_STRUCTS): X86_MLTON_BASIC =
struct

  open S
  open x86

  structure Runtime = Machine.Runtime

  (*
   * x86.Size.t equivalents
   *)
  val wordBytes = Runtime.wordSize
  val wordSize = Size.fromBytes wordBytes
  val wordScale = Scale.fromBytes wordBytes
  val pointerBytes = Runtime.pointerSize
  val pointerSize = Size.fromBytes pointerBytes
  val pointerScale = Scale.fromBytes pointerBytes
  val floatSize = Size.DBLE
  val floatBytes = Size.toBytes floatSize
  val normalHeaderBytes = Runtime.normalHeaderSize
  val arrayHeaderBytes = Runtime.arrayHeaderSize
  val intInfOverheadBytes = Runtime.intInfOverheadSize
   
  local
     datatype z = datatype Runtime.Type.dest
  in
    fun toX86Size' t
      = case t
	  of Char => x86.Size.BYTE
	   | Double => x86.Size.DBLE
	   | Int => x86.Size.LONG
	   | Pointer => x86.Size.LONG
	   | Uint => x86.Size.LONG
    val toX86Size = fn t => toX86Size' (Runtime.Type.dest t)
    fun toX86Scale' t
      = case t
	  of Char => x86.Scale.One
	   | Double => x86.Scale.Eight
	   | Int => x86.Scale.Four
	   | Pointer => x86.Scale.Four
	   | Uint => x86.Scale.Four
    val toX86Scale = fn t => toX86Scale' (Runtime.Type.dest t)
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
	val StaticTemp = MemLoc.Class.StaticTemp
	val CStack = MemLoc.Class.CStack
	val Code = MemLoc.Class.Code
	  
	val CStatic = new "CStatic"
	val StaticNonTemp = new "StaticNonTemp"

	val GCState = new "GCState"
	val GCStateHold = new "GCStateHold"
	val GCStateVolatile = new "GCStateVolatile"
	  
	val ThreadStack = new "ThreadStack"
      end

      val allClasses = ref x86.ClassSet.empty 
      val livenessClasses = ref x86.ClassSet.empty 
      val holdClasses = ref x86.ClassSet.empty 
      val volatileClasses = ref x86.ClassSet.empty
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
		     StaticTemp::
		     CStack::
		     Code::
		     CStatic::
		     StaticNonTemp::
		     GCState::
		     GCStateHold::
		     GCStateVolatile::
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
(*
		     GCStateVolatile::
*)
		     nil)

	    val _ = volatileClasses :=
	            x86.ClassSet.fromList
		    (
		     GCStateVolatile::
		     nil)

	    val _ = runtimeClasses :=
	            x86.ClassSet.fromList
		    (
		     Heap::
		     Stack::
		     Globals::
		     GCState::
		     GCStateHold::
		     GCStateVolatile::
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

  val makeContents = x86.MemLoc.makeContents
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
    val localC_base = Label.fromString "localuchar"
    val localD_base = Label.fromString "localdouble"
    val localI_base = Label.fromString "localint"
    val localP_base = Label.fromString "localpointer"
    val localU_base = Label.fromString "localuint"
    datatype z = datatype Runtime.Type.dest
  in
    fun local_base ty
      = case Runtime.Type.dest ty
	  of Char    => localC_base
	   | Double  => localD_base
	   | Int     => localI_base
	   | Pointer => localP_base
	   | Uint    => localU_base
  end

  local
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
    datatype z = datatype Runtime.Type.dest
  in
    fun global_base ty
      = case Runtime.Type.dest ty
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
   *  call to GC_gc is about 9 lines further (push 4 more arguments,
   *  adjust stackTop, save return label,
   *  save gcState.frontier and gcState.stackTop, make call).
   * However, there are probably cases where this is different.
   *
   * We also have another hack because with Cygwin, Label.toString appends
   * an _ to the beginning of each label.
   *)
  val fileLineLabel =
     Promise.lazy
     (fn () =>
      Label.fromString (case !Control.hostType of
			   Control.Cygwin => "_LINE__"
			 | Control.FreeBSD => "__LINE__"
			 | Control.Linux => "__LINE__"
			 | _ => Error.bug "x86 can't handle hostType"))
					 
  val fileLine
    = fn () => if !Control.debug
		 then Operand.immediate (Immediate.const_int 0)
		 else (Operand.immediate
		       (Immediate.binexp
			{oper = Immediate.Addition,
			 exp1 = Immediate.label (fileLineLabel ()),
			 exp2 = Immediate.const_int 9}))

  val gcState_label = Label.fromString "gcState"

  structure Field = Runtime.GCField
  fun make (f: Field.t, size, class) =
     let
	fun imm () =
	   Immediate.binexp
	   {oper = Immediate.Addition,
	    exp1 = Immediate.label gcState_label,
	    exp2 = Immediate.const_int (Field.offset f)}
	fun contents () =
	   makeContents {base = imm (),
			 size = size,
			 class = class}
	fun operand () = Operand.memloc (contents ())
     in
	(imm, contents, operand)
     end

  val gcState_operand =
     Operand.memloc (makeContents {base = Immediate.label gcState_label,
				   size = pointerSize,
				   class = Classes.StaticNonTemp})
  
  val (_, _, gcState_canHandleContentsOperand) =
     make (Field.CanHandle, wordSize, Classes.GCState)

  val (_, _, gcState_cardMapContentsOperand) =
     make (Field.CardMap, wordSize, Classes.GCState)

  val (gcState_currentThread, gcState_currentThreadContents,
        gcState_currentThreadContentsOperand) =
      make (Field.CurrentThread, pointerSize, Classes.GCState)

  val (_, gcState_frontierContents, gcState_frontierContentsOperand) =
     make (Field.Frontier, pointerSize, Classes.GCStateHold)

  val (_, _, gcState_limitContentsOperand) =
     make (Field.Limit, pointerSize, Classes.GCState)

  val (_, _, gcState_limitPlusSlopContentsOperand) =
     make (Field.LimitPlusSlop, pointerSize, Classes.GCState)

  val (_, _, gcState_maxFrameSizeContentsOperand) =
     make (Field.MaxFrameSize, pointerSize, Classes.GCState)

  val (_, _,  gcState_signalIsPendingContentsOperand) =
     make (Field.SignalIsPending, wordSize, Classes.GCState)

  val (_, gcState_stackBottomContents, gcState_stackBottomContentsOperand) =
     make (Field.StackBottom, pointerSize, Classes.GCState)

  val (_, _, gcState_stackLimitContentsOperand) =
     make (Field.StackLimit, pointerSize, Classes.GCState)

  val (gcState_stackTop, gcState_stackTopContents,
       gcState_stackTopContentsOperand) =
     make (Field.StackTop, pointerSize, Classes.GCStateHold)

  local
    val stackTopTemp = 
      Immediate.label (Label.fromString "stackTopTemp")
    val stackTopTempContents = 
      makeContents {base = stackTopTemp,
		    size = wordSize,
		    class = Classes.StaticTemp} 
    val stackTopTempContentsOperand = 
      Operand.memloc (stackTopTempContents)
  in
    val stackTopTemp = fn () => stackTopTemp
    val stackTopTempContents = fn () => stackTopTempContents
    val stackTopTempContentsOperand = fn () => stackTopTempContentsOperand
  end

  local
     fun make (contents, class) () =
	Operand.memloc (MemLoc.simple {base = contents (),
				       index = Immediate.const_int 0,
				       scale = wordScale,
				       size = pointerSize,
				       class = class})
  in
     val gcState_frontierDerefOperand =
	make (gcState_frontierContents, Classes.Heap)
     val gcState_stackTopDerefOperand =
	make (gcState_stackTopContents, Classes.Stack)
     val stackTopTempDerefOperand =
	make (stackTopTempContents, Classes.Stack)
  end

  fun gcState_stackTopMinusWordDeref () =
     MemLoc.simple {base = gcState_stackTopContents (), 
		    index = Immediate.const_int ~1,
		    scale = wordScale,
		    size = pointerSize,
		    class = Classes.Stack}
  fun gcState_stackTopMinusWordDerefOperand () =
     Operand.memloc (gcState_stackTopMinusWordDeref ())

  fun stackTopTempMinusWordDeref () =
     MemLoc.simple {base = stackTopTempContents (), 
		    index = Immediate.const_int ~1,
		    scale = wordScale,
		    size = pointerSize,
		    class = Classes.Stack}
  fun stackTopTempMinusWordDerefOperand () =
     Operand.memloc (stackTopTempMinusWordDeref ())

  fun gcState_currentThread_exnStackContents () =
     MemLoc.simple {base = gcState_currentThreadContents (),
		    index = Immediate.const_int 0,
		    size = pointerSize,
		    scale = wordScale,
		    class = Classes.Heap}
  fun gcState_currentThread_exnStackContentsOperand () =
     Operand.memloc (gcState_currentThread_exnStackContents ())

  (* init *)
  fun init () = let
		  val _ = Classes.initClasses ()
		in
		  ()
		end
end
