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
  structure CFunction = Machine.CFunction
  structure CType = CFunction.CType
  local
     open CType
  in
     structure IntSize = IntSize
     structure RealSize = RealSize
     structure WordSize = WordSize
  end

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
  val c_stackPDerefFloat
    = MemLoc.simple {base = c_stackPContents,
		     index = Immediate.const_int 0,
		     scale = wordScale,
		     size = Size.SNGL,
		     class = Classes.CStack}
  val c_stackPDerefFloatOperand
    = Operand.memloc c_stackPDerefFloat
				 
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

  val realTemp1D = Label.fromString "realTemp1D"
  val realTemp1ContentsD
    = makeContents {base = Immediate.label realTemp1D,
		    size = Size.DBLE,
		    class = Classes.StaticTemp}
  val realTemp1ContentsOperandD
    = Operand.memloc realTemp1ContentsD
  val realTemp1S = Label.fromString "realTemp1S"
  val realTemp1ContentsS
    = makeContents {base = Immediate.label realTemp1S,
		    size = Size.SNGL,
		    class = Classes.StaticTemp}
  val realTemp1ContentsOperandS
    = Operand.memloc realTemp1ContentsS
  fun realTemp1ContentsOperand floatSize
    = case floatSize of
        Size.DBLE => realTemp1ContentsOperandD
      | Size.SNGL => realTemp1ContentsOperandD
      | _ => Error.bug "realTemp1ContentsOperand: floatSize"

  val realTemp2D = Label.fromString "realTemp2D"
  val realTemp2ContentsD
    = makeContents {base = Immediate.label realTemp2D,
		    size = Size.DBLE,
		    class = Classes.StaticTemp}
  val realTemp2ContentsOperandD
    = Operand.memloc realTemp2ContentsD
  val realTemp2S = Label.fromString "realTemp2S"
  val realTemp2ContentsS
    = makeContents {base = Immediate.label realTemp2S,
		    size = Size.SNGL,
		    class = Classes.StaticTemp}
  val realTemp2ContentsOperandS
    = Operand.memloc realTemp2ContentsS
  fun realTemp2ContentsOperand floatSize
    = case floatSize of
        Size.DBLE => realTemp2ContentsOperandD
      | Size.SNGL => realTemp2ContentsOperandD
      | _ => Error.bug "realTemp2ContentsOperand: floatSize"

  val realTemp3D = Label.fromString "realTemp3D"
  val realTemp3ContentsD
    = makeContents {base = Immediate.label realTemp3D,
		    size = Size.DBLE,
		    class = Classes.StaticTemp}
  val realTemp3ContentsOperandD
    = Operand.memloc realTemp3ContentsD
  val realTemp3S = Label.fromString "realTemp3S"
  val realTemp3ContentsS
    = makeContents {base = Immediate.label realTemp3S,
		    size = Size.SNGL,
		    class = Classes.StaticTemp}
  val realTemp3ContentsOperandS
    = Operand.memloc realTemp3ContentsS
  fun realTemp3ContentsOperand floatSize
    = case floatSize of
        Size.DBLE => realTemp3ContentsOperandD
      | Size.SNGL => realTemp3ContentsOperandD
      | _ => Error.bug "realTemp3ContentsOperand: floatSize"

  val fpswTemp = Label.fromString "fpswTemp"
  val fpswTempContents 
    = makeContents {base = Immediate.label fpswTemp,
		    size = Size.WORD,
		    class = Classes.StaticTemp}
  val fpswTempContentsOperand
    = Operand.memloc fpswTempContents
  val fildTemp = Label.fromString "fildTemp"
  val fildTempContents 
    = makeContents {base = Immediate.label fildTemp,
		    size = Size.WORD,
		    class = Classes.StaticTemp}
  val fildTempContentsOperand
    = Operand.memloc fildTempContents

  val eq1Temp = Label.fromString "eq1Temp"
  val eq1TempContents 
    = makeContents {base = Immediate.label eq1Temp,
		    size = wordSize,
		    class = Classes.StaticTemp}
  val eq1TempContentsOperand
    = Operand.memloc eq1TempContents
  val eq2Temp = Label.fromString "eq2Temp"
  val eq2TempContents 
    = makeContents {base = Immediate.label eq2Temp,
		    size = wordSize,
		    class = Classes.StaticTemp}
  val eq2TempContentsOperand
    = Operand.memloc eq2TempContents


  local
    val localI_base =
       IntSize.memoize
       (fn s => Label.fromString (concat ["localInt", IntSize.toString s]))
    val localP_base = Label.fromString "localPointer"
    val localR_base =
       RealSize.memoize
       (fn s => Label.fromString (concat ["localReal", RealSize.toString s]))
    val localW_base =
       WordSize.memoize
       (fn s => Label.fromString (concat ["localWord", WordSize.toString s]))
    datatype z = datatype CType.t
    datatype z = datatype IntSize.t
  in
    fun local_base ty =
       case ty of
	  Int s => localI_base s
	| Pointer => localP_base
	| Real s => localR_base s
	| Word s => localW_base s
  end

  local
     fun make (name, memo, toString) =
	(memo (fn s =>
	       Label.fromString (concat ["global", name, toString s])),
	 memo (fn s =>
	       Label.fromString (concat ["num_global", name, toString s])))
     val (globalI_base, globalI_num) =
	make ("Int", IntSize.memoize, IntSize.toString)
     val globalP_base = Label.fromString "globalPointer"
     val globalP_num = Label.fromString "num_globalpointer"
     val (globalR_base, globalR_num) =
	make ("Real", RealSize.memoize, RealSize.toString)
     val (globalW_base, globalW_num) =
	make ("Word", WordSize.memoize, WordSize.toString)
    datatype z = datatype CType.t
    datatype z = datatype IntSize.t
  in
     fun global_base ty =
	case ty of
	   Int s => globalI_base s
	 | Pointer => globalP_base
	 | Real s => globalR_base s
	 | Word s => globalW_base s
  end

  val globalPointerNonRoot_base = Label.fromString "globalPointerNonRoot"

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
      Label.fromString (case !Control.hostOS of
			   Control.Cygwin => "_LINE__"
			 | Control.FreeBSD => "__LINE__"
			 | Control.Linux => "__LINE__"
			 | Control.NetBSD => "__LINE__"
			 | _ => Error.bug "x86 can't handle hostOS"))
					 
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
  fun make' (offset: int, size, class) =
     let
	fun imm () =
	   Immediate.binexp
	   {oper = Immediate.Addition,
	    exp1 = Immediate.label gcState_label,
	    exp2 = Immediate.const_int offset}
	fun contents () =
	   makeContents {base = imm (),
			 size = size,
			 class = class}
	fun operand () = Operand.memloc (contents ())
     in
	(imm, contents, operand)
     end
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

  val (_, gcState_exnStackContents,
       gcState_exnStackContentsOperand) =
     make (Field.ExnStack, wordSize, Classes.GCState)
  
  val (_, gcState_frontierContents, 
       gcState_frontierContentsOperand) =
     make (Field.Frontier, pointerSize, Classes.GCStateHold)

  val (_, gcState_stackBottomContents, 
       gcState_stackBottomContentsOperand) =
     make (Field.StackBottom, pointerSize, Classes.GCState)

  val (_, gcState_stackTopContents,
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

  fun gcState_offset {offset, ty} =
    let
      val (_,_,operand) = 
	make' (offset, Vector.sub(x86.Size.fromCType ty, 0), Classes.GCState)
    in
      operand ()
    end

  (* init *)
  fun init () = let
		  val _ = Classes.initClasses ()
		in
		  ()
		end
end
