(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64MLtonBasic (S: AMD64_MLTON_BASIC_STRUCTS): AMD64_MLTON_BASIC =
struct

  open S
  open amd64

  local
     open Machine
  in
     structure CType = CType
     structure Runtime = Runtime
  end

  (*
   * amd64.Size.t equivalents
   *)
  val wordBytes = Bytes.toInt Bytes.inWord64
  val wordSize = Size.fromBytes wordBytes
  val wordScale = Scale.fromBytes wordBytes
  val pointerBytes = Bytes.toInt Bytes.inWord64
  val pointerSize = Size.fromBytes pointerBytes

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
        val CArg = MemLoc.Class.CArg
        val CStack = MemLoc.Class.CStack
        val Code = MemLoc.Class.Code

        val CStatic = new "CStatic"
        val StaticNonTemp = new "StaticNonTemp"

        val GCState = new "GCState"
        val GCStateHold = new "GCStateHold"
        val GCStateVolatile = new "GCStateVolatile"
      end

      val allClasses = ref amd64.ClassSet.empty 
      val livenessClasses = ref amd64.ClassSet.empty 
      val holdClasses = ref amd64.ClassSet.empty 
      val volatileClasses = ref amd64.ClassSet.empty
      val runtimeClasses = ref amd64.ClassSet.empty 
      val heapClasses = ref amd64.ClassSet.empty
      val cargClasses = ref amd64.ClassSet.empty 
      val cstaticClasses = ref amd64.ClassSet.empty 

      fun initClasses ()
        = let
            val _ = allClasses :=       
                    amd64.ClassSet.fromList
                    (
                     Heap::
                     Stack::
                     Locals::
                     Globals::
                     Temp::
                     StaticTemp::
                     CArg::
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
                       then amd64.ClassSet.fromList
                            (
                             Temp::
                             Locals::
                             StaticTemp::
                             Stack::
                             nil)
                       else amd64.ClassSet.fromList
                            (
                             Temp::
                             Locals::
                             StaticTemp::
                             nil))

            val _ = holdClasses :=
                    amd64.ClassSet.fromList
                    (
                     GCStateHold::
(*
                     GCStateVolatile::
*)
                     nil)

            val _ = volatileClasses :=
                    amd64.ClassSet.fromList
                    (
                     GCStateVolatile::
                     nil)

            val _ = runtimeClasses :=
                    amd64.ClassSet.fromList
                    (
                     Heap::
                     Stack::
                     Globals::
                     GCState::
                     GCStateHold::
                     GCStateVolatile::
                     nil)

            val _ = heapClasses :=
                    amd64.ClassSet.fromList
                    (
                     Heap::
                     nil)

            val _ = cstaticClasses :=
                    amd64.ClassSet.fromList
                    (
                     CStatic::
                     nil)
            val _ = cargClasses :=
                    amd64.ClassSet.fromList
                    (
                     CArg::
                     nil)
          in
            ()
          end
    end

  val makeContents = amd64.MemLoc.makeContents
  val c_stackP = Label.fromString "c_stackP"
  val c_stackPContents 
    = makeContents {base = Immediate.label c_stackP,
                    size = pointerSize,
                    class = Classes.StaticNonTemp}
  val c_stackPContentsOperand 
    = Operand.memloc c_stackPContents
  val c_stackPDerefWord
    = MemLoc.simple {base = c_stackPContents,
                     index = Immediate.zero,
                     scale = wordScale,
                     size = Size.QUAD,
                     class = Classes.CStack}
  val c_stackPDerefWordOperand
    = Operand.memloc c_stackPDerefWord
  val c_stackPDerefDouble
    = MemLoc.simple {base = c_stackPContents,
                     index = Immediate.zero,
                     scale = wordScale,
                     size = Size.DBLE,
                     class = Classes.CStack}
  val c_stackPDerefDoubleOperand
    = Operand.memloc c_stackPDerefDouble
  val c_stackPDerefFloat
    = MemLoc.simple {base = c_stackPContents,
                     index = Immediate.zero,
                     scale = wordScale,
                     size = Size.SNGL,
                     class = Classes.CStack}
  val c_stackPDerefFloatOperand
    = Operand.memloc c_stackPDerefFloat

  val applyFFTempFun = Label.fromString "applyFFTempFun"
  val applyFFTempFunContents 
    = makeContents {base = Immediate.label applyFFTempFun,
                    size = wordSize,
                    class = Classes.CStatic}
  val applyFFTempFunContentsOperand
    = Operand.memloc applyFFTempFunContents
  val applyFFTempRegArg = Label.fromString "applyFFTempRegArg"
  fun applyFFTempRegArgContents i
    = MemLoc.imm {base = Immediate.label applyFFTempRegArg,
                  index = Immediate.int i,
                  scale = Scale.Eight,
                  size = wordSize,
                  class = Classes.CArg}

  val applyFFTempXmmRegArgS = Label.fromString "applyFFTempXmmRegArgS"
  fun applyFFTempXmmRegArgSContents i
    = MemLoc.imm {base = Immediate.label applyFFTempXmmRegArgS,
                  index = Immediate.int i,
                  scale = Scale.Four,
                  size = Size.SNGL,
                  class = Classes.CArg}
  val applyFFTempXmmRegArgD = Label.fromString "applyFFTempXmmRegArgD"
  fun applyFFTempXmmRegArgDContents i
    = MemLoc.imm {base = Immediate.label applyFFTempXmmRegArgD,
                  index = Immediate.int i,
                  scale = Scale.Eight,
                  size = Size.DBLE,
                  class = Classes.CArg}
  fun applyFFTempXmmRegArgContents (floatSize, i)
    = case floatSize of
         Size.DBLE => applyFFTempXmmRegArgDContents i
       | Size.SNGL => applyFFTempXmmRegArgSContents i
       | _ => Error.bug "amd64MLtonBasic.applyFFTempXmmRegArgContents"

  val fpcvtTemp = Label.fromString "fpcvtTemp"
  val fpcvtTempContents 
    = makeContents {base = Immediate.label fpcvtTemp,
                    size = wordSize,
                    class = Classes.StaticTemp}
  val fpcvtTempContentsOperand
    = Operand.memloc fpcvtTempContents
  val fpeqTemp = Label.fromString "fpeqTemp"
  fun fpeqTempContents size
    = makeContents {base = Immediate.label fpeqTemp,
                    size = size, 
                    class = Classes.StaticTemp}
  fun fpeqTempContentsOperand size
    = Operand.memloc (fpeqTempContents size)

  local
     fun make prefix =
        let
           fun make name size = Label.fromString (concat [prefix, name, size])
           val r = make "Real"
           val w = make "Word"
           datatype z = datatype CType.t
        in
           CType.memo
           (fn t =>
            case t of
               CPointer => Label.fromString (concat [prefix, "CPointer"])
             | Int8 => w "8"
             | Int16 => w "16"
             | Int32 => w "32"
             | Int64 => w "64"
             | Objptr => Label.fromString (concat [prefix, "Objptr"])
             | Real32 => r "32"
             | Real64 => r "64"
             | Word8 => w "8"
             | Word16 => w "16"
             | Word32 => w "32"
             | Word64 => w "64")
        end
  in
     val local_base = make "local"
     val global_base = make "global"
  end

  val globalObjptrNonRoot_base = Label.fromString "globalObjptrNonRoot"

  val gcState_label = Label.fromString "gcState"

  structure Field = Runtime.GCField
  fun make' (offset: int, size, class) =
     let
        fun imm () =
           Immediate.labelPlusInt
           (gcState_label, offset)
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
           Immediate.labelPlusInt
           (gcState_label, Bytes.toInt (Field.offset f))
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
    val stackTopTempContents = fn () => stackTopTempContents
    val stackTopTempContentsOperand = fn () => stackTopTempContentsOperand
  end

  fun gcState_stackTopMinusWordDeref () =
     MemLoc.simple {base = gcState_stackTopContents (), 
                    index = Immediate.int ~1,
                    scale = wordScale,
                    size = pointerSize,
                    class = Classes.Stack}
  fun gcState_stackTopMinusWordDerefOperand () =
     Operand.memloc (gcState_stackTopMinusWordDeref ())

  fun stackTopTempMinusWordDeref () =
     MemLoc.simple {base = stackTopTempContents (), 
                    index = Immediate.int ~1,
                    scale = wordScale,
                    size = pointerSize,
                    class = Classes.Stack}
  fun stackTopTempMinusWordDerefOperand () =
     Operand.memloc (stackTopTempMinusWordDeref ())

  fun gcState_offset {offset, ty} =
    let
      val (_,_,operand) = 
        make' (offset, Vector.sub(amd64.Size.fromCType ty, 0), Classes.GCState)
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
