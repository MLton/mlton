(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Machine (S: MACHINE_STRUCTS): MACHINE = 
struct
    
open S
      
val wordSize: int = 4
val pointerSize = wordSize
val objectHeaderSize = wordSize
val arrayHeaderSize = 2 * wordSize
val intInfOverhead = arrayHeaderSize + wordSize (* for the sign *)

local
   open MachineOutput
in
   structure Cases = Cases
   structure ChunkLabel = ChunkLabel
   structure LimitCheck = LimitCheck
   structure Prim = Prim
   structure Type = Type
end

structure Label =
   struct
      open MachineOutput.Label

      fun newNoname () = newString "L"
   end

structure Register =
   struct
      open MachineOutput.Register

      fun equals (r1, r2) = 
	 Type.equals (ty r1, ty r2) 
	 andalso index r1 = index r2
   end      

structure Global =
   struct
      open MachineOutput.Global

      fun equals (g1, g2) = 
	 Type.equals (ty g1, ty g2)
	 andalso index g1 = index g2
   end

structure Operand =
   struct
      open MachineOutput.Operand

      fun isPointer (x: t): bool =
	 Type.isPointer (ty x)
	 andalso (case x of
		     ArrayOffset _ => true
		   | Contents _ => true
		   | Global _ => true
		   | GlobalPointerNonRoot _ => true
		   | Offset _ => true
		   | Register _ => true
		   | StackOffset _ => true
		   | _ => false)

      fun ensurePointer s (x: t): unit =
	 Assert.assert (concat ["ensurePointer:", s, ":", toString x], fn () =>
			isPointer x)
	 
      fun arrayOffset arg =
	 (ensurePointer "arrayOffset" (#base arg); ArrayOffset arg)
      val castInt = CastInt
      val char = Char
      fun contents (z, t) =
	 (ensurePointer "contents" z; Contents {oper = z, ty = t})
      val float = Float
      val global = Global
      val int = Int
      val intInf = IntInf
      val label = Label
      fun offset arg = (ensurePointer "offset" (#base arg); Offset arg)
      val pointer = Pointer
      val register = Register
      val maxStackOffset: int ref = ref 0
      fun stackOffset {offset, ty} =
	 let
	    val n = offset + Type.size ty
	    val _ = if n > !maxStackOffset then maxStackOffset := n else ()
	 in StackOffset {offset = offset, ty = ty}
	 end
      val uint = Uint
	 
      val deRegister =
	 fn Register r => SOME r
	  | _ => NONE

      val deStackOffset =
	 fn StackOffset s => SOME s
	  | _ => NONE

      val rec equals =
	 fn (ArrayOffset {base = b, offset = i, ...},
	     ArrayOffset {base = b', offset = i', ...}) =>
	        equals (b, b') andalso equals (i, i') 
	   | (CastInt z, CastInt z') => equals (z, z')
	   | (Char c, Char c') => c = c'
	   | (Contents {oper = z, ...}, Contents {oper = z', ...}) => equals (z, z')
	   | (Float f, Float f') => f = f'
	   | (Int n, Int n') => n = n'
	   | (IntInf w, IntInf w') => Word.equals (w, w')
	   | (Offset {base = b, offset = i, ...},
	      Offset {base = b', offset = i', ...}) => equals (b, b') andalso i = i' 
	   | (Pointer n, Pointer n') => n = n'
	   | (Register r, Register r') => Register.equals (r, r')
	   | (StackOffset {offset = n, ...}, StackOffset {offset = n', ...}) =>
		n = n'
	   | (Uint w, Uint w') => w = w'
	   | _ => false

      fun interfere {write: t, read: t}: bool =
	 let fun inter read = interfere {write = write, read = read}
	 in case (read, write) 
	    of (ArrayOffset {base, offset, ...}, _) => 
	       inter base orelse inter offset
	  | (Contents {oper, ...}, _) => inter oper
	  | (Global g, Global g') => Global.equals (g, g')
	  | (GlobalPointerNonRoot i, GlobalPointerNonRoot j) => i = j
	  | (Offset {base, offset, ...}, _) => inter base
	  | (Register r, Register r') => Register.equals (r, r')
	  | (StackOffset {offset = off, ty = ty},
	     StackOffset {offset = off', ty = ty'}) =>
	       let 
		  val max = off + Type.size ty
		  val max' = off' + Type.size ty'
	       in max > off' andalso max' > off
	       end
	  | _ => false
	 end
   end

structure Statement =
   struct
      open MachineOutput.Statement

      fun push x =
	 if 0 = Int.rem (x, 4)
	   then Push x
	 else Error.bug "frame size must be word aligned"
	    
      val pop = push o ~
	 
      fun move (arg as {dst, src}) =
	 if Operand.equals (dst, src)
	    then Noop
	 else Move arg

      val assign = Assign
      val setExnStackLocal = SetExnStackLocal
      val setExnStackSlot = SetExnStackSlot
      val setSlotExnStack = SetSlotExnStack

      (* These checks, and in particular POINTER_BITS and NON_POINTER_BITS must
       * agree with runtime/gc.h.
       *)
      local
	 val POINTER_BITS: int = 15
	 val NON_POINTER_BITS: int = 15
	 fun make (p', np') (p, np) =
	    let val p' = Int.^(2, p')
	       val np' = Int.^(2, np')
	    in (if p < p'
		   then ()
		else Error.bug "object with too many pointers")
	       ; if np < np'
		    then ()
		 else Error.bug "object with too many non pointers"
	    end
      in
	 val checkObjectHeader = make (POINTER_BITS, NON_POINTER_BITS)
	 val checkArrayHeader = make (POINTER_BITS, NON_POINTER_BITS - 1)
      end
   
      fun allocate (arg as {dst, size, numPointers, numWordsNonPointers, ...}) =
	 (checkObjectHeader (numPointers, numWordsNonPointers)
	  ; Allocate (if size = 0
			 then {dst = dst,
			       numPointers = 0,
			       numWordsNonPointers = 1,
			       size = wordSize (* min size *),
			       stores = Vector.new0 ()}
		      else arg))
	 
      fun array (r as {numPointers, numBytesNonPointers, ...}) =
	 (checkArrayHeader (numPointers, numBytesNonPointers)
	  ; Array r)
	 
      fun moves {srcs, dsts} =
	 Vector.fromListRev
	 (Vector.fold2 (srcs, dsts, [], fn (src, dst, ac)  =>
			move {src = src, dst = dst} :: ac))
   end

(* ------------------------------------------------- *)
(*                   Frames                          *)
(* ------------------------------------------------- *)

structure Frames =
   struct
      type t = {return: Label.t,
		chunkLabel: ChunkLabel.t,
		size: int,
		offsets: int list} list ref
	 
      val maxFrameSize = Int.^ (2, 16)
	 
      fun add (frames: t, f as {size, ...}) =
	 if size >= maxFrameSize
	    then (Error.bug
		  (concat ["MLton cannot handle stack frames larger than ",
			   Int.toString maxFrameSize,
			   " bytes."]))
	 else List.push (frames, f)
   end

structure Transfer =
   struct
      open MachineOutput.Transfer

      val isSwitch =
	 fn Switch _ => true
	  | _ => false

      val arith = Arith
      val bug = Bug
      val ccall = CCall
      val farJump = FarJump
      val limitCheck = LimitCheck
      val nearJump = NearJump
      val raisee = Raise
      val return = Return
      val runtime = Runtime
      val switchIP = SwitchIP

      fun switch (arg as {cases, default, ...}) =
	 let
	    fun doit cases =
	       case (cases, default) of
		  ([], NONE) => bug
		| ([(_, l)], NONE) => nearJump {label = l, return = NONE}
		| ([], SOME l) => nearJump {label = l, return = NONE}
		| _ => Switch arg
	 in
	    case cases of
	       Cases.Char l => doit l
	     | Cases.Int l => doit l
	     | Cases.Word l => doit l
	 end
   end

structure Block =
   struct
      open MachineOutput.Block
	 
      structure Kind =
	 struct
	    open Kind

	    val func = Func
	    val jump = Jump
	    val cont = Cont
	    val creturn = CReturn
	    val handler = Handler
	 end
   end

structure Chunk =
   struct
      datatype t = T of {chunkLabel: ChunkLabel.t,
			 (* where to start *)
			 entries: Label.t list ref,
			 gcReturns: Label.t list option ref,
			 blocks: Block.t list ref,
			 (* for each type, gives the max # registers used *)
			 regMax: Type.t -> int ref}

      fun addEntry (T {entries, ...}, l) = List.push (entries, l)

      fun clear (T {blocks, ...}) = List.foreach (!blocks, Block.clear)

      fun toMOut (T {chunkLabel, entries, gcReturns, blocks, regMax, ...}) =
	 MachineOutput.Chunk.T {chunkLabel = chunkLabel,
				entries = !entries,
				gcReturns = (case !gcReturns of
						NONE => Error.bug "gcReturns"
					      | SOME gcReturns => gcReturns),
				blocks = !blocks,
				regMax = ! o regMax}

      fun numRegsOfType (T {regMax, ...}, ty: Type.t): int = !(regMax ty)
	 
      fun numPointers (c) = numRegsOfType (c, Type.pointer)
      
      fun label (T {chunkLabel, ...}) = chunkLabel
	 
      fun equals (T {blocks = r, ...}, T {blocks = r', ...}) = r = r'
	 
      fun new (): t =
	 T {chunkLabel = ChunkLabel.new (),
	    entries = ref [],
	    blocks = ref [],
	    regMax = Type.memo (fn _ => ref 0),
	    gcReturns = ref NONE}
	 
      fun register (T {regMax, ...}, n, ty) =
	 let val r = regMax ty
	 in r := Int.max (!r, n + 1)
	    ; Register.T {index = n, ty = ty}
	 end
      
      fun tempRegister (c as T {regMax, ...}, ty) =
	 register (c, !(regMax ty), ty)
	 
      fun newBlock (T {blocks, ...},
		    {label, kind, live, profileInfo, statements, transfer}) =
	 List.push
	 (blocks, Block.T {kind = kind,
			   label = label,
			   live = live,
			   profileInfo = profileInfo,
			   statements = statements,
			   transfer = transfer})
   end

(* ------------------------------------------------- *)
(*                      Program                      *)
(* ------------------------------------------------- *)

structure Program =
   struct
      datatype t = T of {main: {chunkLabel: ChunkLabel.t, 
				label: Label.t} option ref,
			 chunks: Chunk.t list ref,
			 globalCounter: Type.t -> Counter.t,
			 globalPointerNonRootCounter: Counter.t,
			 constantCounter: Type.t -> Counter.t,
			 strings: (Global.t * string) list ref,
			 intInfs: (Global.t * string) list ref,
			 floats: (Global.t * string) list ref,
			 frames: Frames.t,
			 handlers: {chunkLabel: ChunkLabel.t,
				    label: Label.t} list ref}

      fun clear (T {chunks, ...}) = List.foreach (!chunks, Chunk.clear)

      local
	 fun set sel (T r, z) = sel r := SOME z
      in
	 val setMain = set #main
      end

      fun new () =
	 let
	    val globalCounter = Type.memo (fn _ => Counter.new 0)
	    val constantCounter = Type.memo (fn _ => Counter.new 0)
	 in
	    T {main = ref NONE,
	       chunks = ref [],
	       globalCounter = globalCounter,
	       globalPointerNonRootCounter = Counter.new 0,
	       constantCounter = constantCounter,
	       strings = ref [],
	       intInfs = ref [],
	       floats = ref [],
	       frames = ref [],
	       handlers = ref []}
	 end

      fun newFrame (T {frames, ...}, 
		    {chunkLabel, live, return, size}) = 
	 let val liveOffsets
	       = List.fold
	         (live,
		  [],
		  fn (oper, liveOffsets)
		   => case Operand.deStackOffset oper
			of SOME {offset, ty} 
			 => (case Type.dest ty
			       of Type.Pointer => offset::liveOffsets
				| _ => liveOffsets)
			 | NONE => liveOffsets)
	 in Frames.add (frames, 
			{chunkLabel = chunkLabel,
			 offsets = liveOffsets,
			 return = return,
			 size = size})
	 end

      fun newHandler (T {handlers, ...}, {chunkLabel, label}) =
	 List.push (handlers, {chunkLabel = chunkLabel, label = label})

      fun newChunk (T {chunks, ...}) =
	 let
	    val c = Chunk.new ()
	    val _ = List.push (chunks, c)
	 in
	    c
	 end

      fun newGlobal (T {globalCounter, ...}, ty) =
	 Global.T {index = Counter.next (globalCounter ty), ty = ty}

      fun newGlobalPointerNonRoot (T {globalPointerNonRootCounter, ...}) =
	 Operand.GlobalPointerNonRoot
	 (Counter.next globalPointerNonRootCounter)

      local
	 fun make (sel, ty) (p as T r, s) =
	    let val g = newGlobal (p, ty)
	    in List.push (sel r, (g, s));
	       Operand.global g
	    end
      in
	 val newString = make (#strings, Type.pointer)
	 val newIntInf = make (#intInfs, Type.pointer)
	 val newFloat = make (#floats, Type.double)
      end

      val newGlobal = Operand.global o newGlobal

      local
	 structure IntSet = UniqueSet (val cacheSize: int = 1
				       val bits: int = 14
				       structure Element =
					  struct
					     open Int
					     fun hash n = Word.fromInt n
					  end)
      in			   
	 fun toMachineOutput (program as T {main, chunks, frames, handlers,
					    strings, intInfs, floats,
					    globalCounter,
					    globalPointerNonRootCounter,
					    constantCounter}) =
	    let
	       val chunks = !chunks
	       val _ = IntSet.reset ()
	       val c = Counter.new 0
	       val frameOffsets = ref []
	       val {get: IntSet.t -> int, ...} =
		  Property.get
		  (IntSet.plist,
		   Property.initFun
		   (fn offsets =>
		    let val index = Counter.next c
		    in List.push (frameOffsets, IntSet.toList offsets)
		       ; index
		    end))
	       val getFrameLayoutOffsetIndex = get o IntSet.fromList
	       val {get = getFrameLayout: Label.t -> {size: int,
						      offsetIndex: int} option,
		    set = setFrameLayout, ...} = 
		  Property.getSetOnce (Label.plist, Property.initConst NONE)
	       val {get = getNextChunk: Label.t -> ChunkLabel.t option,
		    set = setNextChunk, ...} =
		  Property.getSetOnce (Label.plist, Property.initConst NONE)
	       val _ = List.foreach (!handlers, fn {label, chunkLabel} =>
				     setNextChunk (label, SOME chunkLabel))
	       val _ =
		  List.foreach
		  (!frames, fn {return, chunkLabel, size, offsets} =>
		   (setNextChunk (return, SOME chunkLabel)
		    ; (setFrameLayout
		       (return,
			SOME {size = size,
			      offsetIndex = getFrameLayoutOffsetIndex offsets})))
		   )
	       val nextChunks: Label.t -> ChunkLabel.t option = getNextChunk
	       (* Reverse the list of frameOffsets because offsetIndex 
		* is from back of list.
		*)
	       val frameOffsets: int list list = List.rev (!frameOffsets)
	       val chunks: MachineOutput.Chunk.t list =
		  List.revMap (chunks, Chunk.toMOut)
	       val main: {chunkLabel: ChunkLabel.t, label: Label.t} =
		  case !main of
		     NONE => Error.bug "main not set"
		   | SOME {chunkLabel, label} => {chunkLabel = chunkLabel, 
						  label = label}
	    in
	       MachineOutput.Program.T 
	       {globals = Counter.value o globalCounter,
		globalsNonRoot = Counter.value globalPointerNonRootCounter,
		intInfs = !intInfs, 
		strings = !strings,
		floats = !floats,
		nextChunks = nextChunks, 
		frameOffsets = frameOffsets, 
		frameLayouts = getFrameLayout, 
		maxFrameSize = Type.wordAlign (!Operand.maxStackOffset),
		chunks = chunks,
		main = main}
	    end
      end
   end

end
