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
   structure Mtype = Type
   structure Cases = Cases
   structure ChunkLabel = ChunkLabel
   structure Prim = Prim
   structure PrimInfo = PrimInfo
end

structure Label =
   struct
      open MachineOutput.Label
      fun newNoname () = newString "L"
   end

structure Type =
   struct
      open MachineOutput.Type
	 
      fun name t =
	 case dest t of
	    Char => "C"
	  | Double => "D"
	  | Int => "I" 
	  | Pointer => "P"
	  | Uint => "U"
	  | Void => "V"
   end

structure Register =
   struct
      open MachineOutput.Register

      fun toMOut (x: t) = x
	 
      fun equals (r1, r2) = 
	 Type.equals (ty r1, ty r2) 
	 andalso index r1 = index r2
   end      

structure Global =
   struct
      open MachineOutput.Global

      fun toMOut (x: t) = x

      fun equals (g1, g2) = 
	 Type.equals (ty g1, ty g2)
	 andalso index g1 = index g2
   end

structure Operand =
   struct
      open MachineOutput.Operand

      fun toMOut (x: t) = x

      fun ensurePointer (x: t): unit =
	 Assert.assert ("ensurePointer", fn () =>
			Type.isPointer (ty x)
			andalso (case x of
				    ArrayOffset _ => true
				  | Contents _ => true
				  | Global _ => true
				  | GlobalPointerNonRoot _ => true
				  | Offset _ => true
				  | Register _ => true
				  | StackOffset _ => true
				  | _ => false))

      fun arrayOffset arg = (ensurePointer (#base arg); ArrayOffset arg)
      val castInt = CastInt
      val char = Char
      fun contents (z, t) = (ensurePointer z; Contents {oper = z, ty = t})
      val float = Float
      val global = Global
      val int = Int
      val intInf = IntInf
      val label = Label
      fun offset arg = (ensurePointer (#base arg); Offset arg)
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

structure GCInfo =
   struct
      datatype t = T of {(* Size of frame, including return address. *)
			 frameSize: int,
			 (* Live stack offsets. *)
			 live: Operand.t list,
			 return: Label.t option ref}

      fun layout (T {frameSize, live, return}) =
	 Layout.record [("frameSize", Int.layout frameSize),
			("live", List.layout Operand.layout live),
			("return", Option.layout Label.layout (!return))]
	 
      fun toMOut (T {frameSize, live, return})
	 = MachineOutput.GCInfo.T 
	   {frameSize = frameSize,
	    live = live,
	    return = case !return
		       of NONE => Error.bug "GCInfo.toMOut"
			| SOME l => l};

      fun make {frameSize, live} =
	 T {frameSize = Type.wordAlign frameSize,
	    live = live,
	    return = ref NONE}
   end

structure PrimInfo =
  struct
    datatype t =
      None
    | Runtime of GCInfo.t
    | Normal of Operand.t list

    val none = None
    val runtime = Runtime
    val normal = Normal

    val deRuntime = fn Runtime gcInfo => gcInfo
		     | _ => Error.bug "deRuntime"

    fun layout i
      = let open Layout
	in
	  case i
	    of None => empty
	     | Runtime gcInfo 
	     => seq [str "Runtime ",
		     record [("gcInfo", GCInfo.layout gcInfo)]]
	     | Normal live 
	     => seq [str "Normal ",
		     record [("live", List.layout Operand.layout live)]]
	  end

    local
      structure P = MachineOutput.PrimInfo
    in
      val toMOut =
	 fn None => P.None
	  | Runtime gcInfo 
	  => P.Runtime (GCInfo.toMOut gcInfo)
	  | Normal live 
	  => P.Normal (List.map(live,Operand.toMOut))
    end
  end
  
(* ------------------------------------------------- *)
(*                    LimitCheck                     *)
(* ------------------------------------------------- *)

structure LimitCheck =
   struct
      datatype info = No
       | Maybe of GCInfo.t
       | Yes of GCInfo.t
       | Stack of GCInfo.t

      fun layoutInfo i =
	 let open Layout
	 in case i of
	    No => str "No"
	  | Maybe i => seq [str "Maybe ", GCInfo.layout i]
	  | Stack i => seq [str "Stack ", GCInfo.layout i]
	  | Yes i => seq [str "Yes ", GCInfo.layout i]
	 end

      val live
	= fn No => []
	   | Maybe (GCInfo.T {live,...}) => live
	   | Stack (GCInfo.T {live,...}) => live
	   | Yes (GCInfo.T {live,...}) => live

      datatype t = T of {info: info,
			 bytes: int option ref}
	 
      fun toMOut (T {info, bytes}) =
	 case !bytes of
	    NONE => NONE
	  | SOME b =>
	       case info of
		  No => NONE
		| Maybe i => SOME {info = GCInfo.toMOut i,
				   bytes = b,
				   stackCheck = false}
		| Yes i => SOME {info = GCInfo.toMOut i,
				 bytes = b,
				 stackCheck = false}
		| Stack i => SOME {info = GCInfo.toMOut i,
				   bytes = b,
				   stackCheck = true}
		     
      fun new info = T {info = info, bytes = ref NONE}

      val limitSlop: int = 512 (* should agree with LIMIT_SLOP in gc.c *)
	 
      fun set (T {info, bytes}, n: int, newFrame): int =
	 let
	    fun yes i =
	       (newFrame i
		; bytes := SOME (if n >= limitSlop then n else 0)
		; 0)
	 in case info 
	    of No => n
	  | Maybe i => if n = 0 then 0 else yes i
	  | Yes i => yes i
	  | Stack i => yes i
	 end
   end

structure Statement =
   struct
      type allocateArray =
	 {user: {dst: Operand.t,
		 numElts: Operand.t,
		 numPointers: int,
		 numBytesNonPointers: int,
		 gcInfo: GCInfo.t},
	  limitCheck: {bytesPerElt: int,
		       bytesAllocated: int} option ref}
	 
      datatype t =
	 Noop
       | Move of {dst: Operand.t,
		  src: Operand.t}
       | Push of int
       | Assign of {dst: Operand.t option,
		    oper: Prim.t,
		    pinfo: PrimInfo.t,
		    args: Operand.t vector}
       | LimitCheck of LimitCheck.t
       | SetExnStackLocal of {offset: int}
       | SetExnStackSlot of {offset: int}
       | SetSlotExnStack of {offset: int}
       | Allocate of {dst: Operand.t,
		      size: int,
		      numPointers: int,
		      numWordsNonPointers: int,
		      stores: {offset: int,
			       value: Operand.t} list}
       | AllocateArray of allocateArray

      val layout =
	 let open Layout
	 in
	    fn Noop => str "Noop"
	     | Move {dst, src} =>
		  seq [Operand.layout dst, str " = ", Operand.layout src]
	     | Push i => seq [str "Push (", Int.layout i, str ")"]
	     | Assign {dst, oper, args, ...} =>
		  seq [Option.layout Operand.layout dst, str " = ",
		       Prim.layout oper, str " ",
		       Vector.layout Operand.layout args]
	     | LimitCheck _ => str "LimitCheck"
	     | SetExnStackLocal {offset} =>
		  seq [str "SetExnStackLocal ", Int.layout offset]
	     | SetExnStackSlot {offset} =>
		  seq [str "SetExnStackSlot ", Int.layout offset]
	     | SetSlotExnStack {offset} =>
		  seq [str "SetSlotExnStack ", Int.layout offset]
	     | Allocate {dst, ...} =>
		  seq [Operand.layout dst, str " = Allocate"]
	     | AllocateArray {user = {dst, ...}, ...} =>
		  seq [Operand.layout dst, str " = AllocateArray"]
	 end

      local
	 structure S = MachineOutput.Statement
      in
	 val toMOut =
	    fn Allocate {dst, size, numPointers, numWordsNonPointers, stores} 
	     => S.Allocate {dst = Operand.toMOut dst,
			    size = size,
			    numPointers = numPointers,
			    numWordsNonPointers = numWordsNonPointers,
			    stores = List.map (stores, fn {offset, value} =>
					       {offset = offset,
						value = Operand.toMOut value})}
	     | AllocateArray {user = {dst, numElts, numPointers,
				      numBytesNonPointers, 
				      gcInfo as GCInfo.T {live, ...}},
			      limitCheck} 
	     => S.AllocateArray {dst = Operand.toMOut dst,
				 numElts = Operand.toMOut numElts,
				 numPointers = numPointers,
				 numBytesNonPointers = numBytesNonPointers,
				 live = live,
				 limitCheck =
				 Option.map
				 (!limitCheck, fn {bytesPerElt, bytesAllocated} =>
				  {bytesPerElt = bytesPerElt,
				   bytesAllocated = bytesAllocated,
				   gcInfo = GCInfo.toMOut gcInfo})}
	     | Assign {dst, oper, pinfo, args} 
	     => S.Assign {dst = Option.map (dst, Operand.toMOut),
			  pinfo = PrimInfo.toMOut pinfo,
			  oper = oper,
			  args = Vector.toListMap (args, Operand.toMOut)}
	     | LimitCheck lc
	     => (case LimitCheck.toMOut lc 
		   of NONE => S.Noop
		    | SOME lc => S.LimitCheck lc)
	     | Move {dst, src} => S.Move {dst = Operand.toMOut dst,
					  src = Operand.toMOut src}
	     | Noop => S.Noop
	     | Push i => S.Push i
	     | SetExnStackLocal z => S.SetExnStackLocal z
	     | SetExnStackSlot z => S.SetExnStackSlot z
	     | SetSlotExnStack z => S.SetSlotExnStack z
      end

      fun push x =
	 if 0 = Int.rem (x, 4)
	   then Push x
	   else Error.bug "frame size must be word aligned"
      val pop = push o ~
	 
      fun foldOverOperands (s, b, f) =
	 case s of
	    Move {dst, src} => f (dst, f (src, b))
	  | Assign {dst, args, ...} =>
	       Vector.fold (args, Option.fold (dst, b, f), f)
	  | Allocate {dst, stores, ...} =>
	       List.fold (stores, f (dst, b), fn ({value, ...}, b) =>
			  f (value, b))
	  | AllocateArray {user = {dst, numElts, ...}, ...} =>
	       f (numElts, f (dst, b))
	  | _ => b

      fun move (arg as {dst, src}) =
	 if Operand.equals (dst, src)
	    then Noop
	 else Move arg

      val assign = Assign
      val limitCheck = LimitCheck o LimitCheck.new
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
			       size = wordSize (* min size *),
			       numPointers = 0,
			       numWordsNonPointers = 1,
			       stores = []}
		      else arg))
	 
      fun allocateArray (user as {numPointers, numBytesNonPointers, ...}) =
	 (checkArrayHeader (numPointers, numBytesNonPointers)
	  ; AllocateArray {user = user, limitCheck = ref NONE})
	 
      fun moves {srcs, dsts} =
	 List.map2 (srcs, dsts, fn (src, dst) =>
		    move {src = src, dst = dst})
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
(*		liveOffsets: {offset: int, ty: Type.t} list} list ref *)
	 
      val maxFrameSize = Int.^(2, 16)
	 
      fun add (frames: t, f as {size, ...}) =
	 if size >= maxFrameSize
	    then (Error.bug
		  (concat ["MLton cannot handle stack frames larger than ",
			   Int.toString maxFrameSize,
			   " bytes."]))
	 else List.push (frames, f)
   end

(* ------------------------------------------------- *)
(*                     Transfer                      *)
(* ------------------------------------------------- *)

structure Transfer =
   struct
      open MachineOutput.Transfer

      fun toMOut (x: t) = x

      val isSwitch =
	 fn Switch _ => true
	  | _ => false
	       
      val bug = Bug
      val farJump = FarJump
      val nearJump = NearJump
      val overflow = Overflow
      val return = Return
      val raisee = Raise
      val switchIP = SwitchIP

      fun switch (arg as {cases, default, ...}) =
	 let
	    fun doit cases =
	       case (cases, default) of
		  ([], NONE) => bug
		| ([(_, l)], NONE) => nearJump {label = l, return = NONE}
		| ([], SOME l) => nearJump {label = l, return = NONE}
		| _ => Switch arg
	 in case cases of
	    Cases.Char l => doit l
	  | Cases.Int l => doit l
	  | Cases.Word l => doit l
	 end
   end

(* ------------------------------------------------- *)
(*                       Block                       *)
(* ------------------------------------------------- *)

structure Block =
   struct
     structure Kind =
       struct
	 open MachineOutput.Block.Kind

	 val func = Func
	 val jump = Jump
	 val cont = Cont
	 val handler = Handler

	 fun toMOut (x: t) = x
       end

      datatype t = T of {bytesNeeded: int option ref,
			 kind: Kind.t,
			 label: Label.t,
			 live: Operand.t list,
			 profileName: string,
			 statements: Statement.t array,
			 transfer: Transfer.t}

      fun clear (T {label, ...}) = PropertyList.clear (Label.plist label)
	 
      fun toMOut (T {label, kind, live, 
		     profileName, statements, transfer, ...}) =
	 MachineOutput.Block.T {label = label,
				kind = Kind.toMOut kind,
				live = live,
				statements = Array.map (statements,
							Statement.toMOut),
				transfer = Transfer.toMOut transfer,
				profileName = profileName}
   end


(* ------------------------------------------------- *)
(*                       Chunk                       *)
(* ------------------------------------------------- *)

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
				blocks = List.revMap (!blocks, Block.toMOut),
				regMax = ! o regMax}

      fun numRegsOfType (T {regMax, ...}, ty: Type.t): int = !(regMax ty)
	 
      fun numPointers (c) = numRegsOfType (c, Type.pointer)
	 
      (* ------------------------------------------------- *)
      (*                 insertLimitChecks                 *)
      (* ------------------------------------------------- *)
	 
      fun insertLimitChecks (self as T {chunkLabel, blocks,
					entries, gcReturns, ...},
			     frames): unit =
	 let
	    val returns: Label.t list ref = ref []

	    fun newFrame (GCInfo.T {frameSize, live, return, ...}) =
	       let val l = Label.newNoname ()
		   val liveOffsets
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
	       in List.push (returns, l)
		  ; Frames.add (frames, {return = l,
					 chunkLabel = chunkLabel,
					 size = frameSize,
					 offsets = liveOffsets})
		  ; return := SOME l
	       end
	    val {get = labelBlock, set = setLabelBlock, destroy} =
	       Property.destGetSetOnce
	       (Label.plist, Property.initRaise ("block", Label.layout))
	    open Block Transfer
	    val _ = List.foreach (!blocks, fn b as T {label, ...} =>
				  setLabelBlock (label, b))
	    fun memo (l: Label.t): int =
	       let
		  val T {bytesNeeded, label, statements, transfer, ...} =
		     labelBlock l
	       in
		  case !bytesNeeded of
		     SOME n => n
		   | NONE =>
			let
			   val _ = bytesNeeded := SOME 0
			   val goto = memo
			   val rest =
			      case transfer of
				 NearJump {label, ...} => goto label
			       | Overflow {failure, success, ...} =>
				    Int.max (goto failure, goto success)
			       | SwitchIP {int, pointer, ...} =>
				    Int.max (goto int, goto pointer)
			       | Switch {cases, default, ...} =>
				    Cases.fold
				    (cases, (case default of
						NONE => 0
					      | SOME l => goto l),
				     fn (j, rest) => Int.max (goto j, rest))
			       | _ => 0
			   fun allocateArray
			      ({user = {gcInfo, numElts, numPointers,
					numBytesNonPointers, ...},
				limitCheck}: Statement.allocateArray,
			       bytesAllocated: int): int =
			      let
				 val bytesPerElt =
				    if numPointers = 0
				       then numBytesNonPointers
				    else if numBytesNonPointers = 0
					    then numPointers * pointerSize
					 else Error.unimplemented "tricky arrays"
				 val bytesAllocated =
				    bytesAllocated
				    (* space for array header *)
				    + arrayHeaderSize
				    (* space for forwarding pointer for zero
				     * length arrays *)
				    + pointerSize
				 fun here () =
				    let val _ = newFrame gcInfo
				       val lc = {bytesPerElt = bytesPerElt,
						 bytesAllocated = bytesAllocated};
				    in limitCheck := SOME lc; 0
				    end
				 (* maxArrayLimitCheck is arbitrary -- it's just
				  * there to ensure that really huge array
				  * allocations don't get moved too early.
				  *)
				 val maxArrayLimitCheck = 10000
			      in case numElts of
				 Operand.Int numElts =>
				    if numElts <= maxArrayLimitCheck
				       then (bytesAllocated +
					     Type.align
					     (Type.pointer,
					      numElts * bytesPerElt))
				    else here ()
			       | _ => here ()
			      end
			   val bytesAllocated =
			      Array.foldr
			      (statements, rest,
			       fn (statement, bytesAllocated) =>
			       let datatype z = datatype Statement.t
			       in case statement of
				  AllocateArray r =>
				     allocateArray (r, bytesAllocated)
				| Allocate {size, ...} =>
				     objectHeaderSize + size + bytesAllocated
				| Assign {pinfo, ...} =>
				     (case pinfo
					 of PrimInfo.Runtime gcInfo =>
					    newFrame gcInfo
				       | _ => ()
					    ; bytesAllocated)
				| LimitCheck lc =>
				     LimitCheck.set
				     (lc, bytesAllocated, newFrame)
				| _ => bytesAllocated
			       end)
			in bytesNeeded := SOME bytesAllocated
			   ; bytesAllocated
			end
	       end
	    val _ = List.foreach (!entries, fn l => (memo l; ()))
	    val _ = gcReturns := SOME (!returns)
	    val _ = destroy ()
	    val _ =
	       Control.diagnostics
	       (fn display =>
		let
		   open Layout
		   val _ = display (seq [str "limit checks for ",
					 ChunkLabel.layout chunkLabel])
		   val _ =
		      List.foreach
		      (!blocks, fn Block.T {bytesNeeded, label, ...} =>
		       display
		       (seq [Label.layout label, str " ",
			     Option.layout Int.layout (!bytesNeeded)]))
		in
		   ()
		end)
	 in
	    ()
	 end
      
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
		    {label, kind, live, profileName, statements, transfer}) =
	 List.push
	 (blocks, Block.T {bytesNeeded = ref NONE,
			   label = label,
			   kind = kind,
			   live = live,
			   profileName = profileName,
			   statements = Array.fromList statements,
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
	       (* Insert limit checks. *)
	       val _ = List.foreach (chunks, fn c =>
				     Chunk.insertLimitChecks (c, frames))
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
	       val getFrameLayoutOffsetIndex 
		 = get o IntSet.fromList
(*
		   (fn offsets => List.keepAllMap
		                  (offsets,
				   fn {offset, ty}
				    => case Mtype.dest ty
					 of Mtype.Pointer => SOME offset
					  | _ => NONE))
*)
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
	       val globals: Type.t -> int = Counter.value o globalCounter
	       val globalsNonRoot: int = Counter.value (globalPointerNonRootCounter)
	       val chunks: MachineOutput.Chunk.t list =
		  List.revMap (chunks, Chunk.toMOut)
	       val main: {chunkLabel: ChunkLabel.t, label: Label.t} =
		  case !main of
		     NONE => Error.bug "main not set"
		   | SOME {chunkLabel, label} => {chunkLabel = chunkLabel, 
						  label = label}
	    in
	       MachineOutput.Program.T 
	       {globals = globals, 
		globalsNonRoot = globalsNonRoot, 
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

structure LimitCheck =
   struct
      datatype t = datatype LimitCheck.info

      val layout = LimitCheck.layoutInfo
      val live = LimitCheck.live
   end

end
