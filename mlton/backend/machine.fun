(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Machine (S: MACHINE_STRUCTS): MACHINE =
struct

open S

val wordSize: int = 4
   
structure ChunkLabel = IntUniqueId ()
structure Type = Mtype ()

structure SmallIntInf =
   struct
      type t = word
   end

structure Register =
   struct
      datatype t = T of {index: int,
			 ty: Type.t}

      local
	 fun make f (T r) = f r
      in
	 val index = make #index
	 val ty = make #ty
      end

      fun toString (T {index, ty}) =
         concat ["R", Type.name ty, "(", Int.toString index, ")"]
	 
      val layout = Layout.str o toString

      fun equals (r1, r2) = 
	 Type.equals (ty r1, ty r2) 
	 andalso index r1 = index r2
   end

structure Global =
   struct
      datatype t = T of {index: int,
			 ty: Type.t}

      local
	 fun make f (T r) = f r
      in
	 val index = make #index
	 val ty = make #ty
      end

      fun toString (T {index, ty}) =
         concat ["G", Type.name ty, "(", Int.toString index, ")"]
	 
      val layout = Layout.str o toString

      fun equals (g1, g2) = 
	 Type.equals (ty g1, ty g2)
	 andalso index g1 = index g2
   end

structure Operand =
   struct
      datatype t =
	 ArrayOffset of {base: t, offset: t, ty: Type.t}
       | CastInt of t
       | Char of char
       | Contents of {oper: t, ty: Type.t}
       | Float of string
       | Global of Global.t
       | GlobalPointerNonRoot of int
       | Int of int
       | IntInf of SmallIntInf.t
       | Label of Label.t
       | Offset of {base: t, offset: int, ty: Type.t}
       | Pointer of int
       | Register of Register.t
       | StackOffset of {offset: int, ty: Type.t}
       | Uint of Word.t

    val rec toString =
       fn ArrayOffset {base, offset, ty} =>
            concat ["X", Type.name ty, 
		    "(", toString base, ",", toString offset, ")"]
	| CastInt oper => concat ["PointerToInt (", toString oper, ")"]
	| Char c => Char.escapeC c
	| Contents {oper, ty} =>
	     concat ["C", Type.name ty, "(", toString oper, ")"]
	| Global g => Global.toString g
	| GlobalPointerNonRoot n =>
	     concat ["globalpointerNonRoot [", Int.toString n, "]"]
	| Int n => Int.toString n
	| IntInf w => concat ["SmallIntInf (", Word.toString w, ")"]
	| Label l => Label.toString l
	| Offset {base, offset, ty} =>
	     concat ["O", Type.name ty,
		     "(", toString base, ",", Int.toString offset, ")"]
	| Pointer n => concat ["IntAsPointer (", Int.toString n, ")"]
	| Register r => Register.toString r
	| StackOffset {offset, ty} =>
	     concat ["S", Type.name ty, "(", Int.toString offset, ")"]
	| Uint w => Word.toString w
        | Float s => s

    val layout = Layout.str o toString

    val ty =
	 fn ArrayOffset {ty, ...} => ty
	  | CastInt _ => Type.int
	  | Char _ => Type.char
	  | Contents {ty, ...} => ty
	  | Float _ => Type.double
	  | Global g => Global.ty g
	  | GlobalPointerNonRoot _ => Type.pointer
	  | Int _ => Type.int
	  | IntInf _ => Type.pointer
	  | Label _ => Type.label
	  | Offset {ty, ...} => ty
	  | Pointer _ => Type.pointer
	  | Register r => Register.ty r
	  | StackOffset {ty, ...} => ty
	  | Uint _ => Type.uint

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
      datatype t =
	 Allocate of {dst: Operand.t,
		      numPointers: int,
		      numWordsNonPointers: int,
		      size: int,
		      stores: {offset: int,
			       value: Operand.t} vector}
       | Array of {dst: Operand.t,
		   numElts: Operand.t,
		   numPointers: int,
		   numBytesNonPointers: int}
       | Assign of {dst: Operand.t option,
		    prim: Prim.t, 
		    args: Operand.t vector}
       | Move of {dst: Operand.t,
		  src: Operand.t}
       | Noop
       | SetExnStackLocal of {offset: int}
       | SetExnStackSlot of {offset: int}
       | SetSlotExnStack of {offset: int}

      val layout =
	 let
	    open Layout
	 in
	    fn Allocate {dst, ...} => seq [Operand.layout dst, str " = Allocate"]
	     | Array {dst, ...} => seq [Operand.layout dst, str " = Array"]
	     | Assign {dst, prim, args, ...} =>
		  seq [Option.layout Operand.layout dst, str " = ",
		       Prim.layout prim, str " ",
		       Vector.layout Operand.layout args]
	     | Move {dst, src} =>
		  seq [Operand.layout dst, str " = ", Operand.layout src]
	     | Noop => str "Noop"
	     | SetExnStackLocal {offset} =>
		  seq [str "SetExnStackLocal ", Int.layout offset]
	     | SetExnStackSlot {offset} =>
		  seq [str "SetExnStackSlot ", Int.layout offset]
	     | SetSlotExnStack {offset} =>
		  seq [str "SetSlotExnStack ", Int.layout offset]
	 end
 
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

structure Cases = MachineCases (structure Label = Label)
   
structure LimitCheck =
   struct
      datatype t =
	 Array of {bytesPerElt: int,
		   extraBytes: int, (* for subsequent allocation *)
		   numElts: Operand.t,
		   stackToo: bool}
       | Heap of {bytes: int,
		  stackToo: bool}
       | Signal
       | Stack

      fun layout (l: t): Layout.t =
	 let
	    open Layout
	 in
	    case l of
	       Array {bytesPerElt, extraBytes, numElts, stackToo} =>
		  seq [str "Array ",
		       record [("bytesPerElt", Int.layout bytesPerElt),
			       ("extraBytes", Int.layout extraBytes),
			       ("numElts", Operand.layout numElts),
			       ("stackToo", Bool.layout stackToo)]]
	     | Heap {bytes, stackToo} =>
		  seq [str "Heap ",
		       record [("bytes", Int.layout bytes),
			       ("stackToo", Bool.layout stackToo)]]
	     | Signal => str "Signal"
	     | Stack => str "Stack"
	 end
   end

structure Transfer =
   struct
      datatype t =
	 Arith of {prim: Prim.t,
		   args: Operand.t vector,
		   dst: Operand.t,
		   overflow: Label.t,
		   success: Label.t}
       | Bug
       | CCall of {args: Operand.t vector,
		   prim: Prim.t,
		   return: Label.t,
		   returnTy: Type.t option}
       | Call of {label: Label.t,
		  live: Operand.t vector,
		  return: {return: Label.t,
			   handler: Label.t option,
			   size: int} option}
       | Goto of Label.t
       | LimitCheck of {failure: Label.t,
			kind: LimitCheck.t,
			success: Label.t}
       | Raise
       | Return of {live: Operand.t vector}
       | Runtime of {args: Operand.t vector,
		     prim: Prim.t,
		     return: Label.t}
       | Switch of {test: Operand.t,
		    cases: Cases.t,
		    default: Label.t option}
       | SwitchIP of {test: Operand.t,
		      int: Label.t,
		      pointer: Label.t}

      fun layout t =
	 let
	    open Layout
	 in
	    case t of
	       Arith {prim, args, dst, overflow, success} =>
		  seq [str "Arith ",
		       record [("prim", Prim.layout prim),
			       ("args", Vector.layout Operand.layout args),
			       ("dst", Operand.layout dst),
			       ("overflow", Label.layout overflow),
			       ("success", Label.layout success)]]
	     | Bug => str "Bug"
	     | CCall {args, prim, return, returnTy} =>
		  seq [str "CCall",
		       record [("args", Vector.layout Operand.layout args),
			       ("prim", Prim.layout prim),
			       ("return", Label.layout return),
			       ("returnTy", Option.layout Type.layout returnTy)]]
	     | Call {label, live, return} => 
		  seq [str "Call ", 
		       record [("label", Label.layout label),
			       ("live", Vector.layout Operand.layout live),
			       ("return", Option.layout 
				(fn {return, handler, size} =>
				 record [("return", Label.layout return),
					 ("handler",
					  Option.layout Label.layout handler),
					 ("size", Int.layout size)])
				return)]]
	     | Goto l => seq [str "Goto ", Label.layout l]
	     | LimitCheck {failure, kind, success} =>
		  seq [str "LimitCheck",
		       record [("failure", Label.layout failure),
			       ("kind", LimitCheck.layout kind),
			       ("success", Label.layout success)]]
	     | Raise => str "Raise"
	     | Return {live} => 
		  seq [str "Return ",
		       record [("live", Vector.layout Operand.layout live)]]
	     | Runtime {args, prim, return} =>
		  seq [str "Runtime ",
		       record [("args", Vector.layout Operand.layout args),
			       ("prim", Prim.layout prim),
			       ("return", Label.layout return)]]
	     | Switch {test, cases, default} =>
		  seq [str "Switch ",
		       tuple [Operand.layout test,
			      Cases.layout cases,
			      Option.layout Label.layout default]]
	     | SwitchIP {test, int, pointer} =>
		  seq [str "SwitchIP ", tuple [Operand.layout test,
					       Label.layout int,
					       Label.layout pointer]]
	 end

      val isSwitch =
	 fn Switch _ => true
	  | _ => false
   end

structure FrameInfo =
   struct
      datatype t = T of {frameOffsetsIndex: int,
			 size: int}

      local
	 fun make f (T r) = f r
      in
	 val frameOffsetsIndex = make #frameOffsetsIndex
	 val size = make #size
      end
   
      fun layout (T {frameOffsetsIndex, size}) =
	 Layout.record [("frameOffsetsIndex", Int.layout frameOffsetsIndex),
			("size", Int.layout size)]

      val bogus = T {frameOffsetsIndex = ~1, size = ~1}

   end

structure Kind =
   struct
      datatype t =
	 Cont of {args: Operand.t vector,
		  frameInfo: FrameInfo.t}
       | CReturn of {dst: Operand.t option,
		     prim: Prim.t}
       | Func of {args: Operand.t vector}
       | Handler of {offset: int}
       | Jump
       | Runtime of {frameInfo: FrameInfo.t,
		     prim: Prim.t}

      fun layout k =
	 let
	    open Layout
	 in
	    case k of
	       Cont {args, frameInfo} =>
		  seq [str "Cont ",
		       record [("args", Vector.layout Operand.layout args),
			       ("frameInfo", FrameInfo.layout frameInfo)]]
	     | CReturn {dst, prim} =>
		  seq [str "CReturn ",
		       record [("dst", Option.layout Operand.layout dst),
			       ("prim", Prim.layout prim)]]
	     | Func {args} =>
		  seq [str "Func ",
		       record [("args", Vector.layout Operand.layout args)]]
	     | Handler {offset} =>
		  seq [str "Handler", paren(Int.layout offset)]
	     | Jump => str "Jump"
	     | Runtime {frameInfo, prim} =>
		  seq [str "Runtime ",
		       record [("frameInfo", FrameInfo.layout frameInfo),
			       ("prim", Prim.layout prim)]]
	 end

      val frameInfoOpt =
	 fn Cont {frameInfo, ...} => SOME frameInfo
	  | Runtime {frameInfo, ...} => SOME frameInfo
	  | _ => NONE
   end

structure Block =
   struct
      datatype t = T of {kind: Kind.t,
			 label: Label.t,
			 live: Operand.t vector,
			 profileInfo: {func: string, label: string},
			 statements: Statement.t vector,
			 transfer: Transfer.t}

      fun clear (T {label, ...}) = Label.clear label

      local
	 fun make g (T r) = g r
      in
	 val kind = make #kind
	 val label = make #label
      end

      fun layout (T {label, kind, live, profileInfo, statements, transfer}) =
	 let
	    open Layout
	 in
	    align [seq [Label.layout label, 
			str " ",
			record [("kind", Kind.layout kind),
				("live", Vector.layout Operand.layout live)],
			str ":"],		   
		   align (Vector.toListMap (statements, Statement.layout)),
		   Transfer.layout transfer]
	 end

      fun layouts (block, output' : Layout.t -> unit) = output' (layout block)
   end

structure Chunk =
   struct
      datatype t = T of {chunkLabel: ChunkLabel.t,
			 blocks: Block.t vector,
			 regMax: Type.t -> int}

      fun layout (T {blocks, ...}) =
	 Layout.align (Vector.toListMap (blocks, Block.layout))

      fun layouts (c as T {blocks, ...}, output' : Layout.t -> unit) =
	 let
	    open Layout
	 in
	    Vector.foreach (blocks, fn block => Block.layouts (block, output'))
	 end
   end

structure Program =
   struct
      datatype t = T of {globals: Type.t -> int,
			 globalsNonRoot: int,
			 intInfs: (Global.t * string) list,
			 strings: (Global.t * string) list,
			 floats: (Global.t * string) list,
			 frameOffsets: int vector vector,
			 maxFrameSize: int,
			 chunks: Chunk.t list,
			 main: {chunkLabel: ChunkLabel.t,
				label: Label.t}}

      fun layout (T {chunks, ...}) =
	 let
	    open Layout
	 in 
	    align (List.map (chunks, Chunk.layout))
	 end

      fun layouts (p as T {chunks, ...}, output': Layout.t -> unit) =
	 let
	    open Layout
	 in
	    List.foreach (chunks, fn chunk => Chunk.layouts (chunk, output'))
	 end

      structure Err =
	 struct
	    datatype t = T of {inner: t option,
			       name: string,
			       obj: Layout.t}

	    fun layout (T {inner, name, obj}) =
	       let
		  open Layout
	       in
		  align [seq [str (concat ["invalid ", name, ": "]),
			      obj],
			 case inner of
			    NONE => empty
			  | SOME e => seq [str "in ", layout e]]
	       end

	    exception E of t

	    fun check (name: string,
		       ok: unit -> bool,
		       layout: unit -> Layout.t): unit =
	       if ok () handle E e => raise E (T {inner = SOME e,
						  name = name,
						  obj = layout ()})
		  then ()
	       else raise E (T {inner = NONE,
				name = name,
				obj = layout ()})

	 end
	    
      fun typeCheck (T {chunks, floats, frameOffsets, globals, globalsNonRoot,
			intInfs, main, maxFrameSize, strings}) =
	 let
	    open Layout
	    fun globals (name, gs, ty) =
	       List.foreach
	       (gs, fn (g, s) =>
		Err.check (concat ["global ", name],
			   fn () => Type.equals (ty, Global.ty g),
			   fn () =>
			   seq [String.layout s, str ": ", Type.layout ty]))
	    val _ = globals ("float", floats, Type.double)
	    val _ = globals ("intInf", intInfs, Type.pointer)
	    val _ = globals ("string", strings, Type.pointer)
	    val {get = labelBlock: Label.t -> Block.t,
		 set = setLabelBlock, ...} =
	       Property.getSetOnce (Label.plist,
				    Property.initRaise ("block", Label.layout))
	    val _ =
	       List.foreach
	       (chunks, fn Chunk.T {blocks, ...} =>
		Vector.foreach
		(blocks, fn b as Block.T {label, ...} =>
		 setLabelBlock (label, b)))
	    fun checkOperand (x: Operand.t): unit =
		let
		   datatype z = datatype Operand.t
		   fun ok () =
		      case x of
			 ArrayOffset {base, offset, ty} =>
			    (checkOperand base
			     ; checkOperand offset
			     ; (Type.equals (Operand.ty base, Type.pointer)
				andalso Type.equals (Operand.ty offset, Type.int)))
		       | CastInt x =>
			    (checkOperand x
			     ; Type.equals (Operand.ty x, Type.pointer))
		       | Char _ => true
		       | Contents {oper, ...} =>
			    (checkOperand oper
			     ; Type.equals (Operand.ty oper, Type.pointer))
		       | Float _ => true
		       | Global _ => true
		       | GlobalPointerNonRoot n =>
			    0 <= n andalso n < globalsNonRoot
		       | Int _ => true
		       | IntInf w => 0wx1 = Word.andb (w, 0wx1)
		       | Label l => (labelBlock l; true)
		       | Offset {base, ...} =>
			    (checkOperand base
			     ; Type.equals (Operand.ty base, Type.pointer))
		       | Pointer n => 0 < Int.rem (n, 4)
		       | Register _ => true
		       | StackOffset {offset, ...} => true
		       | Uint _ => true
		in
		   Err.check ("operand", ok, fn () => Operand.layout x)
		end
	    fun checkOperands v = Vector.foreach (v, checkOperand)
	    fun check' (x, name, isOk, layout) =
	       Err.check (name, fn () => isOk x, fn () => layout x)
	    fun frameInfoOk (FrameInfo.T {frameOffsetsIndex, size}) =
	       0 <= frameOffsetsIndex
	       andalso frameOffsetsIndex <= Vector.length frameOffsets
	       andalso 0 <= size
	       andalso size <= maxFrameSize
	       andalso 0 = Int.rem (size, 4)
	    fun checkFrameInfo i =
	       check' (i, "frame info", frameInfoOk, FrameInfo.layout)
	    (* These checks, and in particular POINTER_BITS and NON_POINTER_BITS
	     * must agree with runtime/gc.h.
	     *)
	    local
	       val POINTER_BITS: int = 15
	       val NON_POINTER_BITS: int = 15
	       fun make (p', np') (p, np) =
		  let
		     val p' = Int.^ (2, p')
		     val np' = Int.^ (2, np')
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
	    fun kindOk (k: Kind.t): bool =
	       let
		  datatype z = datatype Kind.t
		  val _ =
		     case k of
			Cont {args, frameInfo} =>
			   (checkOperands args
			    ; checkFrameInfo frameInfo)
		      | CReturn {dst, ...} => Option.app (dst, checkOperand)
		      | Func {args, ...} => checkOperands args
		      | Handler _ => ()
		      | Jump => ()
		      | Runtime {frameInfo, ...} => checkFrameInfo frameInfo
	       in
		  true
	       end
	    fun statementOk (s: Statement.t): bool =
	       let
		  datatype z = datatype Statement.t
	       in
		  case s of
		     Allocate {dst, numPointers, numWordsNonPointers, size,
			       stores} =>
		        (checkObjectHeader (numPointers, numWordsNonPointers)
			 ; checkOperand dst
			 ; Vector.foreach (stores, fn {offset, value} =>
					   checkOperand value)
			 ; true)
		   | Array {dst, numElts, numPointers, numBytesNonPointers} =>
			(checkOperand dst
			 ; checkOperand numElts
			 ; checkArrayHeader (numPointers, numBytesNonPointers)
			 ; (Type.equals (Operand.ty dst, Type.pointer)
			    andalso Type.equals (Operand.ty numElts, Type.int)))
		   | Assign {dst, prim, args} =>
			(Option.app (dst, checkOperand)
			 ; checkOperands args
			 ; true)
		   | Move {dst, src} =>
			(checkOperand dst
			 ; checkOperand src
			 ; Type.equals (Operand.ty dst, Operand.ty src))
		   | Noop => true
		   | SetExnStackLocal _ => true
		   | SetExnStackSlot _ => true
		   | SetSlotExnStack _ => true
	       end
	    val labelKind = Block.kind o labelBlock
	    fun labelIsJump (l: Label.t): bool =
	       case labelKind l of
		  Kind.Jump => true
		| _ => false
	    fun labelIsRuntime (l: Label.t, p: Prim.t): bool =
	       case labelKind l of
		  Kind.Runtime {prim, ...} => Prim.equals (p, prim)
		| _ => false
	    fun transferOk (t: Transfer.t): bool =
	       let
		  datatype z = datatype Transfer.t
	       in
		  case t of
		     Arith {args, dst, overflow, prim, success} =>
			(checkOperands args
			 ; checkOperand dst
			 ; (Type.equals (Type.int, Operand.ty dst)
			    andalso labelIsJump overflow
			    andalso labelIsJump success))
		      | Bug => true
		      | CCall {args, prim = p, return, returnTy} =>
			   let
			      val _ = checkOperands args
			      val Block.T {kind, ...} = labelBlock return
			   in
			      case labelKind return of
				 Kind.CReturn {dst, prim = p'} =>
				    Prim.equals (p, p')
				    andalso (case (dst, returnTy) of
						(NONE, NONE) => true
					      | (SOME x, SOME ty) =>
						   Type.equals
						   (ty, Operand.ty x)
					      | _ => false)
			       | _ => false
			   end
		      | Call {label, live, return} =>
			   (case labelKind label of
			       Kind.Func _ => true
			     | _ => false)
			   andalso (case return of
				       NONE => true
				     | SOME {handler, return, size} =>
					  (case labelKind return of
					      Kind.Handler _ => true
					    | _ => false)
					  andalso
					  (case labelKind return of
					      Kind.Cont {frameInfo, ...} =>
						 size = FrameInfo.size frameInfo
					    | _ => false))
		      | Goto l => labelIsJump l
		      | LimitCheck {failure, kind, success} =>
			   labelIsRuntime (failure, Prim.gcCollect)
			   andalso labelIsJump success
			   andalso let
				      datatype z = datatype LimitCheck.t
				   in
				      case kind of
					 Array {bytesPerElt, numElts, ...} =>
					    (checkOperand numElts
					     ; (Type.equals (Type.int,
							     Operand.ty numElts)
						andalso bytesPerElt > 0))
				       | Heap {bytes, ...} => bytes >= 0
				       | _ => true
				   end
		      | Raise => true
		      | Return {live} => (checkOperands live; true)
		      | Runtime {args, prim, return} =>
			   (checkOperands args
			    ; (Prim.entersRuntime prim
			       andalso labelIsRuntime (return, prim)))
		      | Switch {cases, default, test} =>
			   (checkOperand test
			    ; (Cases.forall (cases, labelIsJump)
			       andalso Option.forall (default, labelIsJump)))
		      | SwitchIP {int, pointer, test} =>
			   (checkOperand test
			    ; (labelIsJump pointer
			       andalso labelIsJump int))
	       end
	    fun blockOk (Block.T {label, kind, live, profileInfo,
				  statements, transfer}): bool =
	       let
		  val _ = check' (kind, "kind", kindOk, Kind.layout)
		  val _ =
		     Vector.foreach
		     (statements, fn s =>
		      check' (s, "statement", statementOk, Statement.layout))
		  val _ = check' (transfer, "transfer", transferOk,
				  Transfer.layout)
	       in
		  true
	       end
	    val _ =
	       List.foreach
	       (chunks,
		fn Chunk.T {chunkLabel, blocks, regMax} =>
		Vector.foreach
		(blocks, fn b => check' (b, "block", blockOk, Block.layout)))
	 in
	    ()
	 end handle Err.E e => (Layout.outputl (Err.layout e, Out.error)
				; Error.bug "Machine type error")
   end

end
