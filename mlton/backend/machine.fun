(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Machine (S: MACHINE_STRUCTS): MACHINE =
struct

open S

structure Runtime = Runtime ()
local
   open Runtime
in
   structure CFunction = CFunction
   structure GCField = GCField
end

structure Atoms = MachineAtoms (structure Label = Label
				structure Prim = Prim
				structure Runtime = Runtime)
open Atoms

structure ChunkLabel = IntUniqueId ()
   
structure SmallIntInf =
   struct
      type t = word
   end

structure Register =
   struct
      datatype t = T of {index: int option ref,
			 plist: PropertyList.t,
			 ty: Type.t}

      local
	 fun make f (T r) = f r
      in
	 val plist = make #plist
	 val ty = make #ty
      end

      fun layout (T {index, ty, ...}) =
	 let
	    open Layout
	 in
	    seq [str "reg ",
		 record [("index", Option.layout Int.layout (!index)),
			 ("ty", Type.layout ty)]]
	 end

      val toString = Layout.toString o layout

      fun index (r as T {index, ...}) =
	 case !index of
	    NONE =>
	       Error.bug (concat ["register ", toString r, " missing index"])
	  | SOME i => i

      fun setIndex (r as T {index, ...}, i) =
	 case !index of
	    NONE => index := SOME i
	  | SOME _ =>
	       Error.bug (concat ["register ", toString r, " index already set"])

      local
	 val c = Counter.new 0
      in
	 fun new ty = T {index = ref NONE,
			 plist = PropertyList.new (),
			 ty = ty}
      end

      fun equals (T {plist = p, ...}, T {plist = p', ...}) =
	 PropertyList.equals (p, p')
   end

structure Global =
   struct
      datatype t = T of {index: int,
			 isRoot: bool,
			 plist: PropertyList.t,
			 ty: Type.t}

      fun layout (T {index, ty, ...}) =
	 let
	    open Layout
	 in
	    seq [str "glob ",
		 record [("index", Int.layout index),
			 ("ty", Type.layout ty)]]
	 end
      
      val toString = Layout.toString o layout

      local
	 fun make f (T r) = f r
      in
	 val index = make #index
	 val isRoot = make #isRoot
	 val plist = make #plist
	 val ty = make #ty
      end

      val nonRootCounter = Counter.new 0
      fun numberOfNonRoot () = Counter.value nonRootCounter

      val memo = Runtime.Type.memo (fn _ => Counter.new 0)
      fun numberOfType t = Counter.value (memo t)
	 
      fun new {isRoot, ty} =
	 let
	    val isRoot = isRoot orelse not (Type.isPointer ty)
	    val counter =
	       if isRoot
		  then memo (Type.toRuntime ty)
	       else nonRootCounter
	    val g = T {index = Counter.next counter,
		       isRoot = isRoot,
		       plist = PropertyList.new (),
		       ty = ty}
	 in
	    g
	 end

      fun equals (T {plist = p, ...}, T {plist = p', ...}) =
	 PropertyList.equals (p, p')
   end

structure Operand =
   struct
      datatype t =
	 ArrayOffset of {base: t,
			 index: t,
			 ty: Type.t}
       | Cast of t * Type.t
       | Char of char
       | Contents of {oper: t,
		      ty: Type.t}
       | File
       | GCState
       | Global of Global.t
       | Int of int
       | SmallIntInf of SmallIntInf.t
       | Label of Label.t
       | Line
       | Offset of {base: t, offset: int, ty: Type.t}
       | Register of Register.t
       | Real of string
       | Runtime of GCField.t
       | StackOffset of {offset: int, ty: Type.t}
       | Word of Word.t
    
      val rec isLocation =
	 fn ArrayOffset _ => true
	  | Cast (z, _) => isLocation z
	  | Contents _ => true
	  | Global _ => true
	  | Offset _ => true
	  | Register _ => true
	  | Runtime z => true
	  | StackOffset _ => true
	  | _ => false

      fun layout (z: t): Layout.t =
	 let
	    open Layout 
	    fun constrain (ty: Type.t): Layout.t =
	       if !Control.showTypes
		  then seq [str ": ", Type.layout ty]
	       else empty
	 in
	    case z of
	       ArrayOffset {base, index, ty} =>
		  seq [str (concat ["X", Type.name ty, " "]),
		       tuple [layout base, layout index],
		       constrain ty]
	     | Cast (z, ty) =>
		  seq [str "Cast ", tuple [layout z, Type.layout ty]]
	     | Char c => str (Char.escapeC c)
	     | Contents {oper, ty} =>
		  seq [str (concat ["C", Type.name ty, " "]),
		       paren (layout oper)]
	     | File => str "<File>"
	     | GCState => str "<GCState>"
	     | Global g => Global.layout g
	     | Int i => Int.layout i
	     | Label l => Label.layout l
	     | Line => str "<Line>"
	     | Offset {base, offset, ty} =>
		  seq [str (concat ["O", Type.name ty, " "]),
		       tuple [layout base, Int.layout offset],
		       constrain ty]
	     | Real s => str s
	     | Register r => Register.layout r
	     | Runtime r => GCField.layout r
	     | SmallIntInf w => seq [str "SmallIntInf ", paren (Word.layout w)]
	     | StackOffset {offset, ty} =>
		  seq [str (concat ["S", Type.name ty, " "]),
		       paren (Int.layout offset)]
	     | Word w => seq [str "0x", Word.layout w]
	 end

    val toString = Layout.toString o layout

    val ty =
       fn ArrayOffset {ty, ...} => ty
	| Cast (_, ty) => ty
	| Char _ => Type.char
	| Contents {ty, ...} => ty
	| File => Type.cpointer
	| GCState => Type.cpointer
	| Global g => Global.ty g
	| Int _ => Type.int
	| Label _ => Type.label
	| Line => Type.int
	| Offset {ty, ...} => ty
	| Real _ => Type.real
	| Register r => Register.ty r
	| Runtime z => Type.fromRuntime (GCField.ty z)
	| SmallIntInf _ => Type.intInf
	| StackOffset {ty, ...} => ty
	| Word _ => Type.word
	 
      val rec equals =
	 fn (ArrayOffset {base = b, index = i, ...},
	     ArrayOffset {base = b', index = i', ...}) =>
	        equals (b, b') andalso equals (i, i') 
	   | (Cast (z, t), Cast (z', t')) =>
		Type.equals (t, t') andalso equals (z, z')
	   | (Char c, Char c') => c = c'
	   | (Contents {oper = z, ...}, Contents {oper = z', ...}) =>
		equals (z, z')
	   | (File, File) => true
	   | (GCState, GCState) => true
	   | (Int i, Int i') => i = i'
	   | (Line, Line) => true
	   | (Offset {base = b, offset = i, ...},
	      Offset {base = b', offset = i', ...}) =>
	        equals (b, b') andalso i = i' 
	   | (Real s, Real s') => s = s'
	   | (Register r, Register r') => Register.equals (r, r')
	   | (SmallIntInf w, SmallIntInf w') => Word.equals (w, w')
	   | (StackOffset {offset = n, ...}, StackOffset {offset = n', ...}) =>
		n = n'
	   | (Word w, Word w') => w = w'
	   | _ => false

      fun interfere {write: t, read: t}: bool =
	 let fun inter read = interfere {write = write, read = read}
	 in case (read, write) 
	    of (ArrayOffset {base, index, ...}, _) => 
	       inter base orelse inter index
	  | (Contents {oper, ...}, _) => inter oper
	  | (Global g, Global g') => Global.equals (g, g')
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

structure Switch = Switch (open Atoms
			   structure Use = Operand)

structure Statement =
   struct
      datatype t =
	 Move of {dst: Operand.t,
		  src: Operand.t}
       | Noop
       | Object of {dst: Operand.t,
		    header: word,
		    size: int,
		    stores: {offset: int,
			     value: Operand.t} vector}
       | PrimApp of {args: Operand.t vector,
		     dst: Operand.t option,
		     prim: Prim.t}
       | SetExnStackLocal of {offset: int}
       | SetExnStackSlot of {offset: int}
       | SetSlotExnStack of {offset: int}

      val layout =
	 let
	    open Layout
	 in
	    fn Move {dst, src} =>
		  seq [Operand.layout dst, str " = ", Operand.layout src]
	     | Noop => str "Noop"
	     | Object {dst, header, size, stores} =>
		  seq [Operand.layout dst, str " = Object ",
		       record [("header", Word.layout header),
			       ("size", Int.layout size)],
		       str " ",
		       Vector.layout (fn {offset, value} =>
				      record [("offset", Int.layout offset),
					      ("value", Operand.layout value)])
		       stores]
	     | PrimApp {args, dst, prim, ...} =>
		  seq [case dst of
			  NONE => empty
			| SOME z => seq [Operand.layout z, str " = "],
		       Prim.layout prim, str " ",
		       Vector.layout Operand.layout args]
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
	 
      fun moves {srcs, dsts} =
	 Vector.fromListRev
	 (Vector.fold2 (srcs, dsts, [], fn (src, dst, ac)  =>
			move {src = src, dst = dst} :: ac))

      fun foldOperands (s, ac, f) =
	 case s of
	    Move {dst, src} => f (dst, f (src, ac))
	  | Object {dst, stores, ...} =>
	       Vector.fold
	       (stores, f (dst, ac), fn ({value, ...}, ac) => f (value, ac))
	  | PrimApp {args, dst, ...} =>
	       Vector.fold (args, Option.fold (dst, ac, f), f)
	  | _ => ac

      fun foldDefs (s, a, f) =
	 case s of
	    Move {dst, ...} => f (dst, a)
	  | Object {dst, ...} => f (dst, a)
	  | PrimApp {dst, ...} => (case dst of
				      NONE => a
				    | SOME z => f (z, a))
	  | _ => a
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

structure Transfer =
   struct
      datatype t =
	 Arith of {args: Operand.t vector,
		   dst: Operand.t,
		   overflow: Label.t,
		   prim: Prim.t,
		   success: Label.t,
		   ty: Type.t}
       | CCall of {args: Operand.t vector,
		   frameInfo: FrameInfo.t option,
		   func: CFunction.t,
		   return: Label.t option}
       | Call of {label: Label.t,
		  live: Operand.t vector,
		  return: {return: Label.t,
			   handler: Label.t option,
			   size: int} option}
       | Goto of Label.t
       | Raise
       | Return of {live: Operand.t vector}
       | Switch of Switch.t

      fun layout t =
	 let
	    open Layout
	 in
	    case t of
	       Arith {prim, args, dst, overflow, success, ...} =>
		  seq [str "Arith ",
		       record [("prim", Prim.layout prim),
			       ("args", Vector.layout Operand.layout args),
			       ("dst", Operand.layout dst),
			       ("overflow", Label.layout overflow),
			       ("success", Label.layout success)]]
	     | CCall {args, frameInfo, func, return} =>
		  seq [str "CCall ",
		       record
		       [("args", Vector.layout Operand.layout args),
			("frameInfo", Option.layout FrameInfo.layout frameInfo),
			("func", CFunction.layout func),
			("return", Option.layout Label.layout return)]]
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
	     | Raise => str "Raise"
	     | Return {live} => 
		  seq [str "Return ",
		       record [("live", Vector.layout Operand.layout live)]]
	     | Switch s => Switch.layout s
	 end

       fun foldOperands (t, ac, f) =
 	 case t of
 	    Arith {args, dst, ...} => Vector.fold (args, f (dst, ac), f)
 	  | CCall {args, ...} => Vector.fold (args, ac, f)
 	  | Switch s =>
	       Switch.foldLabelUse
	       (s, ac, {label = fn (_, a) => a,
			use = f})
 	  | _ => ac

       fun foldDefs (t, a, f) =
	 case t of
	    Arith {dst, ...} => f (dst, a)
	  | _ => a
   end

structure Kind =
   struct
      datatype t =
	 Cont of {args: Operand.t vector,
		  frameInfo: FrameInfo.t}
       | CReturn of {dst: Operand.t option,
		     frameInfo: FrameInfo.t option,
		     func: CFunction.t}
       | Func of {args: Operand.t vector}
       | Handler of {offset: int}
       | Jump

      fun layout k =
	 let
	    open Layout
	 in
	    case k of
	       Cont {args, frameInfo} =>
		  seq [str "Cont ",
		       record [("args", Vector.layout Operand.layout args),
			       ("frameInfo", FrameInfo.layout frameInfo)]]
	     | CReturn {dst, frameInfo, func} =>
		  seq [str "CReturn ",
		       record
		       [("dst", Option.layout Operand.layout dst),
			("frameInfo", Option.layout FrameInfo.layout frameInfo),
			("func", CFunction.layout func)]]
	     | Func {args} =>
		  seq [str "Func ",
		       record [("args", Vector.layout Operand.layout args)]]
	     | Handler {offset} =>
		  seq [str "Handler", paren(Int.layout offset)]
	     | Jump => str "Jump"
	 end

      val frameInfoOpt =
	 fn Cont {frameInfo, ...} => SOME frameInfo
	  | CReturn {frameInfo, ...} => frameInfo
	  | _ => NONE
   end

structure Block =
   struct
      datatype t = T of {kind: Kind.t,
			 label: Label.t,
			 live: Operand.t vector,
			 profileInfo: {ssa: {func: string, label: string},
				       rssa: {func: string, label: string}},
			 statements: Statement.t vector,
			 transfer: Transfer.t}

      fun clear (T {label, ...}) = Label.clear label

      local
	 fun make g (T r) = g r
      in
	 val kind = make #kind
	 val label = make #label
      end

      fun layout (T {kind, label, live, profileInfo, statements, transfer}) =
	 let
	    open Layout
	 in
	    align [seq [Label.layout label, 
			str " ",
			record [("kind", Kind.layout kind),
				("live", Vector.layout Operand.layout live)],
			str ":"],		   
		   indent (align
			   [align (Vector.toListMap (statements, Statement.layout)),
			    Transfer.layout transfer],
			   4)]
	 end

      fun layouts (block, output' : Layout.t -> unit) = output' (layout block)

      fun foldDefs (T {kind, statements, transfer, ...}, a, f) =
	 let
	    val a =
	       case kind of
		  Kind.CReturn {dst, ...} =>
		     (case dst of
			 NONE => a
		       | SOME z => f (z, a))
		| _ => a
	    val a =
	       Vector.fold (statements, a, fn (s, a) =>
			    Statement.foldDefs (s, a, f))
	    val a = Transfer.foldDefs (transfer, a, f)
	 in
	    a
	 end
   end

structure Chunk =
   struct
      datatype t = T of {chunkLabel: ChunkLabel.t,
			 blocks: Block.t vector,
			 regs: Register.t vector}

      fun layout (T {blocks, ...}) =
	 let
	    open Layout
	 in
	    align (Vector.toListMap (blocks, Block.layout))
	 end

      fun layouts (c as T {blocks, ...}, output : Layout.t -> unit) =
	 Vector.foreach (blocks, fn block => Block.layouts (block, output))
   end

structure Program =
   struct
      datatype t = T of {chunks: Chunk.t list,
			 frameOffsets: int vector vector,
			 handlesSignals: bool,
			 intInfs: (Global.t * string) list,
			 main: {chunkLabel: ChunkLabel.t,
				label: Label.t},
			 maxFrameSize: int,
			 objectTypes: ObjectType.t vector,
			 profileAllocLabels: string vector,
			 reals: (Global.t * string) list,
			 strings: (Global.t * string) list}

      fun layouts (p as T {chunks, frameOffsets, handlesSignals,
			   main = {label, ...},
			   maxFrameSize, objectTypes,
			   profileAllocLabels, ...},
		   output': Layout.t -> unit) =
	 let
	    open Layout
	    val output = output'
	 in
	    output (record
		    [("handlesSignals", Bool.layout handlesSignals),
		     ("main", Label.layout label),
		     ("maxFrameSize", Int.layout maxFrameSize),
		     ("pointerTypes",
		      Vector.layout ObjectType.layout objectTypes),
		     ("profileAllocLabels",
		      Vector.layout String.layout profileAllocLabels),
		     ("frameOffsets",
		      Vector.layout (Vector.layout Int.layout) frameOffsets)])
            ; List.foreach (chunks, fn chunk => Chunk.layouts (chunk, output))
	 end
	    
      fun typeCheck (T {chunks, frameOffsets, intInfs, main,
			maxFrameSize, objectTypes, reals, strings, ...}) =
	 let
	    val _ =
	       Vector.foreach
	       (objectTypes, fn ty =>
		Err.check ("objectType",
			   fn () => ObjectType.isOk ty,
			   fn () => ObjectType.layout ty))
	    fun tyconTy (pt: PointerTycon.t): ObjectType.t =
	       Vector.sub (objectTypes, PointerTycon.index pt)
	    open Layout
	    fun globals (name, gs, ty) =
	       List.foreach
	       (gs, fn (g, s) =>
		Err.check (concat ["global ", name],
			   fn () => Type.equals (ty, Global.ty g),
			   fn () =>
			   seq [String.layout s, str ": ", Type.layout ty]))
	    val _ = globals ("real", reals, Type.real)
	    val _ = globals ("intInf", intInfs, Type.intInf)
	    val _ = globals ("string", strings, Type.string)
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
	    val _ =
	       List.foreach
	       (chunks,
		fn Chunk.T {blocks, ...} =>
		let
		   fun checkOperand (x: Operand.t): unit =
		      let
			 datatype z = datatype Operand.t
			 fun ok () =
			    case x of
			       ArrayOffset z => arrayOffsetIsOk z
			     | Cast (z, t) =>
				  (checkOperand z
				   ; (castIsOk
				      {from = Operand.ty z,
				       fromInt = (case z of
						     Int i => SOME i
						   | _ => NONE),
				       to = t,
				       tyconTy = tyconTy}))
			     | Char _ => true
			     | Contents {oper, ...} =>
				  (checkOperand oper
				   ; Type.equals (Operand.ty oper,
						  Type.cpointer))
			     | File => true
			     | GCState => true
			     | Global _ => true
			     | Int _ => true
			     | Label l => (labelBlock l; true)
			     | Line => true
			     | Offset z => offsetIsOk z
			     | Real _ => true
			     | Register _ => true
			     | Runtime _ => true
			     | SmallIntInf w => 0wx1 = Word.andb (w, 0wx1)
			     | StackOffset {offset, ty, ...} =>
				  offset + Type.size ty <= maxFrameSize
			     | Word _ => true
		      in
			 Err.check ("operand", ok, fn () => Operand.layout x)
		      end
		   and arrayOffsetIsOk {base, index, ty} =
		      let
			 val _ = checkOperand base
			 val _ = checkOperand index
		      in
			 Type.equals (Operand.ty index, Type.int)
			 andalso
			 case Operand.ty base of
			    Type.CPointer => true (* needed for card marking *)
			  | Type.EnumPointers {enum, pointers} =>
			       0 = Vector.length enum
			       andalso
			       Vector.forall
			       (pointers, fn p =>
				case tyconTy p of
				   ObjectType.Array
				   (MemChunk.T {components, ...}) =>
				      1 = Vector.length components
				      andalso
				      let
					 val {offset, ty = ty', ...} =
					    Vector.sub (components, 0)
				      in
					 offset = 0
					 andalso Type.equals (ty, ty')
				      end
				 | _ => false)
			  | _ => false
		      end
		   and offsetIsOk {base, offset, ty} =
		      let
			 val _ = checkOperand base
			 fun memChunkIsOk (MemChunk.T {components, ...}) =
			    case (Vector.peek
				  (components, fn {offset = offset', ...} =>
				   offset = offset')) of
			       NONE => false
			     | SOME {ty = ty', ...} => Type.equals (ty, ty')
				  
		      in
			 case Operand.ty base of
			    Type.EnumPointers {enum, pointers} =>
			       0 = Vector.length enum
			       andalso
			       ((* Vector_fromArray header update. *)
				(offset = Runtime.headerOffset
				 andalso Type.equals (ty, Type.word))
				orelse
				Vector.forall
				(pointers, fn p =>
				 case tyconTy p of
				    ObjectType.Normal m => memChunkIsOk m
				  | _ => false))
			  | Type.MemChunk m => memChunkIsOk m
			  | _ => false
		      end
		   fun checkOperands v = Vector.foreach (v, checkOperand)
		   fun check' (x, name, isOk, layout) =
		      Err.check (name, fn () => isOk x, fn () => layout x)
		   fun frameInfoOk (FrameInfo.T {frameOffsetsIndex, size}) =
		      0 <= frameOffsetsIndex
		      andalso frameOffsetsIndex <= Vector.length frameOffsets
		      andalso 0 <= size
		      andalso size <= maxFrameSize
		      andalso size <= Runtime.maxFrameSize
		      andalso 0 = Int.rem (size, 4)
		   fun checkFrameInfo i =
		      check' (i, "frame info", frameInfoOk, FrameInfo.layout)
		   fun kindOk (k: Kind.t): bool =
		      let
			 datatype z = datatype Kind.t
			 val _ =
			    case k of
			       Cont {args, frameInfo} =>
				  (checkOperands args
				   ; checkFrameInfo frameInfo)
			     | CReturn {dst, frameInfo, ...} =>
				  (Option.app (dst, checkOperand)
				   ; Option.app (frameInfo, checkFrameInfo))
			     | Func {args, ...} => checkOperands args
			     | Handler _ => ()
			     | Jump => ()
		      in
			 true
		      end
		   fun statementOk (s: Statement.t): bool =
		      let
			 datatype z = datatype Statement.t
		      in
			 case s of
			    Move {dst, src} =>
			       (checkOperand dst
				; checkOperand src
				; (Type.equals (Operand.ty dst, Operand.ty src)
				   andalso Operand.isLocation dst))
			  | Noop => true
			  | Object {dst, header, size, stores} =>
			       (checkOperand dst
				; (case Vector.sub (objectTypes,
						    Runtime.headerToTypeIndex
						    header) of
				      ObjectType.Normal mc =>
					 MemChunk.isValidInit
					 (mc, 
					  Vector.map
					  (stores, fn {offset, value} =>
					   {offset = offset,
					    ty = Operand.ty value}))
				    | _ => false) handle Subscript => false)
			  | PrimApp {args, dst, prim} =>
			       (Option.app (dst, checkOperand)
				; checkOperands args
				; true)
			  | SetExnStackLocal _ => true
			  | SetExnStackSlot _ => true
			  | SetSlotExnStack _ => true
		      end
		   val labelKind = Block.kind o labelBlock
		   fun labelIsJump (l: Label.t): bool =
		      case labelKind l of
			 Kind.Jump => true
		       | _ => false
		   fun transferOk (t: Transfer.t): bool =
		      let
			 datatype z = datatype Transfer.t
		      in
			 case t of
			    Arith {args, dst, overflow, prim, success, ty} =>
			       (checkOperands args
				; checkOperand dst
				; (Type.equals (ty, Operand.ty dst)
				   andalso labelIsJump overflow
				   andalso labelIsJump success))
			  | CCall {args, frameInfo, func, return} =>
			       let
				  val _ = checkOperands args
				  val _ = Option.app (frameInfo, checkFrameInfo)
			       in
				  case return of
				     NONE => true
				   | SOME l =>
					let 
					   val Block.T {kind, ...} = labelBlock l
					in
					   case labelKind l of
					      Kind.CReturn
					      {dst, func = f, ...} => 
						 CFunction.equals (func, f)
						 andalso
						 (case (dst, CFunction.returnTy f) of
						     (NONE, _) => true
						   | (SOME x, SOME ty) =>
							Runtime.Type.equals
							(ty,
							 Type.toRuntime
							 (Operand.ty x))
						   | _ => false)
					    | _ => false
					end
			       end
			  | Call {label, live, return} =>
			       (case labelKind label of
				   Kind.Func _ => true
				 | _ => false)
			       andalso
			       (case return of
				   NONE => true
				 | SOME {handler, return, size} =>
				      (case handler of
					  NONE => true
					| SOME h =>
					     (case labelKind h of
						 Kind.Handler _ => true
					       | _ => false))
					  andalso
					  (case labelKind return of
					      Kind.Cont {frameInfo, ...} =>
						 size = FrameInfo.size frameInfo
					    | _ => false))
			  | Goto l => labelIsJump l
			  | Raise => true
			  | Return {live} => (checkOperands live; true)
			  | Switch s =>
			       Switch.isOk (s, {labelIsOk = labelIsJump})
		      end
		   fun blockOk (Block.T {kind, label, live, profileInfo,
					 statements, transfer}): bool =
		      let
			 val _ = check' (kind, "kind", kindOk, Kind.layout)
			 val _ =
			    Vector.foreach
			    (statements, fn s =>
			     check' (s, "statement", statementOk,
				     Statement.layout))
			 val _ = check' (transfer, "transfer", transferOk,
					 Transfer.layout)
		      in
			 true
		      end
		in
		   Vector.foreach
		   (blocks, fn b => check' (b, "block", blockOk, Block.layout))
		end)
	 in
	    ()
	 end handle Err.E e => (Layout.outputl (Err.layout e, Out.error)
				; Error.bug "Machine type error")
   end

end
