(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Machine (S: MACHINE_STRUCTS): MACHINE =
struct

open S

structure ChunkLabel = IntUniqueId ()
structure Type = Mtype ()
structure RuntimeOperand = Runtime.GCField
   
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
	 ArrayOffset of {base: t,
			 index: t,
			 ty: Type.t}
       | CastInt of t
       | Char of char
       | Contents of {oper: t,
		      ty: Type.t}
       | Float of string
       | Global of Global.t
       | GlobalPointerNonRoot of int
       | Int of int
       | IntInf of SmallIntInf.t
       | Label of Label.t
       | Offset of {base: t, offset: int, ty: Type.t}
       | Pointer of int
       | Register of Register.t
       | Runtime of RuntimeOperand.t
       | StackOffset of {offset: int, ty: Type.t}
       | Uint of Word.t

      val isLocation =
	 fn ArrayOffset _ => true
	  | Contents _ => true
	  | Global _ => true
	  | GlobalPointerNonRoot _ => true
	  | Offset _ => true
	  | Register _ => true
	  | Runtime _ => true
	  | StackOffset _ => true
	  | _ => false

      val rec toString =
	 fn ArrayOffset {base, index, ty} =>
	 concat ["X", Type.name ty, 
		 "(", toString base, ",", toString index, ")"]
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
	  | Runtime r => RuntimeOperand.toString r
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
	| Runtime z => (case RuntimeOperand.ty z of
			   RuntimeOperand.Int => Type.int
			 | RuntimeOperand.Word => Type.word)
	| StackOffset {ty, ...} => ty
	| Uint _ => Type.uint
	 
      val rec equals =
	 fn (ArrayOffset {base = b, index = i, ...},
	     ArrayOffset {base = b', index = i', ...}) =>
	        equals (b, b') andalso equals (i, i') 
	   | (CastInt z, CastInt z') => equals (z, z')
	   | (Char c, Char c') => c = c'
	   | (Contents {oper = z, ...}, Contents {oper = z', ...}) =>
		equals (z, z')
	   | (Float f, Float f') => f = f'
	   | (Int n, Int n') => n = n'
	   | (IntInf w, IntInf w') => Word.equals (w, w')
	   | (Offset {base = b, offset = i, ...},
	      Offset {base = b', offset = i', ...}) =>
	        equals (b, b') andalso i = i' 
	   | (Pointer n, Pointer n') => n = n'
	   | (Register r, Register r') => Register.equals (r, r')
	   | (StackOffset {offset = n, ...}, StackOffset {offset = n', ...}) =>
		n = n'
	   | (Uint w, Uint w') => w = w'
	   | _ => false

      fun interfere {write: t, read: t}: bool =
	 let fun inter read = interfere {write = write, read = read}
	 in case (read, write) 
	    of (ArrayOffset {base, index, ...}, _) => 
	       inter base orelse inter index
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
	 Array of {dst: Operand.t,
		   header: word,
		   numBytes: Operand.t,
		   numElts: Operand.t}
       | Move of {dst: Operand.t,
		  src: Operand.t}
       | Noop
       | Object of {dst: Operand.t,
		    numPointers: int,
		    numWordsNonPointers: int,
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
	    fn Array {dst, header, numBytes, numElts} =>
	    seq [Operand.layout dst,
		 str " = Array",
		 record [("header", Word.layout header),
			 ("numBytes", Operand.layout numBytes),
			 ("numElts", Operand.layout numElts)]]
	     | Move {dst, src} =>
		  seq [Operand.layout dst, str " = ", Operand.layout src]
	     | Noop => str "Noop"
	     | Object {dst, numPointers, numWordsNonPointers, stores} =>
		  seq [Operand.layout dst, str " = Object ",
		       tuple [Int.layout numWordsNonPointers,
			      Int.layout numPointers],
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
	    Array {dst, numBytes, numElts, ...} =>
	       f (dst, f (numBytes, f (numElts, ac)))
	  | Move {dst, src} => f (dst, f (src, ac))
	  | Object {dst, stores, ...} =>
	       Vector.fold
	       (stores, f (dst, ac), fn ({value, ...}, ac) => f (value, ac))
	  | PrimApp {args, dst, ...} =>
	       Vector.fold (args, Option.fold (dst, ac, f), f)
	  | _ => ac
   end

structure Cases = MachineCases (structure Label = Label)

structure Transfer =
   struct
      datatype t =
	 Arith of {prim: Prim.t,
		   args: Operand.t vector,
		   dst: Operand.t,
		   overflow: Label.t,
		   success: Label.t,
		   ty: Type.t}
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
	       Arith {prim, args, dst, overflow, success, ...} =>
		  seq [str "Arith ",
		       record [("prim", Prim.layout prim),
			       ("args", Vector.layout Operand.layout args),
			       ("dst", Operand.layout dst),
			       ("overflow", Label.layout overflow),
			       ("success", Label.layout success)]]
	     | Bug => str "Bug"
	     | CCall {args, prim, return, returnTy} =>
		  seq [str "CCall ",
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

      fun foldOperands (t, ac, f) =
	 case t of
	    Arith {args, dst, ...} => Vector.fold (args, f (dst, ac), f)
	  | CCall {args, ...} => Vector.fold (args, ac, f)
	  | Runtime {args, ...} => Vector.fold (args, ac, f)
	  | Switch {test, ...} => f (test, ac)
	  | SwitchIP {test, ...} => f (test, ac)
	  | _ => ac
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
   end

structure Chunk =
   struct
      datatype t = T of {chunkLabel: ChunkLabel.t,
			 blocks: Block.t vector,
			 regMax: Type.t -> int}

      fun layout (T {blocks, regMax, ...}) =
	 let
	    open Layout
	 in
	    align
	    [align (List.map (Type.all, fn t =>
			      seq [str "regMax ", Type.layout t,
				   str " = ", Int.layout (regMax t)])),
	     align (Vector.toListMap (blocks, Block.layout))]
	 end

      fun layouts (c as T {blocks, regMax, ...}, output' : Layout.t -> unit) =
	 let
	    open Layout
	    val output = output'
	 in
	    List.foreach (Type.all, fn t =>
			  output (seq [str "regMax ", Type.layout t,
				       str " = ", Int.layout (regMax t)]))
	    ; Vector.foreach (blocks, fn block => Block.layouts (block, output))
	 end
   end

structure Program =
   struct
      datatype t = T of {chunks: Chunk.t list,
			 floats: (Global.t * string) list,
			 frameOffsets: int vector vector,
			 globals: Type.t -> int,
			 globalsNonRoot: int,
			 intInfs: (Global.t * string) list,
			 main: {chunkLabel: ChunkLabel.t,
				label: Label.t},
			 maxFrameSize: int,
			 strings: (Global.t * string) list}

      fun layouts (p as T {chunks, frameOffsets, globals, globalsNonRoot,
			   main = {label, ...}, maxFrameSize, ...},
		   output': Layout.t -> unit) =
	 let
	    open Layout
	    val output = output'
	 in
	    output (record
		    [("globals",
		      List.layout (fn t =>
				   seq [Type.layout t, str " ",
					Int.layout (globals t)])
		      Type.all),
		     ("globalsNonRoot", Int.layout globalsNonRoot),
		     ("main", Label.layout label),
		     ("maxFrameSize", Int.layout maxFrameSize),
		     ("frameOffsets",
		      Vector.layout (Vector.layout Int.layout) frameOffsets)])
            ; List.foreach (chunks, fn chunk => Chunk.layouts (chunk, output))
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
	    val _ =
	       List.foreach
	       (chunks,
		fn Chunk.T {chunkLabel, blocks, regMax} =>
		let
		   fun checkOperand (x: Operand.t): unit =
		      let
			 datatype z = datatype Operand.t
			 fun ok () =
			    case x of
			       ArrayOffset {base, index, ty} =>
				  (checkOperand base
				   ; checkOperand index
				   ; (Type.equals (Operand.ty base, Type.pointer)
				      andalso Type.equals (Operand.ty index,
							   Type.int)))
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
			     | Pointer n => 0 < Int.rem (n, Runtime.wordSize)
			     | Register (Register.T {index, ty}) =>
				  0 <= index andalso index < regMax ty
			     | Runtime _ => true
			     | StackOffset {offset, ty, ...} =>
				  0 <= offset
				  andalso offset + Type.size ty <= maxFrameSize
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
			     | CReturn {dst, ...} =>
				  Option.app (dst, checkOperand)
			     | Func {args, ...} => checkOperands args
			     | Handler _ => ()
			     | Jump => ()
			     | Runtime {frameInfo, ...} =>
				  checkFrameInfo frameInfo
		      in
			 true
		      end
		   fun statementOk (s: Statement.t): bool =
		      let
			 datatype z = datatype Statement.t
		      in
			 case s of
			    Array {dst, header, numBytes, numElts} =>
			       (checkOperand dst
				; checkOperand numBytes
				; checkOperand numElts
				; (Type.equals (Operand.ty dst, Type.pointer)
				   andalso Type.equals (Operand.ty numBytes,
							Type.word)
				   andalso Type.equals (Operand.ty numElts,
							Type.int)))
			  | Move {dst, src} =>
			       (checkOperand dst
				; checkOperand src
				; (Type.equals (Operand.ty dst, Operand.ty src)
				   andalso Operand.isLocation dst))
			  | Noop => true
			  | Object {dst, numPointers, numWordsNonPointers,
				    stores} =>
			       (checkOperand dst
				; Vector.foreach (stores, fn {offset, value} =>
						  checkOperand value)
				; (Runtime.isValidObjectHeader
				   {numPointers = numPointers,
				    numWordsNonPointers = numWordsNonPointers}))
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
		   fun labelIsRuntime (l: Label.t, p: Prim.t): bool =
		      case labelKind l of
			 Kind.Runtime {prim, ...} => Prim.equals (p, prim)
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
			  | Runtime {args, prim, return} =>
			       (checkOperands args
				; (Prim.entersRuntime prim
				   andalso labelIsRuntime (return, prim)))
			  | Switch {cases, default, test} =>
			       (checkOperand test
				; (Cases.forall (cases, labelIsJump)
				   andalso Option.forall (default, labelIsJump)
				   andalso
				   (Type.equals
				    (Operand.ty test,
				     case cases of
					Cases.Char _ => Type.char
				      | Cases.Int _ => Type.int
				      | Cases.Word _ => Type.uint))))
			  | SwitchIP {int, pointer, test} =>
			       (checkOperand test
				; (labelIsJump pointer
				   andalso labelIsJump int))
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
