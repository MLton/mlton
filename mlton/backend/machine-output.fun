(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor MachineOutput (S: MACHINE_OUTPUT_STRUCTS): MACHINE_OUTPUT =
struct

open S

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
   end

structure Cases = MachineCases (structure Label = Label)
   
structure LimitCheck =
   struct
      datatype t =
	 Array of {bytesPerElt: int,
		   extraBytes: int, (* for subsequent allocation *)
		   numElts: Operand.t}
       | Heap of {bytes: int,
		  stackToo: bool}
       | Signal
       | Stack

      fun layout (l: t): Layout.t =
	 let
	    open Layout
	 in
	    case l of
	       Array {bytesPerElt, extraBytes, numElts} =>
		  seq [str "Array ",
		       record [("bytesPerElt", Int.layout bytesPerElt),
			       ("extraBytes", Int.layout extraBytes),
			       ("numElts", Operand.layout numElts)]]
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
       | FarJump of {chunkLabel: ChunkLabel.t,
		     label: Label.t,
		     live: Operand.t list,
		     return: {return: Label.t,
			      handler: Label.t option,
			      size: int} option}
       | LimitCheck of {frameSize: int,
			kind: LimitCheck.t,
			live: Operand.t list,
			return: Label.t}
       | NearJump of {label: Label.t,
		      return: {return: Label.t,
			       handler: Label.t option,
			       size: int} option}
       | Raise
       | Return of {live: Operand.t list}
       | Runtime of {args: Operand.t vector,
		     frameSize: int,
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
	     | FarJump {label, live, return, ...} => 
		  seq [str "FarJump ", 
		       record [("label", Label.layout label),
			       ("live", List.layout Operand.layout live),
			       ("return", Option.layout 
				(fn {return, handler, size}
				 => record [("return", Label.layout return),
					    ("handler", Option.layout Label.layout handler),
					    ("size", Int.layout size)])
				return)]]
	     | LimitCheck {frameSize, kind, live, return} =>
		  seq [str "LimitCheck",
		       record [("frameSize", Int.layout frameSize),
			       ("kind", LimitCheck.layout kind),
			       ("live", List.layout Operand.layout live),
			       ("return", Label.layout return)]]
	     | NearJump {label, return} => 
		  seq [str "NearJump ", 
		       record [("label", Label.layout label),
			       ("return", Option.layout 
				(fn {return, handler, size}
				 => record [("return", Label.layout return),
					    ("handler", Option.layout Label.layout handler),
					    ("size", Int.layout size)])
				return)]]
	     | Raise => str "Raise"
	     | Return {live} => 
		  seq [str "Return ",
		       record [("live", List.layout Operand.layout live)]]
	     | Runtime {args, prim, return} =>
		  seq [str "Runtime ",
		       record [("args", Vector.layout Operand.layout args),
			       ("frameSize", Int.layout frameSize),
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
   end

structure Block =
   struct
      structure Kind =
	struct
	  datatype t =
	     Cont of {args: Operand.t list,
		      size: int}
	   | CReturn of {arg: Operand.t,
			 ty: Type.t} option
	   | Func of {args: Operand.t list}
	   | Handler of {offset: int}
	   | Jump

	  fun layout k =
	     let
		open Layout
	     in
		case k of
		   Cont {args, size} =>
		      seq [str "Cont", paren (Int.layout size), str " ",
			   record [("args", List.layout Operand.layout args)]]
		 | CReturn opt =>
		      seq [str "CReturn ",
			   Option.layout
			   (fn {arg, ty} =>
			    record [("arg", Operand.layout arg),
				    ("ty", Type.layout ty)])
			   opt]
		 | Func {args} =>
		      seq [str "Func ",
			   record [("args", List.layout Operand.layout args)]]
		 | Handler {offset} =>
		      seq [str "Handler", paren(Int.layout offset)]
		 | Jump => str "Jump"
	     end
	end

      datatype t = T of {label: Label.t,
			 kind: Kind.t,
			 live: Operand.t list,
			 profileInfo: {func: string, label: string},
			 statements: Statement.t vector,
			 transfer: Transfer.t}

      fun clear (T {label, ...}) = Label.clear label

      local
	 fun make g (T r) = g r
      in
	 val label = make #label
      end

      fun layout (T {label, kind, live, profileInfo, statements, transfer}) =
	 let
	    open Layout
	 in
	    align [seq [Label.layout label, 
			str " ",
			record [("kind", Kind.layout kind),
				("live", List.layout Operand.layout live)],
			str ":"],		   
		   align (Vector.toListMap (statements, Statement.layout)),
		   Transfer.layout transfer]
	 end

      fun layouts (block, output' : Layout.t -> unit) = output' (layout block)
   end

structure Chunk =
   struct
      datatype t = T of {chunkLabel: ChunkLabel.t,
			 (* where to start *)
			 entries: Label.t list,
			 gcReturns: Label.t list,
			 blocks: Block.t list,
			 (* for each type, gives the max # regs used *)
			 regMax: Type.t -> int}
      fun layout (T {blocks, ...}) =
	 let open Layout
	 in
	    align (List.map(blocks, Block.layout))
	 end

      fun layouts (c as T {blocks, ...}, output' : Layout.t -> unit) =
	 let open Layout
	 in List.foreach(blocks, fn block => Block.layouts(block, output'))
	 end
   end

structure Program =
   struct
      datatype t = T of {globals: Type.t -> int,
			 globalsNonRoot: int,
			 intInfs: (Global.t * string) list,
			 strings: (Global.t * string) list,
			 floats: (Global.t * string) list,
			 nextChunks: Label.t -> ChunkLabel.t option,
			 frameOffsets: int list list,
			 frameLayouts: Label.t -> {size: int,
						   offsetIndex: int} option,
			 maxFrameSize: int,
			 chunks: Chunk.t list,
			 main: {chunkLabel: ChunkLabel.t,
				label: Label.t}}

      fun layout (T {chunks, ...}) =
	 let open Layout
	 in 
	    align (List.map(chunks, Chunk.layout))
	 end

      fun layouts (p as T {chunks, ...}, output': Layout.t -> unit) =
	 let open Layout
	 in List.foreach(chunks, fn chunk => Chunk.layouts(chunk, output'))
	 end 
   end

end
