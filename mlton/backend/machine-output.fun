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

structure GCInfo =
   struct
      datatype t = T of {(* Size of frame, including return address. *)
			 frameSize: int,
			 (* Live stack offsets. *)
			 live: Operand.t list,
			 return: Label.t}

      fun layout (T {frameSize, live, return})
	= let open Layout
	  in
	    seq [str "GCInfo ",
		 record [("frameSize", Int.layout frameSize),
			 ("live", List.layout Operand.layout live),
			 ("return", Label.layout (return))]]
	  end
   end

structure PrimInfo =
   struct
      datatype t =
	 None
       | Runtime of GCInfo.t
       | Normal of Operand.t list

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
   end

structure Statement =
   struct
      datatype t =
	 Allocate of {dst: Operand.t,
		      size: int,
		      numPointers: int,
		      numWordsNonPointers: int,
		      stores: {offset: int,
			       value: Operand.t} list}
       | AllocateArray of {dst: Operand.t,
			   numElts: Operand.t,
			   numPointers: int,
			   numBytesNonPointers: int,
			   live: Operand.t list,
			   limitCheck: {gcInfo: GCInfo.t,
					bytesPerElt: int,
					bytesAllocated: int} option}
       | Assign of {dst: Operand.t option,
		    oper: Prim.t, 
		    pinfo: PrimInfo.t,
		    args: Operand.t list}
       | LimitCheck of {info: GCInfo.t,
			bytes: int,
			stackCheck: bool}
       | Move of {dst: Operand.t,
		  src: Operand.t}
       | Noop
       | Push of int
       | SetExnStackLocal of {offset: int}
       | SetExnStackSlot of {offset: int}
       | SetSlotExnStack of {offset: int}

      val layout =
	 let open Layout
	 in
	    fn Noop => str "Noop"
	     | Move {dst, src} =>
		  seq [Operand.layout dst, str " = ", Operand.layout src]
	     | Push i => seq [str "Push (", Int.layout i, str ")"]
	     | Assign {dst, oper, args, pinfo} =>
		  seq [case dst
			 of NONE => empty
			  | SOME dst => seq [Operand.layout dst, str " = "],
		       Prim.layout oper, str " ",
		       List.layout Operand.layout args, str " ",
		       PrimInfo.layout pinfo]
	     | LimitCheck {info, bytes, stackCheck} => 
		  seq [str "LimitCheck ",
		       record [("info", GCInfo.layout info),
			       ("bytes", Int.layout bytes),
			       ("stackCheck", Bool.layout stackCheck)]]
	     | SetExnStackLocal {offset} =>
		  seq [str "SetExnStackLocal ", Int.layout offset]
	     | SetExnStackSlot {offset} =>
		  seq [str "SetExnStackSlot ", Int.layout offset]
	     | SetSlotExnStack {offset} =>
		  seq [str "SetSlotExnStack ", Int.layout offset]
	     | Allocate {dst, stores, ...} =>
		  seq [Operand.layout dst, 
		       str " = Allocate[",
		       (paren o seq)
		       (separateRight(List.map(stores,
					       fn {offset, value}
					        => seq [Int.layout offset,
							str " <- ",
							Operand.layout value]),
				      ", ")),
		       str "]"]
	     | AllocateArray {dst, numElts, limitCheck, ...} =>
		  seq [Operand.layout dst, 
		       str " = AllocateArray[",
		       Operand.layout numElts,
		       str "] ",
		       case limitCheck
			 of NONE => empty
			  | SOME {gcInfo, ...} => GCInfo.layout gcInfo]
	 end

   end

structure Cases = MachineCases (structure Label = Label)

structure Transfer =
   struct
      datatype t =
	 Bug
       | FarJump of {chunkLabel: ChunkLabel.t,
		     label: Label.t,
		     live: Operand.t list,
		     return: {return: Label.t,
			      handler: Label.t option,
			      size: int} option}
       | NearJump of {label: Label.t,
		      return: {return: Label.t,
			       handler: Label.t option,
			       size: int} option}
       | Overflow of {args: Operand.t vector,
		      dst: Operand.t,
		      failure: Label.t,
		      prim: Prim.t,
		      success: Label.t}
       | Raise
       | Return of {live: Operand.t list}
       | Switch of {test: Operand.t,
		    cases: Cases.t,
		    default: Label.t option}
       | SwitchIP of {test: Operand.t,
		      int: Label.t,
		      pointer: Label.t}


      fun layout t =
	 let open Layout
	 in case t of
	    Bug => str "Bug"
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
	  | NearJump {label, return} => 
               seq [str "NearJump ", 
		    record [("label", Label.layout label),
			    ("return", Option.layout 
			               (fn {return, handler, size}
					 => record [("return", Label.layout return),
						    ("handler", Option.layout Label.layout handler),
						    ("size", Int.layout size)])
				       return)]]
	  | Overflow {args, dst, failure, prim, success} =>
	       seq [str "Overflow ",
		    record [("args", Vector.layout Operand.layout args),
			    ("dst", Operand.layout dst),
			    ("failure", Label.layout failure),
			    ("prim", Prim.layout prim),
			    ("success", Label.layout failure)]]
	  | Raise => str "Raise"
	  | Return {live} => 
               seq [str "Return ",
		    record [("live", List.layout Operand.layout live)]]
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
	  datatype t = Func of {args: Operand.t list}
	             | Jump
	             | Cont of {args: Operand.t list,
				size: int}
	             | Handler of {offset: int}

	  val layout
	    = let open Layout
	      in
		fn Func {args} 
		 => seq [str "Func ",
			 record [("args", List.layout Operand.layout args)]]
		 | Jump => str "Jump"
		 | Cont {args, size} 
		 => seq [str "Cont", paren(Int.layout size), str " ",
			 record [("args", List.layout Operand.layout args)]]
		 | Handler {offset} 
		 => seq [str "Handler", paren(Int.layout offset)]
	      end
	end

      datatype t = T of {label: Label.t,
			 kind: Kind.t,
			 live: Operand.t list,
			 profileName: string,
			 statements: Statement.t array,
			 transfer: Transfer.t}

      local
	 fun make g (T r) = g r
      in
	 val label = make #label
      end

      fun layout (T {label, kind, live, profileName, statements, transfer})
	= let open Layout
	  in
	    align [seq [Label.layout label, 
			str " ",
			record [("kind", Kind.layout kind),
				("live", List.layout Operand.layout live)],
			str ":"],		   
		   align (Array.toListMap(statements, Statement.layout)),
		   Transfer.layout transfer]
	  end

      fun layouts (block, output' : Layout.t -> unit)
	= output'(layout block)
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
      fun layout (T {blocks, ...})
	= let open Layout
	  in
	    align (List.map(blocks, Block.layout))
	  end

      fun layouts (c as T {blocks, ...}, output' : Layout.t -> unit)
	= let open Layout
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

      fun layout (T {chunks, ...})
	= let open Layout
	  in 
	    align (List.map(chunks, Chunk.layout))
	  end

      fun layouts (p as T {chunks, ...}, output': Layout.t -> unit)
	= let open Layout
	  in List.foreach(chunks, fn chunk => Chunk.layouts(chunk, output'))
	  end 
   end

end

