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
	| Char c => Char.toString c
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
			 return: Label.t}
   end

structure PrimInfo =
   struct
      datatype t =
	 None
       | Overflow of Label.t

      fun foreachLabel (i: t, f) =
	 case i of
	    None => ()
	  | Overflow l => f l
   end

structure Statement =
   struct
      datatype t =
	 Noop
       | Move of {dst: Operand.t,
		  src: Operand.t}
       | Push of int
       | Assign of {dst: Operand.t option,
		    oper: Prim.t,
		    pinfo: PrimInfo.t,
		    args: Operand.t list,
		    info: GCInfo.t option}		    
       | LimitCheck of {info: GCInfo.t,
			bytes: int,
			stackCheck: bool}
       | SaveExnStack of {offset: int}
       | RestoreExnStack of {offset: int}
       | Allocate of {dst: Operand.t,
		      size: int,
		      numPointers: int,
		      numWordsNonPointers: int,
		      stores: {offset: int,
			       value: Operand.t} list}
       | AllocateArray of {dst: Operand.t,
			   numElts: Operand.t,
			   numPointers: int,
			   numBytesNonPointers: int,
			   limitCheck: {gcInfo: GCInfo.t,
					bytesPerElt: int,
					bytesAllocated: int} option}

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
		       List.layout Operand.layout args]
	     | LimitCheck _ => str "LimitCheck"
	     | SaveExnStack {offset, ...} =>
		  seq [str "SaveExnStack (", Int.layout offset, str ")"]
	     | RestoreExnStack {offset, ...} =>
		  seq [str "RestoreExnStack (", Int.layout offset, str ")"]
	     | Allocate {dst, ...} =>
		  seq [Operand.layout dst, str " = Allocate"]
	     | AllocateArray {dst, ...} =>
		  seq [Operand.layout dst, str " = AllocateArray"]
	 end

   end

structure Cases = MachineCases (structure Label = Label)

structure Transfer =
   struct
      datatype t =
	 Bug
       | Return
       | Raise
       | Switch of {test: Operand.t,
		    cases: Cases.t,
		    default: Label.t option}
       | SwitchIP of {test: Operand.t,
		      int: Label.t,
		      pointer: Label.t}
       | NearJump of {label: Label.t}
       | FarJump of {chunkLabel: ChunkLabel.t,
		     label: Label.t}

      fun layout t =
	 let open Layout
	 in case t of
	    Bug => str "Bug"
	  | FarJump {label, ...} => seq [str "FarJump ", Label.layout label]
	  | NearJump {label, ...} => seq [str "NearJump ", Label.layout label]
	  | Raise => str "Raise"
	  | Return => str "Return"
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
      datatype t = T of {label: Label.t,
			 live: Register.t list,
			 profileName: string,
			 statements: Statement.t array,
			 transfer: Transfer.t}
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

   end

end

