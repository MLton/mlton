(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature MACHINE_OUTPUT_STRUCTS =
   sig
      structure Label: HASH_ID
      structure Prim: PRIM
   end

signature MACHINE_OUTPUT =
   sig
      include MACHINE_OUTPUT_STRUCTS

      structure Type: MTYPE
      structure ChunkLabel: UNIQUE_ID

      structure Register: 
	 sig 
	    datatype t = T of {index: int,
			       ty: Type.t}

	    val index: t -> int
	    val layout: t -> Layout.t
	    val toString: t -> string
	    val ty: t -> Type.t
	 end

      structure Global:
	 sig
	    datatype t = T of {index: int,
			       ty: Type.t}

	    val index: t -> int
	    val layout: t -> Layout.t
	    val toString: t -> string
	    val ty: t -> Type.t
	 end
      
      structure Operand:
	 sig
	    datatype t =
	       ArrayOffset of {base: t, offset: t, ty: Type.t}
	     | CastInt of t
	     | Char of char
	     | Contents of {oper: t, ty: Type.t}
	     | Float of string 
	     | Global of Global.t
	     | GlobalPointerNonRoot of int
	     | Int of int
	     | IntInf of word
	     | Label of Label.t
	     | Offset of {base: t, offset: int, ty: Type.t}
	     | Pointer of int
	     | Register of Register.t
	     | StackOffset of {offset: int, ty: Type.t}
	     | Uint of Word.t

	    val layout: t -> Layout.t
	    val toString: t -> string
	    val ty: t -> Type.t
	 end

      structure GCInfo:
	 sig
	    datatype t = T of {(* Size of frame, including return address. *)
			       frameSize: int,
			       (* Live stack offsets. *)
			       live: Operand.t list,
			       return: Label.t}
	 end

      structure PrimInfo:
	 sig
	    datatype t =
	       None
	     | Runtime of GCInfo.t
	     | Normal of Operand.t list
	 end

      structure Statement:
	 sig
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
	       
	    val layout: t -> Layout.t
	 end

      structure Cases: MACHINE_CASES sharing Label = Cases.Label

      structure Transfer:
	 sig
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

	    val layout: t -> Layout.t
	 end

      structure Block:
	 sig
	   structure Kind:
	     sig
	       datatype t = Func of {args: Operand.t list}
		          | Jump
		          | Cont of {args: Operand.t list,
				     size: int}
		          | Handler of {size: int}
	     end
	  
	    datatype t = T of {label: Label.t,
			       kind: Kind.t,
			       (* Live registers and stack offsets at beginning of block. *)
			       live: Operand.t list,
			       profileName: string,
			       statements: Statement.t array,
			       transfer: Transfer.t}

	    val label: t -> Label.t
	 end

      structure Chunk:
	 sig
	    datatype t = T of {chunkLabel: ChunkLabel.t,
			       (* where to start *)
			       entries: Label.t list,
			       gcReturns: Label.t list,
			       blocks: Block.t list,
			       (* for each type, gives the max # regs used *)
			       regMax: Type.t -> int}
	 end

      structure Program:
	 sig
	    datatype t =
	       T of {globals: Type.t -> int,
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
		     main: {chunkLabel: ChunkLabel.t, label: Label.t}}

	    val layouts: t * (Layout.t -> unit) -> unit
	 end
   end
