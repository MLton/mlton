(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature MACHINE_OUTPUT_STRUCTS =
   sig
      structure Label: ID
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
			       return: Label.t}
	 end

      structure PrimInfo:
	 sig
	    datatype t =
	       None
	     | Overflow of Label.t

	    val foreachLabel: t * (Label.t -> unit) -> unit
	 end

      structure Statement:
	 sig
	    datatype t = Noop
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
	    val layout: t -> Layout.t
	 end

      structure Cases: MACHINE_CASES sharing Label = Cases.Label

      structure Transfer:
	 sig
	    datatype t = Bug
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

	    val layout: t -> Layout.t
	 end

      structure Block:
	 sig
	    datatype t = T of {label: Label.t,
			       (* Live registers at beginning of block. *)
			       live: Register.t list,
			       profileName: string,
			       statements: Statement.t array,
			       transfer: Transfer.t}
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
	 end
   end
