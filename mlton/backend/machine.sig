(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature MACHINE_STRUCTS = 
   sig
      structure Label: HASH_ID
      structure Prim: PRIM
   end

signature MACHINE = 
   sig
      include MACHINE_STRUCTS
	 
      structure ChunkLabel: UNIQUE_ID
      structure Type: MTYPE

      structure Register:
	 sig
	    datatype t = T of {index: int,
			       ty: Type.t}

	    val equals: t * t -> bool
	    val index: t -> int
	    val layout: t -> Layout.t
	    val toString: t -> string
	    val ty: t -> Type.t
	 end

      structure Global:
	 sig
	    datatype t = T of {index: int,
			       ty: Type.t}

	    val equals: t * t -> bool
	    val index: t -> int
	    val layout: t -> Layout.t
	    val toString: t -> string
	    val ty: t -> Type.t
	 end

      structure Operand:
	 sig
	    datatype t =
	       ArrayOffset of {base: t,
			       index: t,
			       ty: Type.t}
	     | CastInt of t (* takes an IntOrPointer and makes it an int *)
	     | Char of char
	     | Contents of {oper: t,
			    ty: Type.t}
	     | Float of string 
	     | Global of Global.t
	     | GlobalPointerNonRoot of int
	     | Int of int
	     | IntInf of word
	     | Label of Label.t
	     | Offset of {base: t,
			  offset: int,
			  ty: Type.t}
	     | Pointer of int (* the int must be nonzero mod 4. *)
	     | Register of Register.t
	     | StackOffset of {offset: int,
			       ty: Type.t}
	     | Uint of Word.t

	    val equals: t * t -> bool
	    val interfere: {write: t, read: t} -> bool
	    val layout: t -> Layout.t
	    val toString: t -> string
	    val ty: t -> Type.t
	 end

      structure Statement:
	 sig
	    datatype t =
	       (* Variable-sized allocation. *)
	       Array of {dst: Operand.t,
			 numElts: Operand.t,
			 numPointers: int,
			 numBytesNonPointers: int}
	     (* When registers or offsets appear in operands, there is an
	      * implicit contents of.
	      * When they appear as locations, there is not.
	      *)
	     | Move of {dst: Operand.t,
			src: Operand.t}
	     | Noop
	     (* Fixed-size allocation. *)
	     | Object of {dst: Operand.t,
			  numPointers: int,
			  numWordsNonPointers: int,
			  size: int, (* in bytes, not including header *)
			  stores: {offset: int,
				   value: Operand.t} vector}
	     | PrimApp of {args: Operand.t vector,
			   dst: Operand.t option,
			   prim: Prim.t}
	     | SetExnStackLocal of {offset: int}
	     | SetExnStackSlot of {offset: int}
	     | SetSlotExnStack of {offset: int}

	    val layout: t -> Layout.t
	    val move: {dst: Operand.t, src: Operand.t} -> t
	    (* Error if dsts and srcs aren't of same length. *)
	    val moves: {dsts: Operand.t vector,
			srcs: Operand.t vector} -> t vector
	 end

      structure Cases: MACHINE_CASES sharing Label = Cases.Label

      structure LimitCheck:
	 sig
	    datatype t =
	       Array of {bytesPerElt: int, (* > 0 *)
			 extraBytes: int, (* >= 0.  for subsequent allocation. *)
			 numElts: Operand.t, (* of type int, but not an immediate *)
			 stackToo: bool}
	     | Heap of {bytes: int,
			stackToo: bool}
	     | Signal
	     | Stack
	 end
   
      structure Transfer:
	 sig
	    datatype t =
	       (* In an arith transfer, dst is modified whether or not the
		* prim succeeds.
		*)
	       Arith of {args: Operand.t vector,
			 dst: Operand.t,
			 overflow: Label.t,
			 prim: Prim.t,
			 success: Label.t}
	     | Bug
	     | CCall of {args: Operand.t vector,
			 prim: Prim.t,
			 (* return must be CReturn with matching prim. *)
			 return: Label.t,
			 (* returnTy must CReturn dst. *)
			 returnTy: Type.t option}
	     | Call of {label: Label.t, (* label must be a Func *)
			live: Operand.t vector,
			return: {return: Label.t,
				 handler: Label.t option,
				 size: int} option}
	     | Goto of Label.t (* label must be a Jump *)
	     | LimitCheck of {(* failure must be Runtime. *)
			      failure: Label.t, 
			      kind: LimitCheck.t,
			      (* success must be Jump. *)
			      success: Label.t} 
	     | Raise
	     | Return of {live: Operand.t vector}
	     | Runtime of {args: Operand.t vector,
			   prim: Prim.t,
			   return: Label.t} (* Must be of Runtime kind. *)
	     | Switch of {test: Operand.t,
			  cases: Cases.t,
			  default: Label.t option}
	     (* Switch to one of two labels, based on whether the operand is an
	      * Integer or a Pointer.  Pointers are word aligned and integers
	      * are not.
	      *)
	     | SwitchIP of {test: Operand.t,
			    int: Label.t,
			    pointer: Label.t}

	    val layout: t -> Layout.t
	 end

      structure FrameInfo:
	 sig
	    datatype t =
	       T of {(* Index into frameOffsets *)
		     frameOffsetsIndex: int,
		     (* Size of frame in bytes, including return address. *)
		     size: int}

	    val bogus: t
	    val size: t -> int
	 end
      
      structure Kind:
	 sig
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

	    val frameInfoOpt: t -> FrameInfo.t option
	 end
      
      structure Block:
	 sig
	    datatype t =
	       T of {kind: Kind.t,
		     label: Label.t,
		     (* Live registers and stack offsets at beginning of block. *)
		     live: Operand.t vector,
		     profileInfo: {func: string, label: string},
		     statements: Statement.t vector,
		     transfer: Transfer.t}

	    val label: t -> Label.t
	 end

      structure Chunk:
	 sig
	    datatype t = T of {chunkLabel: ChunkLabel.t,
			       blocks: Block.t vector,
			       (* for each type, gives the max # regs used *)
			       regMax: Type.t -> int}
	 end

      structure Program:
	 sig
	    datatype t =
	       T of {chunks: Chunk.t list,
		     floats: (Global.t * string) list,
		     (* Each vector in frame Offsets is a specifies the offsets
		      * of live pointers in a stack frame.  A vector is referred
		      * to by index as the frameOffsetsIndex in a block kind.
		      *)
		     frameOffsets: int vector vector,
		     globals: Type.t -> int,
		     globalsNonRoot: int,
		     intInfs: (Global.t * string) list,
		     main: {chunkLabel: ChunkLabel.t,
			    label: Label.t},
		     maxFrameSize: int,
		     strings: (Global.t * string) list}

	    val layouts: t * (Layout.t -> unit) -> unit
	    val typeCheck: t -> unit
	 end
   end
