(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
      include MACHINE_ATOMS

      structure Switch: SWITCH
      sharing Label = Switch.Label
      sharing PointerTycon = Switch.PointerTycon
      sharing Type = Switch.Type
      structure CFunction: C_FUNCTION
      sharing CFunction = Runtime.CFunction
      structure ChunkLabel: UNIQUE_ID

      structure Register:
	 sig
	    type t

	    val equals: t * t -> bool
	    val index: t -> int 
	    val layout: t -> Layout.t
	    val new: Type.t -> t
	    val plist: t -> PropertyList.t
	    val toString: t -> string
	    val ty: t -> Type.t
	 end

      structure Global:
	 sig
	    type t

	    val equals: t * t -> bool
	    val index: t -> int
	    val isRoot: t -> bool
	    val layout: t -> Layout.t
	    val new: {isRoot: bool, ty: Type.t} -> t
	    val numberOfNonRoot: unit -> int
	    val numberOfType: Runtime.Type.t -> int
	    val toString: t -> string
	    val ty: t -> Type.t
	 end

      structure Operand:
	 sig
	    datatype t =
	       ArrayOffset of {base: t,
			       index: t,
			       ty: Type.t}
	     | Cast of t * Type.t
	     | Char of char
	     | Contents of {oper: t,
			    ty: Type.t}
	     | File (* expand by codegen into string constant *)
	     | GCState
	     | Global of Global.t
	     | Int of int
	     | Label of Label.t
	     | Line (* expand by codegen into int constant *)
	     | Offset of {base: t,
			  offset: int,
			  ty: Type.t}
	     | Real of string
	     | Register of Register.t
	     | Runtime of Runtime.GCField.t
	     | SmallIntInf of word
	     | StackOffset of {offset: int,
			       ty: Type.t}
	     | Word of Word.t

	    val equals: t * t -> bool
	    val interfere: {write: t, read: t} -> bool
	    val layout: t -> Layout.t
	    val toString: t -> string
	    val ty: t -> Type.t
	 end
      sharing Operand = Switch.Use

      structure Statement:
	 sig
	    datatype t =
	     (* When registers or offsets appear in operands, there is an
	      * implicit contents of.
	      * When they appear as locations, there is not.
	      *)
	       Move of {dst: Operand.t,
			src: Operand.t}
	     | Noop
	     (* Fixed-size allocation. *)
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

	    val foldOperands: t * 'a * (Operand.t * 'a -> 'a) -> 'a
	    val layout: t -> Layout.t
	    val move: {dst: Operand.t, src: Operand.t} -> t
	    (* Error if dsts and srcs aren't of same length. *)
	    val moves: {dsts: Operand.t vector,
			srcs: Operand.t vector} -> t vector
	 end

      structure FrameInfo:
	 sig
	    datatype t =
	       T of {(* Index into frameOffsets *)
		     frameOffsetsIndex: int,
		     (* Size of frame in bytes, including return address. *)
		     size: int}

	    val bogus: t
	    val layout: t -> Layout.t
	    val size: t -> int
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
			 success: Label.t,
			 ty: Type.t} (* int or word *)
	     | CCall of {args: Operand.t vector,
			 frameInfo: FrameInfo.t option,
			 func: CFunction.t,
			 (* return is NONE iff the func doesn't return.
			  * Else, return must be SOME l, where l is of CReturn
			  * kind with a matching func.
			  *)
			 return: Label.t option}
	     | Call of {label: Label.t, (* label must be a Func *)
			live: Operand.t vector,
			return: {return: Label.t,
				 handler: Label.t option,
				 size: int} option}
	     | Goto of Label.t (* label must be a Jump *)
	     | Raise
	     | Return of {live: Operand.t vector}
	     | Switch of Switch.t

	    val foldOperands: t * 'a * (Operand.t * 'a -> 'a) -> 'a
	    val layout: t -> Layout.t
	 end
      
      structure Kind:
	 sig
	    datatype t =
	       Cont of {args: Operand.t vector,
			frameInfo: FrameInfo.t}
	     | CReturn of {dst: Operand.t option,
			   frameInfo: FrameInfo.t option,
			   func: CFunction.t}
	     | Func of {args: Operand.t vector}
	     | Handler of {offset: int}
	     | Jump

	    val frameInfoOpt: t -> FrameInfo.t option
	 end
      
      structure Block:
	 sig
	    datatype t =
	       T of {kind: Kind.t,
		     label: Label.t,
		     (* Live registers and stack offsets at beginning of block. *)
		     live: Operand.t vector,
		     profileInfo: {ssa: {func: string, label: string},
				   rssa: {func: string, label: string}},
		     statements: Statement.t vector,
		     transfer: Transfer.t}

	    val label: t -> Label.t
	 end

      structure Chunk:
	 sig
	    datatype t = T of {blocks: Block.t vector,
			       chunkLabel: ChunkLabel.t}

	    (* Fold over each register that appears in the chunk.
	     * May visit duplicates.
	     *)
	    val foldRegs: t * 'a * (Register.t * 'a -> 'a) -> 'a
	 end

      structure Program:
	 sig
	    datatype t =
	       T of {chunks: Chunk.t list,
		     (* Each vector in frame Offsets specifies the offsets
		      * of live pointers in a stack frame.  A vector is referred
		      * to by index as the frameOffsetsIndex in a block kind.
		      *)
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

	    (* Fold over each register that appears in the chunk.
	     * May visit duplicates.
	     *)
	    val foldRegs: t * 'a * (Register.t * 'a -> 'a) -> 'a
	    val layouts: t * (Layout.t -> unit) -> unit
	    val typeCheck: t -> unit
	 end
   end
