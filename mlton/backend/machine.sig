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
      structure IntX: INT_X
      structure Label: HASH_ID
      structure Prim: PRIM
      structure SourceInfo: SOURCE_INFO
      structure RealX: REAL_X
      structure WordX: WORD_X
      sharing IntX.IntSize = Prim.IntSize
      sharing RealX.RealSize = Prim.RealSize
      sharing WordX.WordSize = Prim.WordSize
   end

signature MACHINE = 
   sig
      include MACHINE_ATOMS

      structure Switch: SWITCH
      sharing IntX = Switch.IntX
      sharing Label = Switch.Label
      sharing PointerTycon = Switch.PointerTycon
      sharing Type = Switch.Type
      sharing WordX = Switch.WordX
      structure CFunction: C_FUNCTION
      sharing CFunction = Runtime.CFunction
      structure ChunkLabel: ID_NO_AST

      structure Register:
	 sig
	    type t

	    val equals: t * t -> bool
	    val index: t -> int
	    val indexOpt: t -> int option
	    val layout: t -> Layout.t
	    val new: Type.t * int option -> t
	    val setIndex: t * int -> unit
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
	     | Contents of {oper: t,
			    ty: Type.t}
	     | File (* expanded by codegen into string constant *)
	     | Frontier
	     | GCState
	     | Global of Global.t
	     | Int of IntX.t
	     | Label of Label.t
	     | Line (* expand by codegen into int constant *)
	     | Offset of {base: t,
			  offset: int,
			  ty: Type.t}
	     | Real of RealX.t
	     | Register of Register.t
	     | SmallIntInf of word
	     | StackOffset of {offset: int,
			       ty: Type.t}
	     | StackTop
	     | Word of WordX.t

	    val equals: t * t -> bool
	    val interfere: t * t -> bool
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
	     | ProfileLabel of ProfileLabel.t

	    val foldOperands: t * 'a * (Operand.t * 'a -> 'a) -> 'a
	    val layout: t -> Layout.t
	    val move: {dst: Operand.t, src: Operand.t} -> t
	    (* Error if dsts and srcs aren't of same length. *)
	    val moves: {dsts: Operand.t vector,
			srcs: Operand.t vector} -> t vector
	 end

      structure FrameInfo:
	 sig
	    datatype t = T of {frameLayoutsIndex: int}

	    val equals: t * t -> bool
	    val layout: t -> Layout.t
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
	     | Return
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
	     | Func
	     | Handler of {frameInfo: FrameInfo.t,
			   handles: Operand.t vector}
	     | Jump

	    val frameInfoOpt: t -> FrameInfo.t option
	 end
      
      structure Block:
	 sig
	    datatype t =
	       T of {kind: Kind.t,
		     label: Label.t,
		     (* Live registers and stack offsets at start of block. *)
		     live: Operand.t vector,
		     raises: Operand.t vector option,
		     returns: Operand.t vector option,
		     statements: Statement.t vector,
		     transfer: Transfer.t}

	    val foldDefs: t * 'a * (Operand.t * 'a -> 'a) -> 'a
	    val label: t -> Label.t
	 end

      structure Chunk:
	 sig
	    datatype t =
	       T of {blocks: Block.t vector,
		     chunkLabel: ChunkLabel.t,
		     (* Register.index r
		      *    <= regMax (Type.toRuntime (Register.ty r))
		      * for all registers in the chunk.
		      *)
		     regMax: Runtime.Type.t -> int}
	 end

      structure ProfileInfo:
	 sig
	    datatype t =
	       T of {(* For each frame, gives the index into sourceSeqs of the
		      * source functions corresponding to the frame.
		      *)
	             frameSources: int vector,
		     labels: {label: ProfileLabel.t,
			      sourceSeqsIndex: int} vector,
		     (* Each sourceSeq describes a sequence of source functions,
		      * each given as an index into the source vector.
		      *)
		     sourceSeqs: int vector vector,
		     sourceSuccessors: int vector,
		     sources: SourceInfo.t vector}

	    val modify: t -> {newProfileLabel: ProfileLabel.t -> ProfileLabel.t,
			      delProfileLabel: ProfileLabel.t -> unit,
			      getProfileInfo: unit -> t}
	 end

      structure Program:
	 sig
	    datatype t =
	       T of {chunks: Chunk.t list,
		     frameLayouts: {frameOffsetsIndex: int,
				    isC: bool,
				    size: int} vector,
		     (* Each vector in frame Offsets specifies the offsets
		      * of live pointers in a stack frame.  A vector is referred
		      * to by index as the offsetsIndex in frameLayouts.
		      *)
		     frameOffsets: int vector vector,
		     handlesSignals: bool,
		     intInfs: (Global.t * string) list,
		     main: {chunkLabel: ChunkLabel.t,
			    label: Label.t},
		     maxFrameSize: int,
		     objectTypes: ObjectType.t vector,
		     profileInfo: ProfileInfo.t,
		     reals: (Global.t * RealX.t) list,
		     strings: (Global.t * string) list}

	    val frameSize: t * FrameInfo.t -> int
	    val layouts: t * (Layout.t -> unit) -> unit
	    val typeCheck: t -> unit
	 end
   end
