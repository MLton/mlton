(* Copyright (C) 2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature MACHINE_ATOMS_STRUCTS =
   sig
      structure CFunction: C_FUNCTION
      structure CType: C_TYPE
      structure IntSize: INT_SIZE
      structure IntX: INT_X
      structure Label: ID
      structure Prim: PRIM
      structure RealSize: REAL_SIZE
      structure RealX: REAL_X
      structure Runtime: RUNTIME
      structure SourceInfo: SOURCE_INFO
      structure WordSize: WORD_SIZE
      structure WordX: WORD_X
      sharing CType = CFunction.CType = Runtime.CType
      sharing IntSize = CType.IntSize = IntX.IntSize = Prim.IntSize
      sharing RealSize = CType.RealSize = Prim.RealSize = RealX.RealSize
      sharing WordSize = CType.WordSize = Prim.WordSize = WordX.WordSize
   end

signature MACHINE_ATOMS =
   sig
      include MACHINE_ATOMS_STRUCTS

      structure ProfileLabel: PROFILE_LABEL

      structure PointerTycon:
	 sig
	    type t

	    val <= : t * t -> bool
	    val equals: t * t -> bool
	    val index: t -> int (* index into objectTypes array *)
	    val layout: t -> Layout.t
	    val new: unit -> t
	    val plist: t -> PropertyList.t
	    val stack: t
	    val thread: t
	    val toString: t -> string
	    val wordVector: t
	    val word8Vector: t
	 end

      type memChunk
      structure Type:
	 sig
	    datatype t =
	     (* The ints in an enum are in increasing order without dups.
	      * The pointers are in increasing order (of index in objectTypes
	      * vector) without dups.
	      *)
	       EnumPointers of {enum: int vector,
				pointers: PointerTycon.t vector}
	     | ExnStack
	     | Int of IntSize.t
	     | IntInf
	     | Label of Label.t
	     | MemChunk of memChunk (* An internal pointer. *)
	     | Real of RealSize.t
	     | Word of WordSize.t

	    val align: t * int -> int       (* align an address *)
	    val bool: t
	    val containsPointer: t * PointerTycon.t -> bool
	    val cPointer: unit -> t
	    val dePointer: t -> PointerTycon.t option
	    val defaultInt: t
	    val defaultWord: t
	    val equals: t * t -> bool
	    val exnStack: t
	    val fromCType: CType.t -> t
	    val int: IntSize.t -> t
	    val intInf: t
	    val isCPointer: t -> bool
	    val isPointer: t -> bool
	    val isReal: t -> bool
	    val label: Label.t -> t
	    val layout: t -> Layout.t
	    val name: t -> string (* simple one letter abbreviation *)
	    val pointer: PointerTycon.t -> t
	    val real: RealSize.t -> t
	    val size: t -> int
	    val stack: t
	    val thread: t
	    val toCType: t -> CType.t
	    val toString: t -> string
	    val word: WordSize.t -> t
	    val wordVector: t
	    val word8Vector: t
	 end

      structure MemChunk:
	 sig
	    (* The components are stored in increasing order of offset and are
	     * non-overlapping.
	     *)
	    datatype t =
	       T of {components: {mutable: bool,
				  offset: int,
				  ty: Type.t} vector,
		     size: int}
	       
	    val isValidInit: t * {offset: int, ty: Type.t} vector -> bool
	 end where type t = memChunk
      
      structure ObjectType:
	 sig
	    datatype t =
	       Array of MemChunk.t
	     | Normal of MemChunk.t
	     | Stack
	     | Weak of Type.t (* in Weak t, must have Type.isPointer t *)
	     | WeakGone

	    val basic: (PointerTycon.t * t) vector
	    val isOk: t -> bool
	    val layout: t -> Layout.t
	    val thread: t
	    val toRuntime: t -> Runtime.ObjectType.t
	    val weak: Type.t -> t
	    val wordVector: t
	    val word8Vector: t
	 end

      val castIsOk: {from: Type.t,
		     fromInt: IntX.t option,
		     to: Type.t,
		     tyconTy: PointerTycon.t -> ObjectType.t} -> bool
   end
