(* Copyright (C) 2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature MACHINE_ATOMS_STRUCTS =
   sig
      structure Label: HASH_ID
      structure Prim: PRIM
      structure Runtime: RUNTIME
   end

signature MACHINE_ATOMS =
   sig
      include MACHINE_ATOMS_STRUCTS

      structure PointerTycon:
	 sig
	    type t

	    val <= : t * t -> bool
	    val equals: t * t -> bool
	    val index: t -> int (* index into pointerTypes array *)
	    val layout: t -> Layout.t
	    val new: unit -> t
	    val plist: t -> PropertyList.t
	    val stack: t
	    val string: t
	    val thread: t
	    val toString: t -> string
	    val wordVector: t
	 end

      type memChunk
      structure Type:
	 sig
	    datatype t =
	       Char
	     | CPointer
	     (* The ints in an enum are in increasing order without dups.
	      * The pointers are in increasing order (of index in pointerTypes
	      * vector) without dups.
	      *)
	     | EnumPointers of {enum: int vector,
				pointers: PointerTycon.t vector}
	     | Int
	     | IntInf
	     | Label
	     | MemChunk of memChunk (* An internal pointer. *)
	     | Real
	     | Word

	    val align: t * int -> int       (* align an address *)
	    val bool: t
	    val char: t
	    val containsPointer: t * PointerTycon.t -> bool
	    val cpointer: t
	    val dePointer: t -> PointerTycon.t option
	    val equals: t * t -> bool
	    val fromRuntime: Runtime.Type.t -> t
	    val int: t
	    val intInf: t
	    val isPointer: t -> bool
	    val label: t
	    val layout: t -> Layout.t
	    val name: t -> string (* simple one letter abbreviation *)
	    val pointer: PointerTycon.t -> t
	    val real: t
	    val size: t -> int
	    val stack: t
	    val string: t
	    val thread: t
	    val toRuntime: t -> Runtime.Type.t
	    val toString: t -> string
	    val word: t
	    val wordVector: t
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

	    val basic: (PointerTycon.t * t) vector
	    val isOk: t -> bool
	    val layout: t -> Layout.t
	    val stack: t
	    val string: t
	    val thread: t
	    val toRuntime: t -> Runtime.ObjectType.t
	    val wordVector: t
	 end

      val castIsOk: {from: Type.t,
		     fromInt: int option,
		     to: Type.t,
		     tyconTy: PointerTycon.t -> ObjectType.t} -> bool
   end
