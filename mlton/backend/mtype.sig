(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature MTYPE_STRUCTS = 
   sig
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
   end

signature MTYPE = 
   sig
      include MTYPE_STRUCTS

      type t
	 
      datatype dest =
	 Int of IntSize.t
       | Pointer
       | Real of RealSize.t
       | Word of WordSize.t

      val align4: int -> int
      val align8: int -> int
      val align: t * int -> int (* align an address *)	 
      val all: t list
      val bool: t (* same as int *)
      val defaultInt: t
      val defaultReal: t
      val defaultWord: t
      val dest: t -> dest
      val equals: t * t -> bool
      val int: IntSize.t -> t
      val isPointer: t -> bool
      val isReal: t -> bool
      val label: t (* same as uint *)
      val layout: t -> Layout.t
      val memo: (t -> 'a) -> (t -> 'a)
      (* name: R{32,64} I{8,16,32,64] P W[8,16,32] *)
      val name: t -> string
      val pointer: t
      val real: RealSize.t -> t
      val size: t -> int (* bytes *)
      val toString: t -> string
      val word: WordSize.t -> t
   end
