(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
   
signature INT_SIZE_STRUCTS =
   sig
   end

signature INT_SIZE =
   sig
      include INT_SIZE_STRUCTS
	 
      type t

      val all: t list
      val bits: t -> Bits.t
      val bytes: t -> Bytes.t
      val cardinality: t -> IntInf.t
      val compare: t * t -> Relation.t
      val default: t
      val equals: t * t -> bool
      val I : Bits.t -> t
      val isInRange: t * IntInf.t -> bool
      val layout: t -> Layout.t
      val max: t -> IntInf.t
      val memoize: (t -> 'a) -> t -> 'a
      val min: t -> IntInf.t
      datatype prim = I8 | I16 | I32 | I64
      val prim: t -> prim
      val prims: t list
      val range: t -> IntInf.t * IntInf.t
      val roundUpToPrim: t -> t
      val toString: t -> string
   end
