(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
   
signature WORD_SIZE_STRUCTS =
   sig
   end

signature WORD_SIZE =
   sig
      include WORD_SIZE_STRUCTS

      type t

      val + : t * t -> t
      val all: t list
      val bits: t -> Bits.t
      val bytes: t -> Bytes.t
      val byte: t
      val cardinality: t -> IntInf.t
      val compare: t * t -> Relation.t
      val default: t
      val equals: t * t -> bool
      val fromBits: Bits.t -> t
      val layout: t -> Layout.t
      val max: t -> IntInf.t
      val memoize: (t -> 'a) -> t -> 'a
      val one: t
      val pointer: unit -> t
      datatype prim = W8 | W16 | W32 | W64
      val prim: t -> prim
      val prims: t list
      val roundUpToPrim: t -> t
      val toString: t -> string
   end
