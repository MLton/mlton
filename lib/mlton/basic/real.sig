(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature REAL =
   sig
      structure Format:
	 sig
	    type t

	    val exact: t
	    val fix: int option -> t
	    val gen: int option -> t
	    val sci: int option -> t
	 end

      type t
      exception Input
      val + : t * t -> t
      val - : t * t -> t
      val * : t * t -> t
      val / : t * t -> t
      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool
      val abs: t -> t
      val acos: t -> t
      val add1: t -> t
      val asin: t -> t
      val atan2: t * t -> t
      val atan: t -> t
      val ceiling: t -> int
      val choose: t * t -> t
      val compare: t * t -> Relation.t
      val cos: t -> t
      val dec: t ref -> unit
      val equals: t * t -> bool
      val exp: t -> t
      val floor: t -> int
      val format: t * Format.t -> string
      val fromInt: Pervasive.Int.int -> t (* fromInt n = 1 + ... + 1, n times. *)
      val fromIntInf: Pervasive.IntInf.int -> t
      val fromString: string -> t option
      val inc: t ref -> unit
      val input: In0.t -> t
      val inverse: t -> t
      val isFinite: t -> bool
      val layout: t -> Layout.t
      val ln: t -> t
      val log2: t -> t
      val log: t * t -> t
      val maxFinite: t
      val negOne: t
      val one: t
      val pi: t
      val pow: t * t -> t
      val prod: t list -> t
      val realMod: t -> t
      val realPower: t * t -> t
      val round: t -> int
      val signBit: t -> bool
      val sin: t -> t
      val sqrt: t -> t
      val sub1: t -> t
      val tan: t -> t
      val three: t
      val toIntInf: t -> Pervasive.IntInf.int
      val toString: t -> string
      val trunc: t -> int
      val two: t
      val zero: t
      val ~ : t -> t
   end
