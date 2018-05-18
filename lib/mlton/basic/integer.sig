(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INTEGER_STRUCTS =
   sig
      eqtype int

      val * : int * int -> int 
      val + : int * int -> int 
      val - : int * int -> int 
      val < : int * int -> bool 
      val <= : int * int -> bool 
      val > : int * int -> bool 
      val >= : int * int -> bool 
      val abs: int -> int 
      val compare: int * int -> order 
      val div: int * int -> int 
      val divMod: int * int -> int * int
      val fmt: StringCvt.radix -> int -> string 
      val fromInt: Pervasive.Int.int -> int 
      val fromLarge: Pervasive.LargeInt.int -> int 
      val fromString: string -> int option 
      val max: int * int -> int 
      val maxInt: int option 
      val min: int * int -> int 
      val minInt: int option 
      val mod: int * int -> int 
      val precision: Pervasive.Int.int option 
      val quot: int * int -> int 
      val quotRem: int * int -> int * int
      val rem: int * int -> int
      val sameSign: int * int -> bool 
      val scan: StringCvt.radix
         -> (char, 'a) StringCvt.reader
         -> (int, 'a) StringCvt.reader
      val sign: int -> Pervasive.Int.int 
      val toInt: int -> Pervasive.Int.int 
      val toIntInf: int -> Pervasive.IntInf.int
      val toLarge: int -> Pervasive.LargeInt.int 
      val toString: int -> string 
      val ~ : int -> int 
   end

signature INTEGER =
   sig
      include EUCLIDEAN_RING
      type int = t

      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool
      val abs: t -> t
      val choose: t * t -> t
      val compare: t * t -> Relation.t
      val exists: t * t * (t -> bool) -> bool
      val factorial: t -> t
      val fold: t * t * 'a * (t * 'a -> 'a) -> 'a
      val foldDown: t * t * 'a * (t * 'a -> 'a) -> 'a
      val for: t * t * (t -> unit) -> unit
      val forall: t * t * (t -> bool) -> bool
      val format: t * StringCvt.radix -> string 
      val forDown: t * t * (t -> unit) -> unit
      val fromString: string -> t option
      exception Input
      val input: In0.t -> t
      val isEven: t -> bool
      val isNegative: t -> bool
      val isOdd: t -> bool
      val isPositive: t -> bool
      (* largest (i, f) is the largest j <= i such that f j *)
      val largest: t * (t -> bool) -> t
      val least: t * t * (t -> bool) -> t option
      val map: t * t * (t -> 'a) -> 'a list
      val max: t * t -> t
      val min: t * t -> t
      val output: t * Out.t -> unit
      val quot: t * t -> t 
      val quotRem: t * t -> t * t
      val rem: t * t -> t
      val scan: (StringCvt.radix * (char, 'a) StringCvt.reader)
         -> (t, 'a) StringCvt.reader
      (* smallest (i, f) is the smallest j >= i such that f j *)
      val smallest: t * (t -> bool) -> t
      (* val sum: {from: t, to: t, term: t -> t} -> t *)
      val toCommaString: t -> string
      val toInt: t -> Pervasive.Int.int
      val toIntInf: t -> Pervasive.IntInf.int
      val toLarge: t -> Pervasive.LargeInt.int
      val toString: t -> string
   end
