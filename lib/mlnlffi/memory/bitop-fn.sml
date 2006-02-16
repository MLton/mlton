(* bitop-fn.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(* bitop-fn.sml
 *
 *    Bit operations on integers as if they were words
 *         (based on suggestions from Allen Leung).
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
functor IntBitOps (structure I : INTEGER structure W : WORD) : sig

    (* We use a functor to express this stuff generically.
     * If efficiency is a concern, it may be necessary to
     * expand this "by hand".... *)

    type int = I.int

    (* unsigned arithmetic. 
     * non-overflow trapping 
     *)
    val ++   : int * int -> int
    val --   : int * int -> int
    val **   : int * int -> int
    val udiv : int * int -> int
    val umod : int * int -> int
    val umin : int * int -> int
    val umax : int * int -> int

    (* bit ops *)
    val notb : int -> int
    val andb : int * int -> int
    val orb  : int * int -> int
    val xorb : int * int -> int
    val << : int * Word.word -> int
    val >> : int * Word.word -> int
    val ~>> : int * Word.word -> int

    (* unsigned comparisons *)
    val ule   : int * int -> bool
    val ulg   : int * int -> bool
    val ugt   : int * int -> bool
    val uge   : int * int -> bool
    val ucompare : int * int -> order

end = struct

    type int = I.int

    local
        val to   = W.fromLargeInt o I.toLarge
        val from = I.fromLarge o W.toLargeIntX
        fun bop f (x, y) = from (f (to x, to y)) (* binary op *)
        fun uop f x = from (f (to x))            (* unary op *)
        fun sop f (x, y) = from (f (to x, y))    (* shift-like op *)
        fun cop f (x, y) = f (to x, to y)        (* comparison-like op *)
    in
        val ++ = bop W.+
        val -- = bop W.-
        val ** = bop W.*
        val udiv = bop W.div
        val umod = bop W.mod
        val andb = bop W.andb
        val orb = bop W.orb
        val xorb = bop W.xorb
        val notb = uop W.notb

        val umax = bop W.max
        val umin = bop W.min

        val << = sop W.<<
        val >> = sop W.>>
        val ~>> = sop W.~>>

        val ulg = cop W.<
        val ule = cop W.<=
        val ugt = cop W.>
        val uge = cop W.>=
        val ucompare = cop W.compare
    end
end
