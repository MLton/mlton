(* ord-key-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Abstract linearly ordered keys.
 *
 *)

signature ORD_KEY =
  sig
    type ord_key

    val compare: ord_key * ord_key -> order

  end (* ORD_KEY *)
