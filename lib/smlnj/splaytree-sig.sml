(* splaytree-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for a splay tree data structure.
 *
 *)

signature SPLAY_TREE = 
  sig
    datatype 'a splay = 
      SplayObj of {
        value: 'a,
        right: 'a splay,
        left: 'a splay
      }
    | SplayNil


    val splay: (('a -> order) * 'a splay) -> (order * 'a splay)
      (* (r, tree') = splay (cmp, tree) 
       * where tree' is tree adjusted using the comparison function cmp
       * and, if tree' = SplayObj{value, ...}, r = cmp value.
       * tree' = SplayNil iff tree = SplayNil, in which case r is undefined.
       *)

    val join: 'a splay * 'a splay -> 'a splay
      (* join(t, t') returns a new splay tree formed of t and t'
       *)

  end (* SPLAY_TREE *)

