(*************************************************************************)
(* string-key.sml
 *
 *  (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure StringKey = struct
    type ord_key = string
    val compare = String.compare
end
(*************************************************************************)
(* string-set.sml
 *
 *  (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure StringSet = RedBlackSetFn (StringKey)
(*************************************************************************)
(* string-map.sml
 *
 *  (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure StringMap = RedBlackMapFn (StringKey)
(*************************************************************************)
structure IntListKey = struct
    type ord_key = int list
    val compare = List.collate Int.compare
end
(*************************************************************************)
(* intlist-map.sml
 *
 *  (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure IntListMap = RedBlackMapFn (IntListKey)
(*************************************************************************)
structure LargeIntKey = struct
    type ord_key = LargeInt.int
    val compare = LargeInt.compare
end
(*************************************************************************)
structure LargeIntSet = RedBlackSetFn (LargeIntKey)
(*************************************************************************)
