(* ord-map-sig.sml
 *
 * COPYRIGHT (c) 1996 by AT&T Research.  See COPYRIGHT file for details.
 *
 * Abstract signature of an applicative-style finite maps (dictionaries)
 * structure over ordered monomorphic keys.
 *)

signature ORD_MAP =
  sig

    structure Key: ORD_KEY

    type 'a map

    val empty: 'a map
	(* The empty map *)

    val insert : 'a map * Key.ord_key * 'a -> 'a map
    val insert': ((Key.ord_key * 'a) * 'a map) -> 'a map
	(* Insert an item. *)

    val find: 'a map * Key.ord_key -> 'a option
	(* Look for an item, return NONE if the item doesn't exist *)

    val remove: 'a map * Key.ord_key -> 'a map * 'a
	(* Remove an item, returning new map and value removed.
         * Raises LibBase.NotFound if not found.
	 *)

    val numItems: 'a map ->  int
	(* Return the number of items in the map *)

    val listItems : 'a map -> 'a list
    val listItemsi: 'a map -> (Key.ord_key * 'a) list
	(* Return an ordered list of the items (and their keys) in the map.
         *)

    val collate: ('a * 'a -> order) -> ('a map * 'a map) -> order
	(* given an ordering on the map's range, return an ordering
	 * on the map.
	 *)

    val unionWith : ('a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
    val unionWithi: (Key.ord_key * 'a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
	(* return a map whose domain is the union of the domains of the two input
	 * maps, using the supplied function to define the map on elements that
	 * are in both domains.
	 *)

    val intersectWith : ('a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
    val intersectWithi: (Key.ord_key * 'a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
	(* return a map whose domain is the intersection of the domains of the
	 * two input maps, using the supplied function to define the range.
	 *)

    val app : ('a -> unit) -> 'a map -> unit
    val appi: ((Key.ord_key * 'a) -> unit) -> 'a map -> unit
	(* Apply a function to the entries of the map in map order. *)

    val map : ('a -> 'b) -> 'a map -> 'b map
    val mapi: (Key.ord_key * 'a -> 'b) -> 'a map -> 'b map
	(* Create a new map by applying a map function to the
         * name/value pairs in the map.
         *)

    val foldl : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldli: (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
	(* Apply a folding function to the entries of the map
         * in increasing map order.
         *)

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldri: (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
	(* Apply a folding function to the entries of the map
         * in decreasing map order.
         *)

    val filter : ('a -> bool) -> 'a map -> 'a map
    val filteri: (Key.ord_key * 'a -> bool) -> 'a map -> 'a map
	(* Filter out those elements of the map that do not satisfy the
	 * predicate.  The filtering is done in increasing map order.
	 *)

    val mapPartial : ('a -> 'b option) -> 'a map -> 'b map
    val mapPartiali: (Key.ord_key * 'a -> 'b option) -> 'a map -> 'b map
	(* map a partial function over the elements of a map in increasing
	 * map order.
	 *)

  end (* ORD_MAP *)
