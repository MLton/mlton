signature MERGE_SORT =
   sig
      (* The comparison function ('a * 'a -> bool) for any of these should
       * always be the <= funtion, not just <
       * This is necessary to handle lists with duplicate elements.
       *)
      val make: ('a * 'a -> bool) -> {isSorted: 'a list -> bool,
				      merge: 'a list * 'a list -> 'a list,
				      sort: 'a list -> 'a list}
      val merge: 'a list * 'a list * ('a * 'a -> bool) -> 'a list
      val sort: 'a list * ('a * 'a -> bool) -> 'a list
   end
