type int = Int.t
type word = Word.t

signature INSERTION_SORT =
   sig
      (* The comparison function ('a * 'a -> bool) for should be the <= funtion,
       * not just <.
       * This is necessary to handle duplicate elements.
       *)
      val sort: 'a array * ('a * 'a -> bool) -> unit
   end
