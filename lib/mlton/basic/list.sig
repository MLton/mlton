(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LIST =
   sig
      type 'a t = 'a list

      val allButLast: 'a t -> 'a t
      val append: 'a t * 'a t -> 'a t
      (* appendMap (l1, f, l2) = append (map (l1, f), l2) *)
      val appendMap: 'a t * ('a -> 'b) * 'b t -> 'b t
      (* appendRev (l1, l2) = append (rev l1, l2) *)
      val appendRev: 'a t * 'a t -> 'a t
      val compare: 'a t * 'a t * ('a * 'a -> order) -> order
      (* concat [[1, 2], [3, 4, 5], [6]] = [1, 2, 3, 4, 5, 6] *)
      val concat: 'a t t -> 'a t
      (* concatMap = concat o map *)
      val concatMap: 'a t * ('a -> 'b t) -> 'b t
      (* concatRev = concat o rev *)
      val concatRev: 'a t t -> 'a t
      val cons: 'a * 'a t -> 'a t
      val contains: 'a t * 'a * ('a * 'a -> bool) -> bool
      (*   cross [[1, 2, 3], [4, 5], [6, 7]] =
       * [[1, 4, 6], [1, 4, 7], [1, 5, 6], [1, 5, 7],
       *  [2, 4, 6], [2, 4, 7], [2, 5, 6], [2, 5, 7],
       *  [3, 4, 6], [3, 4, 7], [3, 5, 6], [3, 5, 7]]
       *)
      val cross: 'a t t -> 'a t t
      val dropPrefix: 'a t * int -> 'a t
      val dropSuffix: 'a t * int -> 'a t
      (* duplicate (3, f) = [f (), f (), f ()] *)
      val duplicate: int * (unit -> 'a) -> 'a t
      (* Extract each element paired with the elements before it
       * and after it in the same order as in the list.
       *)
(*      val each: 'a t -> ('a t * 'a * 'a t) t *)
      val equals: 'a t * 'b t * ('a * 'b -> bool) -> bool
      val equalsAsSet: 'a t * 'a t * ('a * 'a -> bool) -> bool
      (* Group according to equivalence relation. *)
      val equivalence: 'a t * ('a * 'a -> bool) -> 'a t t
      val exists: 'a t * ('a -> bool) -> bool
      val first: 'a t -> 'a
      val firstN: 'a t * int -> 'a t
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val fold2: 'a t * 'b t * 'c * ('a * 'b * 'c -> 'c) -> 'c
      val fold3: 'a t * 'b t * 'c t * 'd * ('a * 'b * 'c * 'd -> 'd) -> 'd
      val foldi: 'a t * 'b * (int * 'a * 'b -> 'b) -> 'b
      val foldr: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foldr2: 'a t * 'b t * 'c * ('a * 'b * 'c -> 'c) -> 'c
      val forall: 'a t * ('a -> bool) -> bool
      val forall2: 'a t * 'b t * ('a * 'b -> bool) -> bool
      val foralli: 'a t * (int * 'a -> bool) -> bool
      val foreach: 'a t * ('a -> unit) -> unit
      val foreach2: 'a t * 'b t * ('a * 'b -> unit) -> unit
(*      val foreach3: 'a t * 'b t * 'c t * ('a * 'b * 'c -> unit) -> unit *)
      val foreachi: 'a t * (int * 'a -> unit) -> unit
      val index: 'a t * ('a -> bool) -> int option
      (* Insert an element in a list sorted in increasing order.
       * Return the original list if the the element is already there.
       *)
      val insert: 'a t * 'a * ('a * 'a -> bool) -> 'a t
      val insertionSort: 'a t * ('a * 'a -> bool) -> 'a t
      val isEmpty: 'a t -> bool
      val isPrefix: 'a t * 'a t * ('a * 'a -> bool) -> bool
      val keepAll: 'a t * ('a -> bool) -> 'a t
      val keepAllMap: 'a t * ('a -> 'b option) -> 'b t
      val last: 'a t -> 'a
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      (* Specify a string to separate the elements *)
(*      val layoutSep: string * ('a -> Layout.t) -> 'a t -> Layout.t *)
      val length: 'a t -> int
      val map: 'a t * ('a -> 'b) -> 'b t
      val mapi: 'a t * (int * 'a -> 'b) -> 'b t
      val map2: 'a t * 'b t * ('a * 'b -> 'c) -> 'c t
      val map3: 'a t * 'b t * 'c t * ('a * 'b * 'c -> 'd) -> 'd t
(*      val map4: 'a t * 'b t * 'c t * 'd t * ('a * 'b * 'c * 'd -> 'e) -> 'e t *)
      val new: int * 'a -> 'a t
      val nth: 'a t * int -> 'a
      val nthTail: 'a t * int -> 'a t
(*       val ordered :
 *       {< : 'a * 'a -> bool}
 *       -> {insert: 'a t * 'a -> 'a t,
 *           insertionSort: 'a t  -> 'a t,
 *           median: 'a t -> 'a,
 *           orderStatistic: 'a t * int -> 'a,
 *           partition: 'a t * 'a -> 'a t * 'a t,
 *           max: 'a t -> 'a,
 *           min: 'a t -> 'a,
 *           largest: 'a t * int -> 'a t,
 *           smallest: 'a t * int -> 'a t}
 *)
      (* partition ([1, 2, 3, 4], isOdd) = {no = [4, 2], yes = [3, 1]} *)
      val partition: 'a t * ('a -> bool) -> {no: 'a t, yes: 'a t}
      val peek: 'a t * ('a -> bool) -> 'a option
      val peeki: 'a t * (int * 'a -> bool) -> (int * 'a) option
      val peekMap: 'a t * ('a -> 'b option) -> 'b option
      (* Removes first element if it exists.
       * Returns NONE if pred is false on everything.
       *)
(*      val peekRemove: 'a t * ('a -> bool) -> ('a t * 'a) option *)
      val pop: 'a t ref -> 'a
      val power: 'a t -> 'a t t
(*      val prefixes: 'a t -> 'a t t*)
      val push: 'a t ref * 'a -> unit
      (* Removes first element on which predicate is true.
       * Returns original list if predicate is false on every elt.
       *)
      val remove: 'a t * ('a -> bool) -> 'a t
      (* removeAll (l, f) removes all x in l such that f x and reverses order. *)
      val removeAll: 'a t * ('a -> bool) -> 'a t
      val removeCommonPrefix: 'a t * 'a t * ('a * 'a -> bool) -> 'a t * 'a t
      val removeDuplicates: 'a t * ('a * 'a -> bool) -> 'a t
      (* Error if predicate isn't true on some element. *)
      val removeFirst: 'a t * ('a -> bool) -> 'a t
      val removePrefix: 'a t * ('a -> bool) -> 'a t
      val rev: 'a t -> 'a t
      (* The "rev" versions of functions are there for efficiency, when it is
       * easier to fold over the input and accumulate the result in reverse.
       *)
      val revMap: 'a t * ('a -> 'b) -> 'b t
      val revKeepAll: 'a t * ('a -> bool) -> 'a t
      val revKeepAllMap: 'a t * ('a -> 'b option) -> 'b t
      val revRemoveAll: 'a t * ('a -> bool) -> 'a t
      val separate: 'a t * 'a -> 'a t
      val set: {equals: 'a * 'a -> bool,
                layout: 'a -> Layout.t}
         -> {empty: 'a t,
             singleton: 'a -> 'a t,
             size: 'a t -> int,
             equals: 'a t * 'a t -> bool,
             <= : 'a t * 'a t -> bool,
             >= : 'a t * 'a t -> bool,
             < : 'a t * 'a t -> bool,
             > : 'a t * 'a t -> bool,
             + : 'a t * 'a t -> 'a t,
             - : 'a t * 'a t -> 'a t,
             intersect: 'a t * 'a t -> 'a t,
             unions: 'a t t -> 'a t,
             add: 'a t * 'a -> 'a t,
             remove: 'a t * 'a -> 'a t,
             contains: 'a t * 'a -> bool,
             areDisjoint: 'a t * 'a t -> bool,
             subset: 'a t * ('a -> bool) -> 'a t,
             subsetSize: 'a t * ('a -> bool) -> int,
             replace: 'a t * ('a -> 'a option) -> 'a t,
             map: 'a t * ('a -> 'a) -> 'a t,
             layout: 'a t -> Layout.t}
      val snoc: 'a t * 'a -> 'a t
(*      val splitAtMost: 'a t * int -> ('a t * 'a t) option *)
      val splitAt: 'a t * int -> 'a t * 'a t
      val splitLast: 'a t -> 'a t * 'a
      val splitPrefix: 'a t * ('a -> bool) -> 'a t * 'a t
(*      val suffixes: 'a t -> 'a t t *)
      (* subsets (l, n) = subsets of exactly size n *)
      val subsets: 'a t * int -> 'a t t
      val tabulate: int * (int -> 'a) -> 'a t
(*      val tail: 'a t -> 'a t *)
      val toString: ('a -> string) -> 'a t -> string
      val unfold: 'a * ('a -> ('b * 'a) option) -> 'b t
      val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b t
      val unfoldr: 'a * ('a -> ('b * 'a) option) -> 'b t
      val unfoldri: int * 'a * (int * 'a -> 'b * 'a) -> 'b t
      val union: 'a t * 'a t * ('a * 'a -> bool) -> 'a t
      val unzip: ('a * 'b) t -> 'a t * 'b t
(*      val unzip3: ('a * 'b * 'c) t -> 'a t * 'b t * 'c t *)
      val zip: 'a t * 'b t -> ('a * 'b) t
(*      val zip3: 'a t * 'b t * 'c t -> ('a * 'b * 'c) t *)
   end 


functor TestList (S: LIST): sig end =
struct

val _ = print "TestList\n"

open S

val _ =
   Assert.assert
   ("TestList", fn () =>
    SOME true = peekMap ([1, 2, 3], fn x => if x = 2 then SOME true else NONE)
    andalso ([2], [3]) = removeCommonPrefix ([1, 2], [1, 3], op =)
    andalso [2, 4, 6] = keepAll ([1, 2, 3, 4, 5, 6], fn x => 0 = x mod 2))

end
