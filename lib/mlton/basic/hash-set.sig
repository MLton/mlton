type int = Int.t
type word = Word.t

signature HASH_SET =
   sig
      type 'a t

      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val forall: 'a t * ('a -> bool) -> bool
      val foreach: 'a t * ('a -> unit) -> unit
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      (* lookupOrInsert (s, h, p, f)  looks in the set s for an entry with hash h
       * satisfying predicate p.  If the entry is there, it is returned.
       * Otherwise, the function f is called to create a new entry, which is
       * inserted and returned.
       *)
      val lookupOrInsert: 'a t * word * ('a -> bool) * (unit -> 'a) -> 'a
      val new: {hash: 'a -> word} -> 'a t
      val peek: 'a t * word * ('a -> bool) -> 'a option
      (* remove an entry.  Error if it's not there. *)
      val remove: 'a t * word * ('a -> bool) -> unit
      (* removeAll (s, p) removes all entries from s that satisfy predicate p. *)
      val removeAll: 'a t * ('a -> bool) -> unit
      val size: 'a t -> int
      val stats: unit -> Layout.t
      val toList: 'a t -> 'a list
   end


functor TestHashSet (S: HASH_SET): sig end =
struct

open S

val _ = Assert.assert("HashSet", fn () => true)

end
