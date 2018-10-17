(* Copyright (C) 2017 Jason Carr
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* This code is not working -- it is not even in sources.cm *)
structure HashTable: HASH_TABLE =
struct

structure Set = HashSet

datatype ('a, 'b) t = T of {set: ('a * 'b) Set.t,
                        hash: 'a -> word,
                        equals: 'a * 'a -> bool}

fun ('a, 'b) new {equals, hash}: ('a, 'b) t =
   {set=Set.new { hash = hash o #1}, hash=hash, equals=equals}
            (* equals = fn ((a, _), (a', _)) => equals (a, a') *)

local
   open Set
   fun projectSet (T {set, ...}) = set
in
   val size = size o projectSet
   val stats = stats o projectSet
   val toList = toList o projectSet
   val layout = layout o projectSet
   val removeAll = removeAll o projectSet
   val toList = toList o projectSet
end

fun keyEquals (equals, a) (a', _) = equals

fun peek (T {set, hash, equals}, a) =
   Option.map (Set.peek (set, hash a, keyEquals (equals, a)), #2)

fun lookupOrInsert (T {set, hash, equals}, a, genB) =
   #2 o Set.lookupOrInsert (set, hash a, keyEquals (equals, a), fn () => (a, genB ()))

fun insertIfNew (T {set, hash, equals}, a, genB, whenFound) =
   #2 o Set.insertIfNew
      (set, hash a, keyEquals (equals, a),
       fn () => (a, genB ()),
       fn (_, b) => whenFound b)

fun remove (T {set, hash, equals}, a) = Set.remove (set, hash a, keyEquals (equals, a))

end
