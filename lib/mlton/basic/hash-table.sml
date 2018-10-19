(* Copyright (C) 2017 Jason Carr
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
structure HashTable: HASH_TABLE =
struct

structure Set = HashSet

datatype ('a, 'b) t = T of {set: ('a * 'b) Set.t,
                        hash: 'a -> word,
                        equals: 'a * 'a -> bool}

fun ('a, 'b) new {equals, hash}: ('a, 'b) t =
   T {set=Set.new { hash = hash o #1}, hash=hash, equals=equals}


(* we'd like to factor these apart but it makes generalization difficult *)
fun size (T {set, ...}) = Set.size set
fun toList (T {set, ...}) = Set.toList set
fun layout f (T {set, ...}) = Set.layout f set
fun stats' (T {set, ...}) = Set.stats' set

fun keyEquals (equals, a) (a', _) = equals (a, a')

fun peek (T {set, hash, equals}, a) =
   Option.map (Set.peek (set, hash a, keyEquals (equals, a)), #2)

fun lookupOrInsert (T {set, hash, equals}, a, genB) =
   (#2) (Set.lookupOrInsert (set, hash a, keyEquals (equals, a), fn () => (a, genB ())))

fun insertIfNew (T {set, hash, equals}, a, genB, whenFound) =
   (#2) (Set.insertIfNew
      (set, hash a, keyEquals (equals, a),
       fn () => (a, genB ()),
       fn (_, b) => whenFound b))

fun remove (T {set, hash, equals}, a) = Set.remove (set, hash a, keyEquals (equals, a))
fun removeAll (T {set, ...}, f) = Set.removeAll (set, f)



end
