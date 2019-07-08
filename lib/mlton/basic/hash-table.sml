(* Copyright (C) 2018-2019 Jason Carr, Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
structure HashTable:> HASH_TABLE =
struct

structure Set = HashSet

datatype ('a, 'b) t = T of {set: {hash: word, key: 'a, value: 'b} Set.t,
                            hash: 'a -> word,
                            equals: 'a -> 'a -> bool}

fun ('a, 'b) new {equals, hash}: ('a, 'b) t =
   T {set = Set.new {hash = #hash},
      hash = hash,
      equals = fn x => fn y => equals (x, y)}

fun toPair {key, value, hash} = (key, value)
(* we'd like to factor these apart but it makes generalization difficult *)
fun size (T {set, ...}) = Set.size set
fun toList (T {set, ...}) = List.map (Set.toList set, toPair)
fun layout f (T {set, ...}) = Set.layout (f o toPair) set
fun stats' (T {set, ...}) = Set.stats' set

fun peek (T {set, hash, equals, ...}, a) =
   Option.map (Set.peek (set, hash a, equals a o #key), #value)

fun lookupOrInsert (T {set, hash, equals}, a, genB) =
   let
      val hash = hash a
   in
      (#value o Set.lookupOrInsert)
      (set, hash, equals a o #key,
       fn () => {hash = hash, key = a, value = genB ()})
   end

fun insertIfNew (T {set, hash, equals}, a, genB, whenFound) =
   let
      val hash = hash a
   in
      (#value o Set.insertIfNew)
      (set, hash, equals a o #key,
       fn () => {hash = hash, key = a, value = genB ()},
       fn {value = b, ...} => whenFound b)
   end

fun removeWhen (T {set, hash, equals, ...}, a, cond) =
   Set.remove (set, hash a,
               fn {key, value, ...} => cond value andalso equals key a)
fun remove (t, a) = removeWhen (t, a, fn _ => true)
fun removeAll (T {set, ...}, f) = Set.removeAll (set, f o toPair)

fun foldi (T {set, ...}, b, f) =
   Set.fold (set, b, fn ({key, value, ...}, b) =>
             f (key, value, b))
fun fold (t, b, f) =
   foldi (t, b, fn (_, v, b) => f (v, b))
fun foreachi (T {set, ...}, f) =
   Set.foreach (set, fn {key, value, ...} => f (key, value))
fun foreach (t, f) =
   foreachi (t, fn (_, v) => f v)

end
