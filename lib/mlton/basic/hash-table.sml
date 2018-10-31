(* Copyright (C) 2018 Jason Carr
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
structure HashTable: HASH_TABLE =
struct

structure Set = HashSet

datatype ('a, 'b) t = T of {set: {hash: word, key: 'a, value: 'b} Set.t,
                        hash: 'a -> word,
                        equals: 'a * 'a -> bool,
                        cache: bool}

fun ('a, 'b) new {equals, hash, cache}: ('a, 'b) t =
   let
      val hash' = if cache then #hash else hash o #key
   in
      T {set=Set.new {hash = hash'}, hash=hash, equals=equals, cache=cache}
   end

fun toPair {key, value, hash} = (key, value)
(* we'd like to factor these apart but it makes generalization difficult *)
fun size (T {set, ...}) = Set.size set
fun toList (T {set, ...}) = List.map (Set.toList set, toPair)
fun layout f (T {set, ...}) = Set.layout (f o toPair) set
fun stats' (T {set, ...}) = Set.stats' set

fun keyEquals (equals, a) {key=a', hash, value} = equals (a, a')

fun peek (T {set, hash, equals, ...}, a) =
   Option.map (Set.peek (set, hash a, keyEquals (equals, a)), #value)

fun lookupOrInsert (T {set, hash, equals, cache}, a, genB) =
   let
      val hash = hash a
   in
   (#value) (Set.lookupOrInsert (set, hash, keyEquals (equals, a),
         fn () => {hash=if cache then hash else 0w0, key=a, value=genB ()}))
   end

fun insertIfNew (T {set, hash, equals, cache}, a, genB, whenFound) =
   let
      val hash = hash a
   in
   (#value) (Set.insertIfNew
      (set, hash, keyEquals (equals, a),
       fn () => {hash=if cache then hash else 0w0, key=a, value=genB ()},
       fn {value=b, ...} => whenFound b))
   end

fun removeWhen (T {set, hash, equals, ...}, a, cond) =
   Set.remove (set, hash a,
      fn {key,value,...} => cond value andalso equals (key, a))
fun remove (t, a) = removeWhen (t, a, fn _ => true)
fun removeAll (T {set, ...}, f) = Set.removeAll (set, f o toPair)

end
