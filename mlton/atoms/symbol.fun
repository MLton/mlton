(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Symbol (S: SYMBOL_STRUCTS): SYMBOL = 
struct

open S

datatype t = T of {name: string,
                   plist: PropertyList.t}

local
   fun make f (T r) = f r
in
   val plist = make #plist
   val name = make #name
end

val table: (string, t) HashTable.t =
   HashTable.new {equals = String.equals, hash = String.hash}

fun fromString s =
   HashTable.lookupOrInsert
   (table, s, fn () =>
    T {name = s, 
       plist = PropertyList.new ()})

fun foreach f = HashTable.foreach (table, f)

val toString = name

val layout = Layout.str o toString

fun equals (s, s') = PropertyList.equals (plist s, plist s')

local
   fun make f (s, s') = f (name s, name s')
in
   val op <= = make String.<=
   val compare = make String.compare
end

val asterisk = fromString "*"
val bogus = fromString "<bogus>"
val equal = fromString "="
val itt = fromString "it"
val unit = fromString "unit"

end
