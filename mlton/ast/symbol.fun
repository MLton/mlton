functor Symbol (S: SYMBOL_STRUCTS): SYMBOL = 
struct

open S

datatype t = T of {counter: Counter.t,
		   hash: word,
		   name: string,
		   plist: PropertyList.t}

local
   fun make f (T r) = f r
in
   val counter = make #counter
   val hash = make #hash
   val plist = make #plist
   val name = make #name
end

val table: t HashSet.t = HashSet.new {hash = hash}

fun fromString s =
   let
      val hash = String.hash s
   in
      HashSet.lookupOrInsert
      (table, hash, fn T {name, ...} => s = name,
       fn () => T {counter = Counter.new 0,
		   hash = hash,
		   name = s,
		   plist = PropertyList.new ()})
   end

fun foreach f = HashSet.foreach (table, f)

val toString = name

val layout = Layout.str o toString

fun equals (s, s') = PropertyList.equals (plist s, plist s')
   
local
   fun make f (s, s') = f (name s, name s')
in
   val op <= = make String.<=
   val compare = make String.compare
end

fun uniqueString (s: t): string =
   concat [toString s, "_", Int.toString (Counter.next (counter s))]

val asterisk = fromString "*"
val bogus = fromString "<bogus>"
val equal = fromString "="
val itt = fromString "it"
val unit = fromString "unit"

end
