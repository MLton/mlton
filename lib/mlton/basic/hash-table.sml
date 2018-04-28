(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* This code is not working -- it is not even in sources.cm *)
structure HashTable: HASH_TABLE =
struct

structure Set = HashSet

type ('a, 'b) t = ('a * 'b) Set.t

fun ('a, 'b) new {equals, hash}: ('a, 'b) t =
   Set.new {equals = fn ((a, _), (a', _)) => equals (a, a')
            hash = hash o #1}

local
   open Set
in
   val size = size
   val stats = stats
end

fun update (t, a, b) = Set.update (t, (a, b))

fun peek (t, a) =
   Option.map (Set.peek (t, Set.hash (t, 

fun update (T {buckets = ref buckets, equals, mask, ...}, w, a, b) =
   let
      val j = index (w, !mask)
      val _ =
         Array.update
         (buckets, j,
          (w, a, b)
          :: List.fold (Array.sub (buckets, j), [], fn (z as (w', a', _), ac) =>
                        if Word.equals (w, w') andalso equals (a, a')
                           then ac
                        else z :: ac))
   in ()
   end

fun peek(t, w, i) = peekGen(t, w, i, fn _ => NONE, SOME)

fun lookupGen (table as T {buckets, numItems, ...}, w, i, x, yes) =
   let
      fun no (j, b) =
         let val x = x ()
            val _ = Int.inc numItems
            val _ = Array.update (!buckets, j, (w, i, x) :: b)
            val _ = maybeGrow table
         in x
         end
   in peekGen (table, w, i, no, yes)
   end

fun lookupOrInsert (table, w, i, x) =
   lookupGen (table, w, i, x, fn x => x)

fun insertIfNew (table, w, i, x, yes) =
   lookupGen (table, w, i, fn () => x, fn _ => yes ())

fun foldi(T{buckets, ...}, b, f) =
   Array.fold(!buckets, b, fn (r, b) =>
              List.fold(r, b, fn ((_, i, x), b) => f(i, x, b)))

fun listItemsi t = foldi(t, [], fn (i, x, l) => (i, x) :: l)

fun layout lay t = List.layout lay (listItemsi t)

fun fold(t, b, f) = foldi(t, b, fn (_, x, b) => f(x, b))

fun foreachi(t, f) = foldi(t, (), fn (i, x, ()) => f(i, x))

fun foreach(t, f) = foreachi(t, f o #2)

fun foralli(t, f) =
   DynamicWind.withEscape
   (fn escape =>
    (foreachi(t, fn z => if f z then () else escape false)
     ; true))

fun forall(t, f) = foralli(t, f o #2)

fun listItems t = fold(t, [], op ::)

fun appi(t, f) = foldi(t, (), fn (i, x, ()) => f(i, x))

fun app(t, f) = appi(t, f o #2)

fun mapi (T{numItems, mask, equals, buckets}, f) =
   T {numItems = ref (!numItems),
      mask = ref (!mask),
      equals = equals,
      buckets = ref (Array.map (!buckets, fn r =>
                                List.revMap (r, fn (w, i, x) =>
                                             (w, i, f (i, x)))))}

fun map (t, f) = mapi (t, f o #2)

end
