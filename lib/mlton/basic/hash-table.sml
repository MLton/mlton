(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure HashTable: HASH_TABLE =
struct

datatype ('a, 'b) t =
   T of {numItems: int ref,
	 mask: Word.t ref,
	 equals: 'a * 'a -> bool,
	 buckets: (Word.t * 'a * 'b) list array ref}

val initialSize: int = Int.^(2, 6)
val initialMask: word = Word.fromInt initialSize - 0w1

fun ('a, 'b) new(equals: 'a * 'a -> bool): ('a, 'b) t =
   T{numItems = ref 0,
     mask = ref initialMask,
     equals = equals,
     buckets = ref(Array.new(initialSize, []))}

fun numItems(T{numItems, ...}) = !numItems
fun numBuckets(T{buckets, ...}) = Array.length(!buckets)

fun index(w: Word.t, mask: Word.t): int =
   Word.toInt(Word.andb(w, mask))
   
val numPeeks: int ref = ref 0
val numLinks: int ref = ref 0
val maxLength: int ref = ref 0

fun stats() =
   let open Layout
   in align
      [seq[str "numPeeks = ", Int.layout(!numPeeks)],
       seq[str "average position in bucket = ",
	   str let open Real
	       in format(fromInt(!numLinks) / fromInt(!numPeeks),
			 Format.fix(SOME 3))
	       end]]
   end

fun maybeGrow(T{buckets = bucketsRef, mask, numItems, ...}): unit =
   let
      val buckets = !bucketsRef
      val n = Array.length buckets
      val tooLargeFactor = 4
   in if !numItems * tooLargeFactor > n
	 then let
		 val growFactor = 2
		 val newBuckets = Array.new(n * growFactor, [])
		 (* m depends on growFactor being 2 *)
		 val m = Word.orb(0w1, Word.<<(!mask, 0w1))
	      in Array.foreach
		 (buckets, fn r =>
		  List.foreach(r, fn (w, i, x) =>
			       let val j = index (w, m)
			       in Array.update
				  (newBuckets, j,
				   (w, i, x) :: Array.sub (newBuckets, j))
			       end))
		 ; bucketsRef := newBuckets
		 ; mask := m
	      end
      else ()
   end

fun peekGen(t as T {buckets = ref buckets, equals, mask, ...}, w, i, no, yes) =
   let
      val _ = Int.inc numPeeks
      val j = index (w, !mask)
      val b = Array.sub (buckets, j)
   in case List.peek (b, fn (w', i', _) =>
		      (Int.inc numLinks
		       ; Word.equals (w, w') andalso equals (i, i'))) of
      NONE => no (j, b)
    | SOME (_, _, x) => yes x
   end

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

