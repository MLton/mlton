structure HashSet: HASH_SET =
struct

datatype 'a t =
   T of {buckets: 'a list array ref,
	 hash: 'a -> word,
	 mask: word ref,
	 numItems: int ref}

val initialSize: int = Int.^ (2, 6)
val initialMask: word = Word.fromInt initialSize - 0w1

fun 'a new {hash}: 'a t =
   T {buckets = ref (Array.new (initialSize, [])),
      hash = hash,
      numItems = ref 0,
      mask = ref initialMask}

fun size (T {numItems, ...}) = !numItems
fun numBuckets (T {buckets, ...}) = Array.length (!buckets)

fun index (w: word, mask: word): int =
   Word.toInt (Word.andb (w, mask))
   
val numPeeks: int ref = ref 0
val numLinks: int ref = ref 0

fun stats () =
   let open Layout
   in align
      [seq [str "numPeeks = ", Int.layout (!numPeeks)],
       seq [str "average position in bucket = ",
	    str let open Real
		in format (fromInt (!numLinks) / fromInt (!numPeeks),
			   Format.fix (SOME 3))
		end]]
   end

fun resize (T {buckets, hash, mask, ...}, size: int, newMask: word): unit =
   let
      val newBuckets = Array.new (size, [])
   in Array.foreach (!buckets, fn r =>
		     List.foreach (r, fn a =>
				   let val j = index (hash a, newMask)
				   in Array.update
				      (newBuckets, j,
				       a :: Array.sub (newBuckets, j))
				   end))
      ; buckets := newBuckets
      ; mask := newMask
   end
   	       
fun maybeGrow (s as T {buckets, mask, numItems, ...}): unit =
   let
      val n = Array.length (!buckets)
   in if !numItems * 4 > n
	 then resize (s,
		      n * 2,
		      (* The new mask depends on growFactor being 2. *)
		      Word.orb (0w1, Word.<< (!mask, 0w1)))
      else ()
   end

(* Shrink the table so it is more than 1/8th full. *)
fun maybeShrink (s as T {buckets, mask, numItems, ...}): unit =
   let
      val n = Array.length (!buckets)
      val maxSize = Int.max (!numItems * 8, initialSize)
      fun loop (n, shiftAmount) =
	 if n <= maxSize
	    then (n, shiftAmount)
	 else loop (Int.quot (n, 2), 1 + shiftAmount)
      val (n', shiftAmount) = loop (n, 0)
   in
      if n = n'
	 then ()
      else resize (s, n', Word.>> (!mask, Word.fromInt shiftAmount))
   end

fun remove (T {buckets, hash, mask, numItems}, p) =
   Array.modify (!buckets, fn elts =>
		 List.fold (elts, [], fn (a, ac) =>
			    if p a
			       then (Int.dec numItems; ac)
			    else a :: ac))
   (* maybe shrink *)


fun peekGen (T {buckets = ref buckets, mask, ...}, w, p, no, yes) =
   let
      val _ = Int.inc numPeeks
      val j = index (w, !mask)
      val b = Array.sub (buckets, j)
   in case List.peek (b, fn a => (Int.inc numLinks; p a)) of
      NONE => no (j, b)
    | SOME a => yes a
   end

fun peek (t, w, p) = peekGen (t, w, p, fn _ => NONE, SOME)

(* fun update (T {buckets = ref buckets, equals, hash, mask, ...}, a) =
 *    let
 *       val j = index (hash a, !mask)
 *       val _ =
 * 	 Array.update (buckets, j,
 * 		       a :: (List.remove (Array.sub (buckets, j),
 * 					  fn a' => equals (a, a'))))
 *    in ()
 *    end
 *)

fun lookupOrInsert (table as T {buckets, numItems, ...}, w, p, f) =
   let
      fun no (j, b) =
	 let val a = f ()
	    val _ = Int.inc numItems
	    val _ = Array.update (!buckets, j, a :: b)
	    val _ = maybeGrow table
	 in a
	 end
   in peekGen (table, w, p, no, fn x => x)
   end

fun fold (T {buckets, ...}, b, f) =
   Array.fold (!buckets, b, fn (r, b) => List.fold (r, b, f))

local
   structure F = Fold (type 'a t = 'a t
		       type 'a elt = 'a
		       val fold = fold)
   open F
in
   val forall = forall
   val foreach = foreach
end

fun toList t = fold (t, [], fn (a, l) => a :: l)

fun layout lay t = List.layout lay (toList t)

end
