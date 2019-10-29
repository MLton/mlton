(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure HashSet: HASH_SET =
struct

datatype 'a t =
   T of {buckets: 'a list array ref,
         hash: 'a -> word,
         mask: word ref,
         numItems: int ref}

fun 'a newWithBuckets {hash, numBuckets: int}: 'a t =
   let
      val mask: word = Word.fromInt numBuckets - 0w1
   in
      T {buckets = ref (Array.new (numBuckets, [])),
         hash = hash,
         numItems = ref 0,
         mask = ref mask}
   end

val initialSize: int = Int.pow (2, 6)

fun new {hash} = newWithBuckets {hash = hash,
                                 numBuckets = initialSize}

fun newOfSize {hash, size} =
   newWithBuckets {hash = hash,
                   numBuckets = 4 * Int.roundUpToPowerOfTwo size}

fun size (T {numItems, ...}) = !numItems

fun index (w: word, mask: word): int =
   Word.toInt (Word.andb (w, mask))

val numPeeks: Int64.int ref = ref 0
val numLinks: Int64.int ref = ref 0

fun stats () =
   let open Layout
   in align
      [seq [str "hash set numPeeks = ", str (Int64.toString (!numPeeks))],
       (* seq [str "hash set numLinks = ", str (Int64.toString (!numLinks))], *)
       seq [str "hash set average position = ",
            str let open Real
                    val fromInt = fromIntInf o Int64.toLarge
                in format (fromInt (!numLinks) / fromInt (!numPeeks),
                           Format.fix (SOME 3))
                end]]
   end

fun stats' (T {buckets, numItems, ...}) =
   let open Layout
       val numi = !numItems
       val numb = Array.length (!buckets)
       val numb' = numb - 1
       val avg = let open Real in (fromInt numi / fromInt numb) end
       val (min,max,total)
         = Array.fold
           (!buckets,
            (NONE, NONE, 0.0),
            fn (l,(min,max,total)) 
             => let
                  val n = List.length l
                  val d = (Real.fromInt n) - avg
                in
                  (SOME (Option.fold(min,n,Int.min)),
                   SOME (Option.fold(max,n,Int.max)),
                   total + d * d)
                end)
       val stdd = let open Real in Math.sqrt(total / (fromInt numb')) end
       val rfmt = fn r => Real.format (r, Real.Format.fix (SOME 3))
   in align
      [seq [str "numItems = ", Int.layout numi],
       seq [str "numBuckets = ", Int.layout numb],
       seq [str "avg = ", str (rfmt avg),
            str " stdd = ", str (rfmt stdd),
            str " min = ", Option.layout Int.layout min,
            str " max = ", Option.layout Int.layout max]]
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

fun removeAll (T {buckets, numItems, ...}, p) =
   Array.modify (!buckets, fn elts =>
                 List.fold (elts, [], fn (a, ac) =>
                            if p a
                               then (Int.dec numItems; ac)
                            else a :: ac))

fun remove (T {buckets, mask, numItems, ...}, w, p) =
   let
      val i = index (w, !mask)
      val b = !buckets
      val _ = Array.update (b, i, List.removeFirst (Array.sub (b, i), p))
      val _ = Int.dec numItems
   in
      ()
   end

fun peekGen (T {buckets = ref buckets, mask, ...}, w, p, no, yes) =
   let
      val _ =
         numPeeks := 1 + !numPeeks
         handle Overflow => Error.bug "HashSet: numPeeks overflow"
      val j = index (w, !mask)
      val b = Array.sub (buckets, j)
      fun update () =
         numLinks := !numLinks + 1
         handle Overflow => Error.bug "HashSet: numLinks overflow"
   in case List.peek (b, fn a => (update (); p a)) of
      NONE => no (j, b)
    | SOME a => yes a
   end

fun peek (t, w, p) = peekGen (t, w, p, fn _ => NONE, SOME)

(* fun update (T {buckets = ref buckets, equals, hash, mask, ...}, a) =
 *    let
 *       val j = index (hash a, !mask)
 *       val _ =
 *       Array.update (buckets, j,
 *                     a :: (List.remove (Array.sub (buckets, j),
 *                                        fn a' => equals (a, a'))))
 *    in ()
 *    end
 *)

fun insertIfNew (table as T {buckets, numItems, ...}, w, p, f, 
                 g: 'a -> unit) =
   let
      fun no (j, b) =
         let val a = f ()
            val _ = Int.inc numItems
            val _ = Array.update (!buckets, j, a :: b)
            val _ = maybeGrow table
         in a
         end
      fun yes x = (g x; x)
   in peekGen (table, w, p, no, yes)
   end

fun lookupOrInsert (table, w, p, f) =
   insertIfNew (table, w, p, f, ignore)

fun fold (T {buckets, ...}, b, f) =
   Array.fold (!buckets, b, fn (r, b) => List.fold (r, b, f))

local
   structure F = Fold (type 'a t = 'a t
                       type 'a elt = 'a
                       val fold = fold)
   open F
in
   val foreach = foreach
end

fun forall (T {buckets, ...}, f) =
   Array.forall (!buckets, fn r => List.forall (r, f))

fun toList t = fold (t, [], fn (a, l) => a :: l)

fun layout lay t = List.layout lay (toList t)

fun fromList (l, {hash, equals}) =
   let
      val s = new {hash = hash}
      val () =
         List.foreach (l, fn a =>
                       ignore (lookupOrInsert (s, hash a,
                                               fn b => equals (a, b),
                                               fn _ => a)))
   in
      s
   end

end
