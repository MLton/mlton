(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Vector (S: sig
                      include VECTOR_STRUCTS
                      val unsafeSub: 'a t * int -> 'a
                   end): VECTOR =
struct

open S

val size = length

fun unfold (n, a, f) = unfoldi (n, a, f o #2)

fun tabulate (n, f) = #1 (unfoldi (n, (), fn (i, ()) => (f i, ())))

fun fromArray a =
   tabulate (Pervasive.Array.length a, fn i => Pervasive.Array.sub (a, i))

fun toArray v =
   Pervasive.Array.tabulate (length v, fn i => sub (v, i))

datatype ('a, 'b) continue =
   Continue of 'a
  | Done of 'b

fun first v =
   let
      val n = length v
   in
      if n = 0
         then Error.bug "Vector.first"
      else unsafeSub (v, 0)
   end

fun fold' (v, start, b, f, g) =
   let
      val n = length v
      fun loop (i, b) =
         if i >= n
            then g b
         else
            case f (i, unsafeSub (v, i), b) of
               Continue b => loop (i + 1, b)
             | Done c => c
   in
      if 0 <= start andalso start <= n
         then loop (start, b)
      else Error.bug "Vector.fold'"
   end

fun foldFrom (v, start, b, f) =
   fold' (v, start, b,
          fn (_, a, b) => Continue (f (a, b)),
          fn b => b)

fun fold (a, b, f) = foldFrom (a, 0, b, f)

fun isEmpty a = 0 = length a

fun dropPrefix (v, n) = tabulate (length v - n, fn i => sub (v, i + n))

fun dropSuffix (v, n) = tabulate (length v - n, fn i => sub (v, i))

fun new (n, x) = tabulate (n, fn _ => x)

fun mapi (a, f) = tabulate (length a, fn i => f (i, unsafeSub (a, i)))

fun map (v, f) = mapi (v, f o #2)

fun copy v = map (v, fn x => x)

fun existsR (v, start, stop, f) =
   fold' (v, start, (),
          fn (i, a, ()) => if i = stop
                              then Done false
                           else if f a
                                   then Done true
                                else Continue (),
          fn _ => false)

fun foldi (v, b, f) = fold' (v, 0, b, Continue o f, fn b => b)

fun loopi (v, f, g) =
   fold' (v, 0, (),
          fn (i, a, ()) => (case f (i, a) of
                               NONE => Continue ()
                             | SOME b => Done b),
          g)

fun loop (v, f, g) = loopi (v, f o #2, g)

fun peekMapi (v, f) =
   let
      val n = length v
      fun loop i =
         if i = n
            then NONE
         else
            (case f (sub (v, i)) of
                NONE => loop (i + 1)
              | SOME b => SOME (i, b))
   in
      loop 0
   end

fun peekMap (v, f) =
   loop (v,
         fn a => (case f a of
                     NONE => NONE
                   | z => SOME z),
         fn () => NONE)

fun fromListMap (l, f) =
   let
      val r = ref l
   in
      tabulate (List.length l, fn _ =>
                case !r of
                   [] => Error.bug "Vector.fromListMap"
                 | x :: l => (r := l; f x))
   end

fun fromList l = fromListMap (l, fn x => x)

fun foldr2 (a, a', b, f) =
   let
      val n = length a
      val n' = length a'
      fun loop (i, b) =
         if i < 0
            then b
         else loop (i - 1, f (unsafeSub (a, i), unsafeSub (a', i), b))
   in
      if n = n'
         then loop (n - 1, b)
      else Error.bug "Vector.foldr2"
   end

fun foldi2From (a, a', start, b, f) =
   let
      val n = length a
      val n' = length a'
      fun loop (i, b) =
         if i >= n
            then b
         else loop (i + 1, f (i, unsafeSub (a, i), unsafeSub (a', i), b))
   in
      if n = n' andalso 0 <= start andalso start <= n 
         then loop (start, b)
      else Error.bug "Vector.foldi2From"
   end

fun foldi2 (a, a', b, f) = foldi2From (a, a', 0, b, f)

fun foreachi2 (v, v', f) =
   foldi2 (v, v', (), fn (i, x, x', ()) => f (i, x, x'))

fun fold2 (a, a', b, f) =
   foldi2 (a, a', b, fn (_, x, x', b) => f (x, x', b))

fun fold3From (a, a', a'', start, b, f) =
   let
      val n = length a
      val n' = length a'
      val n'' = length a''
      fun loop (i, b) =
         if i >= n
            then b
         else loop (i + 1, f (unsafeSub (a, i),
                              unsafeSub (a', i),
                              unsafeSub (a'', i),
                              b))
   in
      if n = n' andalso n = n'' andalso 0 <= start andalso start <= n
         then loop (start, b)
      else Error.bug "Vector.fold3From"
   end

fun fold3 (a, a', a'', b, f) = fold3From (a, a', a'', 0, b, f)

fun foreachR (v, start, stop, f: 'a -> unit) =
   if 0 <= start andalso start <= stop andalso stop <= length v
      then
         let
            fun step (i, a, ()) =
               if i >= stop
                  then Done ()
               else (f a; Continue ())
         in
            fold' (v, start, (), step, fn () => ())
         end
   else Error.bug "Vector.foreachR"

fun foreach2 (a, a', f) =
   fold2 (a, a', (), fn (x, x', ()) => f (x, x'))

fun forall2 (v, v', f) =
   let
      val n = length v
      fun loop i =
         i = n
         orelse (f (sub (v, i), sub (v', i))
                 andalso loop (i + 1))
   in
      if n = length v'
         then loop 0
      else Error.bug "Vector.forall2"
   end

fun foreach3 (v1, v2, v3, f: 'a * 'b * 'c -> unit) =
   let
      val n = length v1
      val _ =
         if n = length v2 andalso n = length v3
            then ()
         else Error.bug "Vector.foreach3"
      fun loop i =
         if i = n
            then ()
         else (f (sub (v1, i), sub (v2, i), sub (v3, i))
               ; loop (i + 1))
   in
      loop 0
   end

fun foreachi (a, f) = foldi (a, (), fn (i, x, ()) => f (i, x))

fun foreach (a, f) = foreachi (a, f o #2)

fun 'a peeki (v, f) =
   let
      val n = length v
      fun loop i =
         if i = n
            then NONE
         else let
                 val x = sub (v, i)
              in
                 if f (i, x)
                    then SOME (i, x)
                 else loop (i + 1)
              end
   in
      loop 0
   end

fun peek (a, f) = Option.map (peeki (a, f o #2), #2)

fun existsi (a, f) = isSome (peeki (a, f))

fun exists (a, f) = existsi (a, f o #2)

fun contains (v, a, f) = exists (v, fn a' => f (a, a'))

fun foralli (a, f) = not (existsi (a, not o f))

fun forall (a, f) = foralli (a, f o #2)

fun equals (a, a', equals) =
   length a = length a'
   andalso foralli (a, fn (i, x) => equals (x, unsafeSub (a', i)))

fun foldri (a, b, f) =
   Int.foldDown (0, length a, b, fn (i, b) => f (i, unsafeSub (a, i), b))

fun foldr (a, b, f) =
   foldri (a, b, fn (_, a, b) => f (a, b))

fun foreachri (a, f) = foldri (a, (), fn (i, x, ()) => f (i, x))

fun foreachr (a, f) = foreachri (a, f o #2)

fun toList a = foldr (a, [], op ::)

fun toListMap (a, f) = foldr (a, [], fn (a, ac) => f a :: ac)

fun layout l v = Layout.tuple (toListMap (v, l))

fun toString xToString l =
   Layout.toString (layout (Layout.str o xToString) l)

fun new0 () = tabulate (0, fn _ => Error.bug "Vector.new0")

fun new1 x = tabulate (1, fn _ => x)

fun new2 (x0, x1) = tabulate (2, fn 0 => x0 | 1 => x1 | _ => Error.bug "Vector.new2")

fun new3 (x0, x1, x2) =
   tabulate (3,
             fn 0 => x0
              | 1 => x1
              | 2 => x2
              | _ => Error.bug "Vector.new3")

fun new4 (x0, x1, x2, x3) =
   tabulate (4,
             fn 0 => x0
              | 1 => x1
              | 2 => x2
              | 3 => x3
              | _ => Error.bug "Vector.new4")

fun new5 (x0, x1, x2, x3, x4) =
   tabulate (5,
             fn 0 => x0
              | 1 => x1
              | 2 => x2
              | 3 => x3
              | 4 => x4
              | _ => Error.bug "Vector.new5")

fun new6 (x0, x1, x2, x3, x4, x5) =
   tabulate (6,
             fn 0 => x0
              | 1 => x1
              | 2 => x2
              | 3 => x3
              | 4 => x4
              | 5 => x5
              | _ => Error.bug "Vector.new6")

fun unzip (a: ('a * 'b) t) = (map (a, #1), map (a, #2))

fun unzip3 (a: ('a * 'b * 'c) t) = (map (a, #1), map (a, #2), map (a, #3))

fun rev v =
   let
      val n = length v
      val n1 = n - 1
   in
      tabulate (n, fn i => unsafeSub (v, n1 - i))
   end

fun fromListRev l = rev (fromList l)

fun mapAndFold (v, b, f) =
   let
      val r = ref b
      val v = map (v, fn x =>
                   let
                      val (c, b) = f (x, !r)
                      val _ = r := b
                   in c
                   end)
   in (v, !r)
   end

fun map2i (v, v', f) =
   let
      val n = length v
   in
      if n = length v'
         then tabulate (n, fn i => f (i, unsafeSub (v, i), unsafeSub (v', i)))
      else Error.bug "Vector.map2i"
   end

fun map2 (v, v', f) = map2i (v, v', fn (_, x, x') => f (x, x'))

fun map2AndFold (v, v', b, f) =
   let
      val r = ref b
      val v =
         map2 (v, v', fn (x, x') =>
               let
                  val (y, b) = f (x, x', !r)
                  val _ = r := b
               in y
               end)
   in (v, !r)
   end

fun map3 (v1, v2, v3, f) =
   let
      val n = length v1
   in
      if n = length v2 andalso n = length v3
         then tabulate (n, fn i => f (unsafeSub (v1, i),
                                      unsafeSub (v2, i),
                                      unsafeSub (v3, i)))
      else Error.bug "Vector.map3"
   end

fun zip (v, v') = map2 (v, v', fn z => z)

local
   fun doit (f, mapi) =
      let
         val n = ref 0
         val b = mapi (fn x =>
                       let
                          val b = f x
                          val _ = if isSome b then n := 1 + !n else ()
                       in b
                       end)
         val r = ref 0
         fun loop (i: int) =
            case unsafeSub (b, i) of
               NONE => loop (i + 1)
             | SOME b => (r := i + 1; b)
      in tabulate (!n, fn _ => loop (!r))
      end
in
   fun keepAllMapi (a, f) = doit (f, fn f => mapi (a, f))
   fun keepAllMap2i (a, b, f) = doit (f, fn f => map2i (a, b, f))
end

fun keepAllMap (v, f) = keepAllMapi (v, f o #2)

fun keepAllMap2 (v, v', f) = keepAllMap2i (v, v', fn (_, x, x') => f (x, x'))

fun keepAllSome v = keepAllMap (v, fn a => a)

fun keepAll (v, f) = keepAllMap (v, fn a => if f a then SOME a else NONE)

fun compare (v, v', comp) =
   let
      val n = length v
      val n' = length v'
   in
      Relation.lexico
      (Int.compare (n, n'), fn () =>
       let
          fun loop i =
             if i = n
                then EQUAL
             else 
                Relation.lexico
                (comp (unsafeSub (v, i), unsafeSub (v', i)), fn () =>
                 loop (i + 1))
       in
          loop 0
       end)
   end

fun toListRev v = fold (v, [], op ::)

fun last v =
   let
      val n = length v
   in
      if n = 0
         then Error.bug "Vector.last"
      else unsafeSub (v, n - 1)
   end

fun tabulator (n: int, f: ('a -> unit) -> unit) =
   let
      val a = Pervasive.Array.array (n, NONE)
      val r = ref 0
      val _ =
         f (fn x =>
            let
               val i = !r
            in
               if i >= n
                  then Error.bug "Vector.tabulator: too many elements"
               else (Pervasive.Array.update (a, i, SOME x)
                     ; r := i + 1)
            end)
   in
      if !r < n
         then Error.bug "Vector.tabulator: not enough elements"
      else tabulate (n, fn i => valOf (Pervasive.Array.sub (a, i)))
   end

fun 'a concat (vs: 'a t list): 'a t =
   case vs of
      [] => new0 ()
    | v :: vs' => 
         let
            val n = List.fold (vs, 0, fn (v, s) => s + length v)
         in
            #1 (unfold (n, (0, v, vs'),
                        let
                           fun loop (i, v, vs) =
                              if i < length v
                                 then (sub (v, i), (i + 1, v, vs))
                              else
                                 case vs of
                                    [] => Error.bug "Vector.concat"
                                  | v :: vs => loop (0, v, vs)
                        in loop
                        end))
         end

fun concatV vs =
   if 0 = length vs then
      new0 ()
   else
      let
         val n = fold (vs, 0, fn (v, s) => s + length v)
         fun state i = (i, sub (vs, i), 0)
      in
         #1 (unfold (n, state 0,
                     let
                        fun loop (i, v, j) =
                           if j < length v then
                              (sub (v, j), (i, v, j + 1))
                           else
                              loop (state (i + 1))
                     in loop
                     end))
      end

fun splitLast v =
   let
      val n = length v
   in
      if n <= 0
         then Error.bug "Vector.splitLast"
      else (tabulate (n - 1, fn i => unsafeSub (v, i)),
            unsafeSub (v, n - 1))
   end

fun isSortedRange (v: 'a t,
                   start: int,
                   stop: int,
                   le : 'a * 'a -> bool): bool =
   (Assert.assert
    ("Vector.isSortedRange", fn () =>
     0 <= start andalso start <= stop andalso stop <= length v)
    ; start = stop
      orelse
      let
         fun loop (i, prev) =
            i >= stop
            orelse let val cur = sub (v, i)
                   in
                      le (prev, cur)
                      andalso loop (i + 1, cur)
                   end
      in loop (start + 1, sub (v, start))
      end)

fun isSorted (v, op <=) = isSortedRange (v, 0, length v, op <=)

fun indexi (v, f) =
   fold' (v, 0, (),
          fn (i, a, _) => if f (i, a) then Done (SOME i) else Continue (),
          fn _ => NONE)

fun index (v, f) = indexi (v, f o #2)

fun indices (a: bool t): int t =
   keepAllMapi (a, fn (i, b) => if b then SOME i else NONE)

val indices =
   Trace.trace ("Vector.indices", layout Bool.layout, layout Int.layout)
   indices

fun isSubsequence (va, vb, f) =
   let
      val na = length va
      val nb = length vb
      fun loop (ia, ib) =
         ia >= na
         orelse let
                   val a = sub (va, ia)
                   fun loop' ib =
                      ib < nb
                      andalso if f (a, sub (vb, ib))
                                 then loop (ia + 1, ib + 1)
                              else loop' (ib + 1)
                in
                   loop' ib
                end
   in
      loop (0, 0)
   end

fun removeFirst (v, f) =
   let
      val seen = ref false
      val v = keepAll (v, fn a =>
                       not (f a)
                       orelse (!seen)
                       orelse (seen := true
                               ; false))
      val _ = if !seen then () else Error.bug "Vector.removeFirst"
   in
      v
   end

fun partitioni (v, f) =
   let
     val n = ref 0
     val v' = mapi (v, fn (i, x) =>
                    let
                      val b = f (i, x)
                      val _ = if b then n := 1 + !n else ()
                    in
                      (x,b)
                    end)
     val n = !n
     val r = ref 0
     fun loop b (i:int) =
       case unsafeSub (v', i) of
         (x, b') => if b = b' 
                      then (r := i + 1; x)
                      else loop b (i + 1)
     val yes = tabulate (n, fn _ => loop true (!r))
     val _ = r := 0
     val no = tabulate (length v - n, fn _ => loop false (!r))
   in
     {yes = yes, no = no}
   end

fun partition (v, f) = partitioni (v, f o #2)

fun prefix (v, n) = tabulate (n, fn i => sub (v, i))

fun removeDuplicates (v, equals) =
   keepAllMapi (v, fn (i, x) =>
                if i > 0 andalso equals (x, sub (v, i - 1))
                   then NONE
                else SOME x)

fun randomElement v = sub (v, Random.natLessThan (length v))

end
