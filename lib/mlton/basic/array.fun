(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Array (S: sig
                     include ARRAY_STRUCTS
                     val unsafeSub: 'a t * int -> 'a
                     val unsafeUpdate: 'a t * int * 'a -> unit
                  end): ARRAY =
struct

open S

local
   structure V = Vector (S)
in
   open V
end

val array = new

fun modify (a, f) = foreachi (a, fn (i, x) => unsafeUpdate (a, i, f x))

fun swap (a, i, j) =
   let val t = sub (a, i)
   in unsafeUpdate (a, i, sub (a, j))
      ; unsafeUpdate (a, j, t)
   end

fun shuffleN (a, n) =
   let
      val m = length a
   in
      Int.for (0, n, fn i => swap (a, i, i + Random.natLessThan (m - i)))
   end

fun shuffle a = shuffleN (a, length a)

fun getAndSet a = (fn i => sub (a, i),
                   fn (i, x) => update (a, i, x))

fun fromListRev l =
   case l of
      [] => new0 ()
    | x :: l =>
         let
            val n = List.length l
            val a = new (n + 1, x)
            fun loop (l, i) =
               case l of
                  [] => ()
                | x :: l => (unsafeUpdate (a, i, x)
                             ; loop (l, i - 1))
            val _ = loop (l, n - 1)
         in a
         end

fun toVectorMap (a, f) = Vector.tabulate (length a, fn i => f (sub (a, i)))

fun toVector a = toVectorMap (a, fn x => x)

fun fromVector v = tabulate (Vector.length v, fn i => Vector.sub (v, i))

end
