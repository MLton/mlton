(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Equatable: EQUATABLE =
struct

structure Set = DisjointSet

datatype 'a delay =
   Computed of 'a
 | Uncomputed of {compute: unit -> 'a,
                  whenComputed: ('a -> unit) AppendList.t ref}

datatype 'a t = T of 'a delay Set.t

fun layout (T s, f) =
   case Set.! s of
      Computed a => f a
    | Uncomputed _ => Layout.str "<uncomputed>"

fun delay f =
   T (Set.singleton (Uncomputed {compute = f,
                                 whenComputed = ref AppendList.empty}))

fun new a = T (Set.singleton (Computed a))

fun equals (T s, T s') = Set.equals (s, s')

fun value (T s) =
   case Set.! s of
      Computed a => a
    | Uncomputed {compute, whenComputed} =>
         let
            val a = compute ()
            val () = Set.:= (s, Computed a)
            val () = AppendList.foreach (!whenComputed, fn f => f a)
         in
            a
         end

fun equate (T s, T s', combine) =
   if Set.equals (s, s')
      then ()
   else
      let
         val d = Set.! s
         val d' = Set.! s'
         val () = Set.union (s, s')
         fun one (a, {compute = _, whenComputed}) =
            (* Must set the value before calling the whenComputed, because
             * those may look at the value (which would cause it to be set,
             * which would then be overwritten).
             *)
            (Set.:= (s, Computed a)
             ; AppendList.foreach (!whenComputed, fn f => f a))
      in
         case (d, d') of
            (Computed a, Computed a') =>
               Set.:= (s, Computed (combine (a, a')))
          | (Computed a, Uncomputed u) => one (a, u)
          | (Uncomputed u, Computed a) => one (a, u)
          | (Uncomputed {compute, whenComputed = w},
             Uncomputed {whenComputed = w', ...}) =>
               Set.:=
               (s, Uncomputed {compute = compute,
                               whenComputed = ref (AppendList.append (!w, !w'))})
      end

fun whenComputed (T s, f): unit =
   case Set.! s of
      Computed a => f a
    | Uncomputed {whenComputed = w, ...} => AppendList.push (w, f)

end
