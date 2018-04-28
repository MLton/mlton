(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Fold (S: FOLD_STRUCTS): FOLD = 
struct

open S

fun foldi (l: 'a t, b, f) =
   #1 (fold (l, (b, 0: int), fn (x, (b, i)) => (f (i, x, b), i + 1)))

fun foreachi (l, f) = foldi (l, (), fn (i, x, ()) => f (i, x))

fun foreach (l, f: 'a elt -> unit) = fold (l, (), f o #1)

fun last l =
   case fold (l, NONE, SOME o #1) of
      NONE => Error.bug "Fold.last"
    | SOME x => x

fun length l = fold (l, 0: int, fn (_, n) => n + 1)

fun mapi (l, f) = rev (foldi (l, [], fn (i, x, l) => f (i, x) :: l))

fun map (l, f) = mapi (l, f o #2)

fun layout f l = Layout.list (map (l, f))

fun revKeepAllMap (l, f) =
   fold (l, [], fn (x, ac) =>
         case f x of
            NONE => ac
          | SOME y => y :: ac)

fun keepAllMap z = rev (revKeepAllMap z)

fun revKeepAll (l, f) =
   fold (l, [], fn (x, ac) => if f x then x :: ac else ac)

fun keepAll z = rev (revKeepAll z)

fun revRemoveAll (l, f) =
   fold (l, [], fn (x, ac) => if f x then ac else x :: ac)

fun removeAll z = rev (revRemoveAll z)

end
