(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Array2: ARRAY2 =
struct

open Array2

type 'a t = 'a array

fun toList a =
   let
      fun loop (r, ac) =
         if r < 0
            then ac
         else loop (r - 1,
                    let
                       fun loop (c, ac) =
                          if c < 0
                             then ac
                          else loop (c - 1, sub (a, r, c) :: ac)
                    in loop (nCols a - 1, [])
                    end :: ac)
   in loop (nRows a - 1, [])
   end

fun layout f a = List.layout (List.layout f) (toList a)

fun wholeRegion a : 'a region =
   {base = a, row = 0, col = 0, nrows = NONE, ncols = NONE}

fun foralli (a, f) =
   let exception False
   in (appi RowMajor (fn (r, c, x) =>
                      if f (r, c, x)
                         then ()
                      else raise False)
       (wholeRegion a)
       ; true)
      handle False => false
   end

fun equals (a, a', f) =
   nRows a = nRows a'
   andalso nCols a = nCols a'
   andalso foralli (a, fn (r, c, x) => f (x, sub (a', r, c)))

fun forall (a, f) = foralli (a, f o #3)

fun tabulate (r, c, f) = Pervasive.Array2.tabulate RowMajor (r, c, f)

fun foreachi (a, f) =
   foldi RowMajor (fn (r, c, a, ()) => f (r, c, a)) () (wholeRegion a)

fun foreach (a, f) = foreachi (a, f o #3)

fun copy a = tabulate (nRows a, nCols a, fn (r, c) => sub (a, r, c))

fun new (r, c, x) = tabulate (r, c, fn _ => x)

end
