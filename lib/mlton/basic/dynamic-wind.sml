(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure DynamicWind: DYNAMIC_WIND =
struct

fun wind (thunk, cleanup) =
   let val a = thunk ()
   in cleanup (); a
   end handle exn => (cleanup (); raise exn)

fun windFail (f: unit -> 'a, g: unit -> unit): 'a =
   f () handle ex => (g (); raise ex)

fun 'a withEscape (f: ('a -> 'b) -> 'a): 'a =
   let exception E of 'a
   in f (fn x => raise E x) handle E x => x
   end

end

