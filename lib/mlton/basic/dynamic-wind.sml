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

