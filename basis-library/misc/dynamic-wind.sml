structure DynamicWind: DYNAMIC_WIND =
struct

fun try (f: unit -> 'a, k: 'a -> 'b, h: exn -> 'b) =
   let
      datatype t =
	 A of 'a
       | E of exn
   in
      case A (f ()) handle e => E e of
	 A a => k a
       | E e => h e
   end
	 
fun wind (thunk, cleanup) =
   try (thunk, fn a => (cleanup (); a), fn e => (cleanup (); raise e))

end

