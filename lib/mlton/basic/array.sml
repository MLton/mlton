structure Array =
   Array (open Pervasive.Array
	  type 'a t = 'a array
	  exception New = Size
	  val unsafeSub = Unsafe.Array.sub
	  val unsafeUpdate = Unsafe.Array.update
	  val unfoldi = MLton.Array.unfoldi)

functor MonoArray (Elt: T) =
   struct
      open Array
      type t = Elt.t t
      val equals = fn (a, a') => equals (a, a', Elt.equals)
      val layout = layout Elt.layout
   end
