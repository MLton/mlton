structure Vector =
   let
      structure V = Vector (open Pervasive.Vector
			    type 'a t = 'a vector
			    exception New = Size
			    val unsafeSub = Unsafe.Vector.sub)
   in
      struct
	 open V

	 type 'a vector = 'a t

	 (* The built-in concat is faster in MLton because it can use
	  * Vector.fromArray.
	  * See src/basis-library/arrays-and-vectors/sequence.fun.
	  *)
	 val concat = Pervasive.Vector.concat
      end
   end
