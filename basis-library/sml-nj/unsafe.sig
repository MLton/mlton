(* A subset of the UNSAFE signature provided by SML/NJ. *)

(* unsafe.sig
 *
 * Copyright (c) 1997 Bell Labs, Lucent Technologies.
 *
 * Unsafe operations on ML values.
 *)

signature UNSAFE =
   sig
      structure Array:
	 sig
	    val create: int * 'a -> 'a array
	    val sub: 'a array * int -> 'a
	    val update: 'a array * int * 'a -> unit
	 end
      structure CharArray: UNSAFE_MONO_ARRAY
      structure CharVector: UNSAFE_MONO_VECTOR
      structure Real64Array: UNSAFE_MONO_ARRAY
      structure Vector:
	 sig
	    val sub: 'a vector * int -> 'a
	 end
      structure Word8Array: UNSAFE_MONO_ARRAY
      structure Word8Vector: UNSAFE_MONO_VECTOR
   end
