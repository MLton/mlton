(* sweeks deleted a bunch of stuff 1998-12-17. *)

(* unsafe.sig
 *
 * Copyright (c) 1997 Bell Labs, Lucent Technologies.
 *
 * Unsafe operations on ML values.
 *)

signature UNSAFE =
  sig
    structure Array: UNSAFE_ARRAY
    structure CharArray: UNSAFE_MONO_ARRAY
    structure CharVector: UNSAFE_MONO_VECTOR
    structure Real64Array: UNSAFE_MONO_ARRAY
    structure Vector: UNSAFE_VECTOR
    structure Word8Array: UNSAFE_MONO_ARRAY
    structure Word8Vector: UNSAFE_MONO_VECTOR
  end
