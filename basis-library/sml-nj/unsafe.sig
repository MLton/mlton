(* sweeks deleted a bunch of stuff 1998-12-17. *)

(* unsafe.sig
 *
 * Copyright (c) 1997 Bell Labs, Lucent Technologies.
 *
 * Unsafe operations on ML values.
 *)

signature UNSAFE =
  sig
    structure Vector: UNSAFE_VECTOR
    structure Array: UNSAFE_ARRAY

    structure CharVector: UNSAFE_MONO_VECTOR
      where type vector = CharVector.vector
      where type elem = CharVector.elem
    structure CharArray: UNSAFE_MONO_ARRAY
      where type array = CharArray.array
      where type elem = CharArray.elem

    structure Word8Vector: UNSAFE_MONO_VECTOR
      where type vector = Word8Vector.vector
      where type elem = Word8Vector.elem
    structure Word8Array: UNSAFE_MONO_ARRAY
      where type array = Word8Array.array
      where type elem = Word8Array.elem
    structure Real64Array: UNSAFE_MONO_ARRAY
      where type array = Real64Array.array
      where type elem = Real64Array.elem
  end
