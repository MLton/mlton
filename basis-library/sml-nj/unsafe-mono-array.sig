(* unsafe-mono-array.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature UNSAFE_MONO_ARRAY =
  sig

    type array
    type elem

    val sub: array * int -> elem
    val update: array * int * elem -> unit
    val create: int -> array

  end


(*
 * $Log$
 *)
