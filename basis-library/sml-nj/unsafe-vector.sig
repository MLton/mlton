(* sweeks commented out create 1998-12-17. *)

(* unsafe-vector.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature UNSAFE_VECTOR =
  sig

    val sub : 'a vector * int -> 'a
(*    val create : int * 'a list -> 'a vector *)

  end


(*
 * $Log$
 *)
