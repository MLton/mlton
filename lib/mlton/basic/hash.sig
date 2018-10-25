(* Copyright (C) 2018 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* HASH defines a set of transformations on hash values *)
signature HASH =
   sig
      (* combine a number of values into one value independent from either *)
      val combine: word * word -> word
      val combine3: word * word * word -> word

      (* combine a vector of hash values *)
      val vector: word Vector.t -> word
      val vectorMap: ('a Vector.t * ('a -> word)) -> word

      val list: word List.t -> word
      val listMap: ('a List.t * ('a -> word)) -> word

      (* 0x9e3779b97f4a7c15 *)
      val permute: word -> word
   end
