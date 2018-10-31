(* Copyright (C) 2018 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* HASH defines a set of transformations on hash values *)
signature HASH =
   sig
      (* combine a list of hash values so that each is independent from the last.
       * No guarantee is made as to whether or not combine [w] = w *)
      val combine: word List.t -> word

      (* combine a list of hash values so that each is independent from the last.
       * No guarantee is made as to whether or not combine [w] = w *)
      val vector: word Vector.t -> word
      val vectorMap: ('a Vector.t * ('a -> word)) -> word

      (* 0x9e3779b97f4a7c15 *)
      val permute: word -> word
   end
