(* Copyright (C) 2018-2019 Jason Carr, Matthew Fluet.
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

      val list: word List.t -> word
      val listMap: ('a List.t * ('a -> word)) -> word

      val option: word Option.t -> word
      val optionMap: 'a Option.t * ('a -> word) -> word

      val permute: word -> word

      val vector: word Vector.t -> word
      val vectorMap: ('a Vector.t * ('a -> word)) -> word
   end
