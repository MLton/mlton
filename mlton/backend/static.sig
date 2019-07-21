(* Copyright (C) 2019 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature STATIC_STRUCTS = sig
   structure WordX: WORD_X
   structure WordSize: WORD_SIZE
   structure WordXVector: WORD_X_VECTOR
   sharing WordX.WordSize = WordSize
   sharing WordXVector.WordSize = WordSize
end

signature STATIC =
   sig

      include STATIC_STRUCTS
      structure Data: sig
         datatype 'a elem =
            Address of 'a (* must be statically allocated *)
          | Word of WordX.t

         datatype 'a t =
            Empty of Bytes.t
          | Object of ('a elem) list
          | Vector of WordXVector.t

         val map: ('a t * ('a -> 'b)) -> 'b t
         val layout: ('a -> Layout.t) -> 'a t -> Layout.t
         val size: 'a t -> WordSize.t * int
      end

      datatype location =
         MutStatic (* Mutable static, .data/.bss *)
       | ImmStatic (* Immutable static, .rodata *)
       | Heap (* Dynamically allocated in main *)
      datatype 'a t =
         T of {data: 'a Data.t,
               header: WordXVector.t, (* mapped in-order *)
               location: location}

      val map: ('a t * ('a -> 'b)) -> 'b t
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
   end

