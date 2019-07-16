(* Copyright (C) 2019 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature STATIC_STRUCTS = sig
   structure Index: T
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
         datatype elem =
            Address of Index.t (* must be statically allocated *)
          | Word of WordX.t (* must be pointer-sized *)

         datatype t =
            Empty of Bytes.t
          | Object of elem list
          | Vector of WordXVector.t

         val layout: t -> Layout.t
         val size: t -> WordSize.t * int
      end

      datatype location =
         MutStatic (* Mutable static, .data/.bss *)
       | ImmStatic (* Immutable static, .rodata *)
       | Heap (* Dynamically allocated in main *)
      datatype t =
         T of {data: Data.t,
               header: WordXVector.t, (* mapped in-order *)
               location: location}

      val layout: t -> Layout.t
   end

