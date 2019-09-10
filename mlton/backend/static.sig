(* Copyright (C) 2019 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature STATIC_STRUCTS = sig
   structure ObjptrTycon: OBJPTR_TYCON
   structure RealSize: REAL_SIZE
   structure RealX: REAL_X
   structure Runtime: RUNTIME
   structure WordSize: WORD_SIZE
   structure WordX: WORD_X
   structure WordXVector: WORD_X_VECTOR
   sharing RealX.RealSize = RealSize
   sharing WordX.WordSize = WordSize
   sharing WordXVector.WordSize = WordSize
   sharing WordXVector.WordX = WordX
end

signature STATIC =
   sig

      include STATIC_STRUCTS
      structure Data: sig
         structure Elem: sig
            datatype 'a t =
               Address of 'a (* must be statically allocated *)
             | Word of WordX.t
             | Real of RealX.t
         end

         datatype 'a t =
            Empty of Bytes.t
          | Object of ('a Elem.t) list
          | Vector of WordXVector.t

         val map: ('a t * ('a -> 'b)) -> 'b t
         val layout: ('a -> Layout.t) -> 'a t -> Layout.t
         val size: 'a t -> WordSize.t * int
      end


      structure Location: sig
         datatype t =
            MutStatic (* Mutable static, .data/.bss *)
          | ImmStatic (* Immutable static, .rodata *)
          | Heap (* Dynamically allocated in main *)
      end
      datatype 'a t =
         T of {data: 'a Data.t,
               location: Location.t,
               metadata: WordX.t list} (* mapped in-order *)

      val object: {elems: ('a Data.Elem.t) list,
                   location: Location.t,
                   tycon: ObjptrTycon.t} -> 'a t
      val sequence: {length: int,
                     location: Location.t,
                     totalSize: Bytes.t,
                     tycon: ObjptrTycon.t} -> 'a t
      val vector: {data: WordXVector.t,
                   location: Location.t,
                   tycon: ObjptrTycon.t} -> 'a t

      val map: ('a t * ('a -> 'b)) -> 'b t
      val metadataSize: 'a t -> Bytes.t
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
   end
