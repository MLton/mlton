(* Copyright (C) 2019 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Static (S: STATIC_STRUCTS): STATIC =
   struct

      open S

      structure Data = struct
         structure Elem = struct
            datatype 'a t =
               Address of 'a (* must be statically allocated *)
             | Word of WordX.t (* must be pointer-sized *)
             | Real of RealX.t

            fun layout layoutI =
               let open Layout
               in fn Address i => layoutI i
                   | Word w => WordX.layout (w, {suffix=true})
                   | Real r => RealX.layout (r, {suffix=true})
               end
         end
         datatype 'a t =
            Empty of Bytes.t
          | Object of ('a Elem.t) list
          | Vector of WordXVector.t

         fun map (t, f) =
            case t of
                 Empty b => Empty b
               | Object es => (Object o List.map) (es,
                  fn Elem.Address a => Elem.Address (f a)
                   | Elem.Word wx => Elem.Word wx
                   | Elem.Real rx => Elem.Real rx)
               | Vector wxv => Vector wxv

         fun layout layoutI =
            let open Layout
            in fn Empty b => seq [str "Empty ", Bytes.layout b]
                | Object es => seq [str "Object ",
                     list (List.map (es, Elem.layout layoutI))]
                | Vector v => seq [str "Vector ", WordXVector.layout v]
            end

         val size =
            fn Empty bytes => (WordSize.word8, Bytes.toInt bytes)
             | Vector v => (WordXVector.elementSize v, WordXVector.length v)
             | Object es => (WordSize.objptr (), List.length es)
      end
      structure Location = struct
         datatype t =
            MutStatic (* Mutable static, .data/.bss *)
          | ImmStatic (* Immutable static, .rodata, must be statically initialized *)
          | Heap (* Dynamically allocated in main *)

         val layout =
            fn MutStatic => Layout.str "MutStatic"
             | ImmStatic => Layout.str "ImmStatic"
             | Heap => Layout.str "Heap"
      end
      datatype 'a t =
         T of {data: 'a Data.t,
               location: Location.t,
               metadata: WordX.t list} (* mapped in-order *)

      fun map (T {data, metadata, location}, f) =
         T {data=Data.map (data, f), metadata=metadata, location=location}
      fun layout layoutI (T {data, metadata, location}) =
         let open Layout
         in record
            [("data", Data.layout layoutI data),
             ("location", Location.layout location),
             ("metadata", List.layout (fn w => WordX.layout (w, {suffix = true})) metadata)]
         end

      fun metadataSize (T {metadata, ...}) =
         List.fold (metadata, Bytes.zero, fn (w, b) =>
                    Bytes.+ (WordSize.bytes (WordX.size w), b))

      fun getTyconHeader tycon =
         let
            val header = Runtime.typeIndexToHeader (ObjptrTycon.index tycon)
            val header = WordX.fromIntInf (Word.toIntInf header, WordSize.objptrHeader ())
         in
            header
         end

      fun object {elems, location, tycon} =
         T {data = Data.Object elems,
            location = location,
            metadata = [getTyconHeader tycon]}

      fun sequenceMetadata (tycon, length: int) =
         let
            val counter = WordX.zero (WordSize.seqIndex ())
            val length = WordX.fromIntInf (Int.toIntInf length, WordSize.seqIndex ())
            val header = getTyconHeader tycon
         in
            [counter, length, header]
         end


      fun vector {data, location, tycon} =
         T {data = Data.Vector data,
            location = location,
            metadata = sequenceMetadata (tycon, WordXVector.length data)}

      fun sequence {length, location, totalSize, tycon} =
         T {data = Data.Empty totalSize,
            location = location,
            metadata = sequenceMetadata (tycon, length)}



   end
