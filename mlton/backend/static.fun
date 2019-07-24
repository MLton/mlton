(* Copyright (C) 2019 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Static (S: STATIC_STRUCTS): STATIC =
   struct

      open S

      structure Data = struct
         datatype 'a elem =
            Address of 'a (* must be statically allocated *)
          | Word of WordX.t (* must be pointer-sized *)
         datatype 'a t =
            Empty of Bytes.t
          | Object of ('a elem) list
          | Vector of WordXVector.t

         fun map (t, f) =
            case t of
                 Empty b => Empty b
               | Object es => (Object o List.map) (es,
                  fn Address a => Address (f a)
                   | Word wx => Word wx)
               | Vector wxv => Vector wxv

         fun layoutElem layoutI =
            let open Layout
            in fn Address i => layoutI i
                | Word w => WordX.layout (w, {suffix=true})
            end

         fun layout layoutI =
            let open Layout
            in fn Empty b => seq [str "Empty ", Bytes.layout b]
                | Object es => seq [str "Object ",
                     list (List.map (es, layoutElem layoutI))]
                | Vector v => seq [str "Vector ", WordXVector.layout v]
            end

         val size =
            fn Empty bytes => (WordSize.word8, Bytes.toInt bytes)
             | Vector v => (WordXVector.elementSize v, WordXVector.length v)
             | Object es => (WordSize.objptr (), List.length es)
      end
      datatype location =
         MutStatic (* Mutable static, .data/.bss *)
       | ImmStatic (* Immutable static, .rodata, must be statically initialized *)
       | Heap (* Dynamically allocated in main *)
      datatype 'a t =
         T of {data: 'a Data.t,
               header: WordXVector.t, (* mapped in-order *)
               location: location}

      val layoutLocation =
         fn MutStatic => Layout.str "MutStatic"
          | ImmStatic => Layout.str "ImmStatic"
          | Heap => Layout.str "Heap"
      fun map (T {data, header, location}, f) =
         T {data=Data.map (data, f), header=header, location=location}
      fun layout layoutI (T {data, header, location}) =
         let open Layout
         in record
            [("data", Data.layout layoutI data),
             ("header", WordXVector.layout header),
             ("location", layoutLocation location)]
         end


      fun object {elems, tycon, location} =
         let
            val header = Runtime.typeIndexToHeader (ObjptrTycon.index tycon)
            val header = WordX.fromIntInf (Word.toIntInf header, WordSize.objptrHeader ())
            val header =  WordXVector.fromList
               ({elementSize = WordSize.objptrHeader ()}, [header])
         in
            T
            {header = header,
             data = Data.Object elems,
             location = location}
         end

      fun vector {data, tycon, location} =
         let
            val header = Runtime.typeIndexToHeader (ObjptrTycon.index tycon)
            val header = WordX.fromIntInf (Word.toIntInf header, WordSize.objptrHeader ())
            val wordSize = WordSize.objptr ()
            val length = WordX.fromIntInf (Int.toIntInf (WordXVector.length data), wordSize)
            val capacity = WordX.zero wordSize
            val header =  WordXVector.fromList
               ({elementSize = WordSize.objptrHeader ()}, [capacity, length, header])
         in
            T
            {header = header,
             data = Data.Vector data,
             location = location}
         end


   end
