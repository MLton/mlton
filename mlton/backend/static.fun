(* Copyright (C) 2019 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Static (S: STATIC_STRUCTS): STATIC =
   struct

      open S

      datatype elem =
         Address of Index.t (* must be statically allocated *)
       | Word of WordX.t (* must be pointer-sized *)
      datatype data =
         Empty of Bytes.t
       | Object of elem list
       | Vector of WordXVector.t
      datatype location =
         MutStatic (* Mutable static, .data/.bss *)
       | ImmStatic (* Immutable static, .rodata, must be statically initialized *)
       | Heap (* Dynamically allocated in main *)
      datatype t =
         T of {data: data,
               header: WordXVector.t, (* mapped in-order *)
               location: location}
      (*

      fun dataEquals
      fun equals
         (T {data=data1, header=header1, location=loc1, mutable=mut1},
          T {data=data2, header=header2, location=loc2, mutable=mut2}) =

       *)



      val layoutElem =
         let open Layout
         in fn Address g => Index.layout g
             | Word w => WordX.layout (w, {suffix=false})
         end
      val layoutData =
         let open Layout
         in fn Empty b => seq [str "Empty ", Bytes.layout b]
             | Object es => seq [str "Object ",
                  list (List.map (es, layoutElem))]
             | Vector v => seq [str "Vector ", WordXVector.layout v]
         end
      val layoutLocation =
         fn MutStatic => Layout.str "MutStatic"
          | ImmStatic => Layout.str "ImmStatic"
          | Heap => Layout.str "Heap"
      fun layout (T {data, header, location}) =
         let open Layout
         in record
            [("data", layoutData data),
             ("header", WordXVector.layout header),
             ("location", layoutLocation location)]
         end
   end
