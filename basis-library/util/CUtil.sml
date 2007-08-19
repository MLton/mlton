(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure CUtil: C_UTIL =
   struct
      open Int

      structure Pointer = Primitive.MLton.Pointer

      fun makeLength (sub, term) p =
         let
            fun loop i =
               if term (sub (p, i))
                  then i
                  else loop (i +? 1)
         in loop 0
         end

      fun toArrayOfLength (s: 'a,
                           sub: 'a * int -> 'b,
                           n: int) : 'b array =
         let
            val a = Array.arrayUninit n
            fun loop i =
               if i >= n
                  then ()
                  else (Array.update (a, i, sub (s, i))
                        ; loop (i + 1))
            val () = loop 0
         in 
            a
         end

      structure C_Pointer =
         struct
            type t = C_Pointer.t
            val null = Pointer.toWord Pointer.null
            fun isNull p = p = null
         end

      structure C_String =
         struct
            type t = C_String.t

            fun sub (cs, i) =
               Primitive.Char8.idFromWord8
               (Pointer.getWord8 
                (Pointer.fromWord cs, 
                 C_Ptrdiff.fromInt i))

            fun update (cs, i, c) =
               Pointer.setWord8 
               (Pointer.fromWord cs, 
                C_Ptrdiff.fromInt i, 
                Primitive.Char8.idToWord8 c)

            val length = makeLength (sub, fn #"\000" => true | _ => false)

            fun toCharArrayOfLength (cs, n) = 
               toArrayOfLength (cs, sub, n)

            fun toStringOfLength (cs, n) =
               String.unsafeFromArray 
               (CharArray.fromPoly (toCharArrayOfLength (cs, n)))

            fun toString cs = toStringOfLength (cs, length cs)
         end

      structure C_StringArray =
         struct
            type t = C_StringArray.t

            fun sub (css: t, i) = 
               (Pointer.toWord o Pointer.getCPointer)
               (Pointer.fromWord css, 
                C_Ptrdiff.fromInt i)

            val length = makeLength (sub, C_Pointer.isNull)

            val toArrayOfLength = 
               fn (css, n) => 
               toArrayOfLength (css, C_String.toString o sub, n)

            fun toArray css = toArrayOfLength (css, length css)

            val toList = Array.toList o toArray
         end

      structure StringVector =
         struct
            type t = string * C_Pointer.t array * C_Size.t vector
            val padVec =
               Vector.fromList
               ["\000\000\000",
                "\000\000",
                "\000",
                "\000\000\000\000"]
            fun fromList (l : string list) : t =
               let
                  val n = List.length l
                  (* The C side updates the array with addresses
                   * using the vector of offsets.
                   *)
                  val aPtr = Array.array (1 +? n, C_Pointer.null)
                  val (vOff,(_,_,acc)) =
                     Vector.unfoldi
                     (n, (l, 0w0, []), fn (_, (l, off, acc)) =>
                      let
                         val s' = List.hd l
                         val l' = List.tl l
                         val n' = String.size s'

                         val pad' = Vector.sub (padVec, Int.mod (n', 4))
                         val sz' = n' + Vector.length pad'
                         val off' = C_Size.+ (off, C_Size.fromInt sz')
                      in
                         (off, (l', off', pad'::s'::acc))
                      end)
                  val str = Vector.concat (List.rev acc)
               in
                  (str, aPtr, vOff)
               end
         end
   end
