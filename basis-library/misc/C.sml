(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure C: C =
   struct
      open Int
         
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
            val a = Primitive.Array.array n
            fun loop i =
               if i >= n
                  then ()
               else (Array.update (a, i, sub (s, i))
                     ; loop (i + 1))
         in loop 0;
            a
         end

      structure CS =
         struct
            type t = Pointer.t

            fun sub (cs, i) =
               Primitive.Char.fromWord8 (Primitive.Pointer.getWord8 (cs, i))

            fun update (cs, i, c) =
               Primitive.Pointer.setWord8 (cs, i, Primitive.Char.toWord8 c)

            fun toCharArrayOfLength (cs, n) = toArrayOfLength (cs, sub, n)

            fun toStringOfLength cs =
               String.fromArray (CharArray.fromPoly (toCharArrayOfLength cs))

            val length = makeLength (sub, fn #"\000" => true | _ => false)

            fun toString cs = toStringOfLength (cs, length cs)
         end
      
      structure CSS =
         struct
            type t = Pointer.t

            fun sub (css: t, i) = Primitive.Pointer.getPointer (css, i)

            val length = makeLength (sub, Primitive.Pointer.isNull)

            val toArrayOfLength =
               fn (css, n) => toArrayOfLength (css, CS.toString o sub, n)

            fun toArray css = toArrayOfLength (css, length css)

            val toList = Array.toList o toArray

            (* The C side converts the last element of the array, "",
             * to the null terminator that C primitives expect.
             * As far as C can tell, the other elements of the array
             * are just char*'s.
             *)
            fun fromList l =
               let
                  val a = Array.array (1 +? List.length l, NullString.empty)
                  val _ =
                     List.foldl (fn (s, i) =>
                                 (Array.update (a, i, NullString.nullTerm s)
                                  ; i +? 1))
                     0 l
               in
                  a
               end
         end
   end
