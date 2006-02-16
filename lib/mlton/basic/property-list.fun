(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PropertyList (H: HET_CONTAINER):> PROPERTY_LIST =
struct

datatype t = T of H.t list ref

fun new (): t = T (ref [])

fun length (T r) = List.length (!r)

val equals = fn (T r, T r') => Ref.equals (r, r')

fun clear (T hs) = hs := []

val numPeeks: int ref = ref 0
val numLinks: int ref = ref 0
val maxLength: int ref = ref 0
   
fun stats () =
   let open Layout
   in align
      [seq [str "numPeeks = ", Int.layout (!numPeeks)],
       seq [str "maxLength = ", Int.layout (!maxLength)],
       seq [str "average position in property list = ",
            str let open Real
                in format (fromInt (!numLinks) / fromInt (!numPeeks),
                           Format.fix (SOME 3))
                end]]
   end

fun 'a newProperty () =
   let
      val {make, pred, peek = peekH} = H.new ()
      fun peek (T hs) =
         let
            fun loop (l, n) =
               let
                  fun update () =
                     ((numLinks := n + !numLinks
                       handle Overflow => Error.bug "PropertyList: numLinks overflow")
                      ; if n > !maxLength
                           then maxLength := n
                        else ())
               in case l of
                  [] => (update (); NONE)
                | e :: l =>
                     case peekH e of
                        r as SOME _ => (update (); r)
                      | NONE => loop (l, n + 1)
               end
            val _ =
               numPeeks := 1 + !numPeeks
               handle Overflow => Error.bug "PropertyList: numPeeks overflow"
         in
            loop (!hs, 0)
         end

      fun add (T hs, v: 'a): unit = hs := make v :: !hs

      fun remove (T hs) = hs := List.remove (!hs, pred)
   in
      {add = add, peek = peek, remove = remove}
   end

end
