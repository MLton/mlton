(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PropertyList (H: HET_CONTAINER):> PROPERTY_LIST =
struct

datatype t = T of H.t list ref

fun new (): t = T (ref [])

fun length (T r) = List.length (!r)

val equals = fn (T r, T r') => Ref.equals (r, r')

fun clear (T hs) = hs := []

val numPeeks: Int64.int ref = ref 0
val numLinks: Int64.int ref = ref 0
val maxLength: int ref = ref 0

fun stats () =
   let open Layout
   in align
      [seq [str "property list numPeeks = ", str (Int64.toString (!numPeeks))],
       (* seq [str "property list numLinks = ", str (Int64.toString (!numLinks))], *)
       seq [str "property list maxLength = ", Int.layout (!maxLength)],
       seq [str "property list average position = ",
            str let open Real
                    val fromInt = fromIntInf o Int64.toLarge
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
                     ((numLinks := Int64.fromInt n + !numLinks
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
