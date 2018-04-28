(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CircularList(S: CIRCULAR_LIST_STRUCTS): CIRCULAR_LIST =
struct

open S

type 'a t = 'a Elt.t Pointer.t

val empty = Pointer.null

fun makeEmpty p = Pointer.clear p

fun isEmpty p = Pointer.isNull p

fun isSingle p =
   case Pointer.follow p of
      NONE => false
    | SOME d => Elt.eqPrev(d, Elt.prev d)

val first = Pointer.!

fun insert(p,d) =
   case Pointer.follow p of
      SOME d' => Elt.insertR(d', d)
    | NONE => (Elt.link(d, d); Pointer.:=(p, d))

fun rotate p =
   case Pointer.follow p of
      SOME d => Pointer.:=(p, Elt.next d)
    | NONE => ()

fun deleteSafe(p, d) =
   (if Elt.eqPrev(Pointer.! p, d)
       then if isSingle p then makeEmpty p
            else Pointer.:=(p, Elt.next d)
    else ()
    ; Elt.unlink d)

fun delete(l, d) =
   if Elt.isLinked d then deleteSafe(l, d)
   else Error.bug "CircularList.delete"

fun foreach(p, f) =
   if Pointer.isNull p then ()
   else
      let
         val start = Pointer.! p
         fun foreach d =
            let val next = Elt.next d
            in (f d
                ; if Elt.eqPrev(start, next)
                     then ()
                  else foreach next)
            end
      in foreach start
      end

fun deleteEach(p, f) = foreach(p, fn d => (Elt.unlink d; f d))

fun splice(p, p') =
   if Pointer.isNull p then Pointer.copy(p, p')
   else if Pointer.isNull p' then ()
        else let val e1 = Pointer.! p
                 val e1' = Pointer.! p'
                 val e2 = Elt.next e1
                 val e2' = Elt.next e1'
             in Elt.link(e1, e2')
                ; Elt.link(e1', e2)
             end

end
