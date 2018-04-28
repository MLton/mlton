(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor BinaryHeap (Key: BOUNDED_ORDER): HEAP =
struct

structure Array = ResizableArray
structure Key = Key

(*--------------------------------------------------------*)
(*                        Element                         *)
(*--------------------------------------------------------*)
structure Element =
   struct
      datatype 'a t = T of {key: Key.t ref,
                            value: 'a,
                            index: int ref}
      fun new(k, v, i) = T{key = ref k,
                           value = v,
                           index = ref i}
      fun key(T{key, ...}) = !key
      fun setKey(T{key, ...}, k) = key := k
      fun value (T{value, ...}) = value
      fun index (T{index, ...}) = !index
      fun setIndex(T{index, ...}, i) = index := i
   end
structure Elt = Element

(*--------------------------------------------------------*)
(*                          Heap                          *)
(*--------------------------------------------------------*)

datatype 'a t = T of 'a Elt.t Array.t

fun empty() = T (Array.fromList [])

fun fixIndex(a, i) = Elt.setIndex(Array.sub(a, i), i)
fun swap(a, i, j) = (Array.swap(a, i, j)
                     ; fixIndex(a, i)
                     ; fixIndex(a, j))

fun isEmpty (T a) = Array.length a = 0

fun parent(i: int) = (i - 1) div 2
fun left(i: int) = 2 * i + 1
fun right(i: int) = 2 * i + 2
fun key(a, i) = Elt.key(Array.sub(a, i))
fun keyOption(a, i) = Option.map(Array.subOption(a, i), Elt.key )

fun siftUp(a, i) =
   let fun siftUp i = if i = 0 then ()
                      else let val p = parent i
                           in if Key.<(key(a, i), key(a, p))
                                 then (swap(a, i, p); siftUp p)
                              else ()
                           end
   in siftUp i
   end

fun siftDown(a, i) =
   let
      fun siftDown i =
         let val l = left i
            val r = right i
         in case keyOption(a, l) of
            NONE => ()
          | SOME kl =>
               let val min = (case keyOption(a, r) of
                                 NONE => l
                               | SOME kr => if Key.<(kl, kr)
                                                then l else r)
               in if Key.<(key(a, i), key(a, min)) then ()
                  else (swap(a, i, min); siftDown min)
               end
         end
   in siftDown i
   end

fun new es =
   let val a = Array.fromList (List.mapi (es, fn (i, (k, v)) =>
                                          Elt.new (k, v, i)))
      val start = (Array.length a) div 2
   in Int.forDown (start, 0, fn i => siftDown (a, i))
      ; T a
   end

fun isEmpty (T a) = Array.length a = 0

fun foreach(T a, f) = Array.foreach(a, f)

fun insert(T a, k, v) =
   let val i = Array.length a
      val e = Elt.new(k, v, i)
   in Array.addToEnd(a, e)
      ; siftUp(a, i)
      ; e
   end

fun min (h as (T a)) =
   if isEmpty h then Error.bug "min"
   else Array.sub(a, 0)

fun deleteMin (h as (T a)) =
   if isEmpty h then Error.bug "deleteMin"
   else Elt.value (if Array.length a = 1
                    then Array.deleteLast a
                 else let val min = Array.sub(a, 0)
                      in Array.update(a, 0, Array.deleteLast a)
                         ; fixIndex(a, 0)
                         ; siftDown(a, 0)
                         ; min
                      end)

fun decreaseKey(T a, e, k) =
   if Key.<(Elt.key e, k) then Error.bug "decreaseKey"
   else (Elt.setKey(e, k); siftUp(a, Elt.index e))

fun delete(h, e) = (decreaseKey(h, e, Key.smallest); deleteMin h; ())

fun union(h, h') =
    foreach(h', fn e => (insert(h, Elt.key e, Elt.value e); ()))

end
