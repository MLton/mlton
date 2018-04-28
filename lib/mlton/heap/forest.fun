(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Forest of heap ordered trees.
 * Can be specialized to eager or lazy binomial heaps, fibonacci heaps.
 *)

functor ForestHeap (S: FOREST_HEAP_STRUCTS): FOREST_HEAP =
struct

open S

structure Elt =
struct
   (* Can't make child a circular list, because the elements aren't defined yet.
    *)
   datatype 'a t = T of {value: 'a Pointer.t,
                         mark: bool ref,
                         parent: 'a t Pointer.t,
                         next: 'a t Pointer.t,
                         prev: 'a t Pointer.t,
                         child: 'a t Pointer.t,
                         numChildren: int ref}

   fun destruct(T{prev, value, next, ...}) = (prev, Pointer.! value, next)

   fun make p = T{value = p,
                  mark = ref false,
                  parent = Pointer.null(),
                  next = Pointer.null(),
                  prev = Pointer.null(),
                  child = Pointer.null(),
                  numChildren = ref 0}

   fun new v = make(Pointer.new v)

   fun dummy v = make(Pointer.null())

   fun valuePtr(T{value, ...}) = value
   fun value e = Pointer.!(valuePtr e)
   fun mark(T{mark, ...}) = mark := true
   fun unMark(T{mark, ...}) = mark := false
   fun isMarked(T{mark, ...}) = !mark
   fun parent(T{parent, ...}) = Pointer.! parent
   fun hasParent(T{parent, ...}) = not(Pointer.isNull parent)
   fun setParent(T{parent, ...}, p) = Pointer.:=(parent, p)
   fun clearParent(T{parent, ...}) = Pointer.clear parent
   fun next(T{next, ...}) = next
   fun prev(T{prev, ...}) = prev
   fun child(T{child, ...}) = child
   fun numChildrenRef(T{numChildren, ...}) = numChildren
   fun numChildren e = !(numChildrenRef e)
   fun incNumChildren e = Int.inc(numChildrenRef e)
   fun decNumChildren e = Int.dec(numChildrenRef e)
   val equals = fn (e, e') => Pointer.eq(valuePtr e, valuePtr e')
end

structure CircList = CircularList(structure Elt = DoublyLinked(Elt))

structure Elt =
   struct
      open Elt
      type 'a t = (Key.t * 'a) Elt.t

      fun key(e: 'a t) = #1(value e)
      fun value(e: 'a t) = #2(Elt.value e)
      fun setKey(e, k) = Pointer.:=(valuePtr e, (k, value e))

      fun siftUp e =
         if hasParent e
            then let val p = parent e
                 in if Key.<(key e, key p)
                       then (Pointer.swap(valuePtr e, valuePtr p) ;
                             siftUp p)
                    else ()
                 end
         else ()
   end

(*--------------------------------------------------------*)
(*                     Heap Datatype                      *)
(*--------------------------------------------------------*)

datatype 'a t = T of {size: int ref,
                      roots: (Key.t * 'a) CircList.t,
                      min: 'a Elt.t Pointer.t}

fun sizeRef (T{size, ...}) = size
fun size h = !(sizeRef h)
fun setSize(h,n) = sizeRef h := n
fun incSize h = Int.inc(sizeRef h)
fun decSize h = Int.inc(sizeRef h)

fun roots (T{roots, ...}) = roots

fun min(T{min, ...}) = Pointer.! min

fun clearMin(T{min, ...}) = Pointer.clear min

fun updateMin(T{min, ...}, e) =
   if Pointer.isNull min orelse Key.<(Elt.key e, Elt.key(Pointer.!min))
      then Pointer.:=(min, e)
   else ()

fun addRoot(h, e) =
   (CircList.insert(roots h, e)
    ; updateMin(h, e))

fun isEmpty h = size h = 0

local
    fun linkPC(parent, child) =
        (Elt.incNumChildren parent
         ; CircList.insert(Elt.child parent, child)
         ; Elt.setParent(Elt.parent child, parent)
         ; Elt.unMark child
         ; parent)
in fun link(e, e') =
    (* pre: numChildren e = numChildren e' *)
   if Key.<(Elt.key e, Elt.key e')
      then linkPC(e, e')
   else linkPC(e', e)
end

fun unlink e = let val p = Elt.parent e
               in Elt.decNumChildren p
                  ; CircList.delete(Elt.child p, e)
                  ; Elt.clearParent e
                  ; Elt.unMark e
               end

local
   structure I = Int
   local open Real
   in val phi = (1.0 + (Real.sqrt 5.0)) / 2.0
      fun maxNumChildren h = floor(log(phi, fromInt(size h)))
   end
in
   fun consolidate h =
      (clearMin h ;
       if size h = 0 then ()
       else let val a = Array.new(maxNumChildren h + 1, NONE)
                fun insertIntoA e =
                    let val n = Elt.numChildren e
                    in case Array.sub(a,n) of
                        NONE => Array.update(a,n, SOME e)
                      | SOME e' => (Array.update(a,n, NONE)
                                    ; insertIntoA(link(e, e')))
                    end
            in CircList.deleteEach(roots h, insertIntoA)
               ; Array.foreach(a, fn NONE => () | SOME e => addRoot(h, e))
            end)
end

(*--------------------------------------------------------*)
(*            Constructors: empty, insert, new            *)
(*--------------------------------------------------------*)

fun empty() = T{size = ref 0,
                roots = CircList.empty(),
                min = Pointer.null()}

fun insertLazy(h, k, v) =
   let val e = Elt.new(k, v)
   in (incSize h ; addRoot(h, e) ; e)
   end

fun insertEager(h, k, v) =
    let val e = insertLazy(h, k, v)
    in (consolidate h ; e)
    end

fun newLazy kvs =
   let val h = empty()
   in (List.foreach(kvs, fn (k, v) => (insertLazy(h, k, v) ; ())) ;
       h)
   end

fun newEager kvs = let val h = newLazy kvs
                   in (consolidate h ; h)
                   end

(*--------------------------------------------------------*)
(*                       DeleteMin                        *)
(*--------------------------------------------------------*)

fun deleteMin h =
   let val m = min h
      val c = Elt.child m
      val rs = roots h
   in decSize h
      ; CircList.delete(rs, m)
      ; CircList.foreach(c, Elt.clearParent)
      ; CircList.splice(rs, c)
      ; consolidate h
      ; Elt.value m
   end

(*--------------------------------------------------------*)
(*                      DecreaseKey                       *)
(*--------------------------------------------------------*)

fun sift(_, e, _) = Elt.siftUp e

fun cut(h, e, k) =
   if Elt.hasParent e
      andalso Key.<(k, Elt.key(Elt.parent e))
      then let val rs = roots h
               fun cut e = if Elt.hasParent e
                              then let val p = Elt.parent e
                                   in unlink e
                                      ; CircList.insert(rs, e)
                                      ; if Elt.isMarked p
                                           then cut p
                                        else Elt.mark p
                                   end
                           else ()
           in cut e
           end
   else ()

fun decreaseKey(h, e, k) =
    if Key.>(k, Elt.key e) then Error.bug "decreaseKey"
    else (Elt.setKey(e, k); updateMin(h, e))

fun decreaseKeySift(h, e, k) =
   (decreaseKey(h, e, k)
    ; sift(h, e, k))

fun decreaseKeyCut(h, e, k) =
   (decreaseKey(h, e, k)
    ; cut(h, e, k))

(*--------------------------------------------------------*)
(*                         Delete                         *)
(*--------------------------------------------------------*)

fun delete(decreaseKey, h, e) =
   let val k = Elt.key e
   in decreaseKey(h, e, Key.smallest)
      ; deleteMin h
      ; Elt.setKey(e, k)
   end

fun deleteSift(h, e) = delete(decreaseKeySift, h, e)

fun deleteCut(h, e) = delete(decreaseKeyCut, h, e)

(*--------------------------------------------------------*)
(*                         Union                          *)
(*--------------------------------------------------------*)

fun union(h, h') =
    (setSize(h, size h + size h')
     ; CircList.splice(roots h, roots h'))

fun unionEager(h, h') = (union(h, h'); consolidate h)

fun unionLazy(h, h') =
   (union(h, h')
    ; updateMin(h, min h') handle Min => ())

(*--------------------------------------------------------*)
(*                         Output                         *)
(*--------------------------------------------------------*)

fun output(heap, outputValue, out) = Error.unimplemented "output"

(*--------------------------------------------------------*)
(*                    Well-Formed Test                    *)
(*--------------------------------------------------------*)
(*
local
   fun sizeInTree e = 1 + sizeInTrees (CircList.T (Elt.children e))
   and sizeInTrees l = CircList.fold l 0 (fn (e,n) => n + sizeInTree e)
   fun sizeInHeap h = sizeInTrees (roots h)
   fun findMin h =
      let val min = ref NONE
         fun updateMin e = (case !min of
                               NONE => min := SOME e
                             | SOME e' => if Key.<(Elt.key e, Elt.key e')
                                             then min := SOME e
                                          else ())
      in (CircList.foreach (roots h) updateMin ;
          case !min of
             SOME e => e
           | NONE => bug "findMin") 
      end
   fun isTreeWellFormed e =
      let fun isChildWellFormed e' = (Elt.equals(e, Elt.parent e')
                                      andalso Key.<=(Elt.key e, Elt.key e')
                                      andalso isTreeWellFormed e')
         val cs = CircList.T (Elt.children e)
      in Elt.numChildren e = CircList.length cs
         andalso CircList.forall cs isChildWellFormed
      end
in
   fun isFibonacciHeap h =
      CircList.forall (roots h) isTreeWellFormed
      andalso size h = sizeInHeap h
      andalso (isEmpty h
               orelse Key.equals(Elt.key (min h), Elt.key (findMin h)))
end
*)
end
