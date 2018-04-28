(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure UniqueSetRep =
   struct
      datatype 'a t = T of {elements: 'a list,
                            plist: PropertyList.t}

   end

functor UniqueSet (S: UNIQUE_SET_STRUCTS): UNIQUE_SET =
struct

open S

val _ = Assert.assert ("UniqueSet: cacheSize, bits", fn () => 
                       cacheSize >= 1 andalso bits >= 1)

type elements = Element.t list

structure Tree: sig
                   structure Set:
                      sig
                         type t

                         val equals: t * t -> bool
                         val toList: t -> elements
                         val plist: t -> PropertyList.t
                      end

                   type t

                   val new: unit -> t
                   val insert: t * elements -> Set.t
                   val size: t -> int
                end =
   struct
      structure Set  =
         struct
            open UniqueSetRep
            type t = Element.t t

            fun new elements = T {elements = elements,
                                  plist = PropertyList.new()}

            fun elements (T {elements, ...}) = elements
            fun plist (T {plist, ...}) = plist

            val toList = elements

            fun equals (s, s') = PropertyList.equals (plist s, plist s')
         end

      datatype node =
         Node of {element: Element.t,
                  isIn: t,
                  isNotIn: t}
       | Leaf of Set.t
      withtype t = node option ref

      fun new(): t = ref NONE

      fun size(t: t): int =
         case !t of
            NONE => 0
          | SOME(Leaf _) => 1
          | SOME(Node{isIn, isNotIn, ...}) => size isIn + size isNotIn

      fun contains(es, e) = List.exists(es, fn e' => Element.equals(e, e'))

      fun insert(tree, elements) =
         let
            fun loop tree =
               case !tree of
                  NONE => let val s = Set.new elements
                          in tree := SOME(Leaf s); s
                          end
                | SOME(Node{element, isIn, isNotIn}) =>
                  if contains(elements, element)
                     then loop isIn
                  else loop isNotIn
                | SOME(Leaf s') =>
                     let
                        fun loop arg =
                           case arg of
                              ([], []) => s' (* same set *)
                            | ([], x' :: _) =>
                                 let val s = Set.new elements
                                 in tree :=
                                    SOME(Node{element = x',
                                              isIn = ref(SOME(Leaf s')),
                                              isNotIn = ref(SOME(Leaf s))})
                                    ; s
                                 end
                            | (x :: xs, xs') =>
                                 let
                                    fun loop2(xs', accum) =
                                       case xs' of
                                          [] =>
                                             let val s = Set.new elements
                                             in tree :=
                                                SOME(Node{element = x,
                                                          isIn = ref(SOME(Leaf s)),
                                                          isNotIn =
                                                          ref(SOME(Leaf s'))})
                                                ; s
                                             end
                                        | x' :: xs' =>
                                             if Element.equals(x, x')
                                                then loop(xs, accum @ xs')
                                             else loop2(xs', x' :: accum)
                                 in loop2(xs', [])
                                 end
                     in loop(elements, Set.elements s')
                     end
         in loop tree
         end

   end

open Tree.Set

val tableSize = Int.pow (2, bits)

val maxIndex = tableSize - 1

val mask = Word.fromInt maxIndex

val table = Array.tabulate(tableSize, fn _ => Tree.new())

fun hashToIndex(w: Word.t): int = Word.toInt(Word.andb(w, mask))

fun intern(l: Element.t list, h: Word.t) =
   Tree.insert(Array.sub(table, hashToIndex h), l)

(* the hash of a set is the xorb of the hash of its members *)
fun hash(l: Element.t list) =
   List.fold(l, 0w0, fn (e, w) => Word.xorb(w, Element.hash e))

fun fromList l =
   let val l = List.fold(l, [], fn (x, l) =>
                         if List.exists(l, fn x' => Element.equals(x, x'))
                            then l
                         else x :: l)
   in intern(l, hash l)
   end

val empty = fromList []

fun isEmpty s = equals(s, empty)

fun foreach(s, f) = List.foreach(toList s, f)

fun singleton x = fromList [x]

val cacheHits: int ref = ref 0
val cacheMisses: int ref = ref 0

fun stats() = {hits = !cacheHits, misses = !cacheMisses}
fun reset() =
   (* need to clear out and reset the tables *)
   (cacheHits := 0
    ; cacheMisses := 0
    ; Int.for(0, tableSize, fn i => Array.update(table, i, Tree.new())))

(*      Int.foreach(0, maxIndex, fn i =>
                  let val n = Tree.size(Vector.sub(table, i))
                  in if n > 0
                        then Control.message(seq[Int.layout i,
                                                 str " -> ",
                                                 Int.layout n])
                     else ()
                  end)*)

local
   fun binary (oper: elements * elements -> elements) =
      let
         val cache = Array.new(cacheSize, NONE)
      in
         fn (s: t, s': t) =>
         let
            fun loop i =
               if i >= cacheSize
                  then
                     let
                        val s'' = fromList(oper(toList s, toList s'))
                        val () = Int.inc cacheMisses
                        val () =
                           Array.update (cache,
                                         Random.natLessThan cacheSize,
                                         SOME (s, s', s''))
                     in
                        s''
                     end
               else case Array.sub(cache, i) of
                  NONE => loop(i + 1)
                | SOME(s1, s1', s'') =>
                     if equals(s, s1) andalso equals(s', s1')
                        then (Int.inc cacheHits; s'')
                     else loop(i + 1)
         in loop 0
         end
      end

   val {+, -, intersect, layout, ...} =
      List.set{equals = Element.equals,
               layout = Element.layout}
in
   val op + = binary op +
   val op - = binary op -
   val op intersect = binary intersect

   val layout = layout o toList
end

(* val fromList = Trace.trace("fromList", List.layout Element.layout, layout) fromList *)

fun traceBinary (name, f) = Trace.trace2 (name, layout, layout, layout) f

val op + = traceBinary ("UniqueSet.+", op +)
val op - = traceBinary ("UniqueSet.-", op -)
val op intersect = traceBinary ("UniqueSet.intersect", intersect)

end
