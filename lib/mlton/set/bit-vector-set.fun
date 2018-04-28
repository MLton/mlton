(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor BitVectorSet (Element : sig 
                                  include T
                                  val fromInt: int -> t
                                  val size: int
                                  val toInt: t -> int
                                end) : SET =
struct
   structure Element = Element

   structure Bin :> sig 
                      eqtype t 
                      val binSize: int 
                      val difference : t * t -> t
                      val empty : t
                      val equals : t * t -> bool
                      val fold : t * 'a * (int * 'a -> 'a) -> 'a
                      val intersect : t * t -> t
                      val singleton : int -> t
                      val union : t * t -> t
                    end = 
   struct 
     open Word
     val binSize = wordSize

     val equals : t * t -> bool = op =
     val empty : t = 0wx0
     fun singleton i = <<(0wx1, Word.fromInt i)
     val difference = fn (b1, b2) => andb (b1, notb b2)
     val intersect = fn (b1, b2) => andb (b1, b2)
     val union = fn (b1, b2) => orb (b1, b2)
     fun fold (w, a, f)
       = let
           fun loop (w, a, i) 
             = if Int.< (i, wordSize)
                 then let
                        val a = if andb (w, 0wx1) <> 0wx0
                                  then f (i, a)
                                  else a
                      in
                        loop (>>(w, 0wx1), a, Int.+ (i, 1))
                      end
                 else a
         in
           loop (w, a, 0)
         end
   end
   type bin = Bin.t
   type t = bin vector 
   type index = int (* position in t *)
   type slot = int (* position in bin *)
   type pos = index * slot

   val ltPos : pos * pos -> bool
     = fn ((index1, slot1), (index2, slot2)) =>
       index1 < index2 orelse
       (index1 = index2 andalso slot1 < slot2)

   val intToPos : int -> pos
     = fn pos => (Int.quot (pos, Bin.binSize), Int.rem (pos, Bin.binSize))
   val posToInt : pos -> int
     = fn (index, slot) => index * Bin.binSize + slot
   val slotToBin : slot -> bin = fn slot => Bin.singleton slot

   val eltToPos = intToPos o Element.toInt
   fun eltToPosBin x = let val pos as (index, slot) = eltToPos x
                       in (pos, slotToBin slot)
                       end
   val posToElt = Element.fromInt o posToInt

   val maxPos as (maxIndex,maxSlot) = intToPos (Element.size - 1)

   val empty : t = Vector.new (maxIndex + 1, Bin.empty)
   fun isEmpty (v : t) = Vector.forall (v, fn b => b = Bin.empty)
   fun singleton x = let val ((index,_), bin) = eltToPosBin x
                     in Vector.tabulate (maxIndex + 1, fn i =>
                                         if i = index
                                           then bin
                                           else Bin.empty)
                     end
   fun contains (v, x) = let val ((index,_), bin) = eltToPosBin x
                         in Bin.intersect (bin, Vector.sub (v, index)) <> Bin.empty
                         end
   fun add (v, x) = let val ((index, _), bin) = eltToPosBin x
                    in Vector.mapi (v, fn (i, b) =>
                                    if i = index
                                      then Bin.union (bin, b)
                                      else b)
                    end
   fun remove (v, x) = let val ((index, _), bin) = eltToPosBin x
                       in Vector.mapi (v, fn (i, b) =>
                                       if i = index
                                         then Bin.difference (b, bin)
                                         else b)
                       end
   fun difference (v1, v2) 
     = Vector.map2 (v1, v2, fn (b1, b2) => Bin.difference (b1, b2))
   fun intersect (v1, v2) 
     = Vector.map2 (v1, v2, fn (b1, b2) => Bin.intersect (b1, b2))
   fun union (v1, v2) 
     = Vector.map2 (v1, v2, fn (b1, b2) => Bin.union (b1, b2))
   fun unions ss = List.fold (ss, empty, union)
   fun equals (v1, v2) = Vector.equals (v1, v2, Bin.equals)
   fun isSubsetEq (v1, v2)
     = Exn.withEscape
       (fn escape =>
        Vector.fold2
        (v1, v2, true, fn (b1, b2, a) =>
         if Bin.difference (b1, b2) = Bin.empty
           then a
           else escape false))
   fun isSubset (s1, s2) = isSubsetEq (s1, s2) andalso not (equals (s1, s2))
   fun isSupersetEq (s1, s2) = isSubsetEq (s2, s1)
   fun isSuperset (s1, s2) = isSubset (s2, s1)

   fun areDisjoint (v1, v2)
     = Exn.withEscape
       (fn escape =>
        Vector.fold2
        (v1, v2, true, fn (b1, b2, a) =>
         if Bin.intersect(b1, b2) = Bin.empty
           then a
           else escape false))


   fun fold (v, a, f) 
     = Vector.foldi
       (v, a, fn (i, b, a) =>
        let
          val check = if i < maxIndex
                        then fn s => true
                        else fn s => s < maxSlot
        in
          Bin.fold (b, a, fn (s, a) => if check s
                                         then f (posToElt (i, s), a)
                                         else a)
        end)
   fun foreach (s, f) = fold (s, (), fn (x, ()) => f x)
   fun peekGen (s, no, f)
     = Exn.withEscape
       (fn escape =>
        (foreach (s, fn x => 
                  case f x 
                    of NONE => ()
                     | SOME yes => escape yes)
         ; no ()))
   fun exists (s, p) = peekGen (s, 
                                fn () => false, 
                                fn x => if p x then SOME true else NONE)
   fun forall (s, p) = not (exists (s, not o p))

   fun subsetSize (s, p)
     = fold (s, 0 : int, fn (x, a) => if p x then a + 1 else a)
   fun size s = subsetSize (s, fn _ => true)

   fun replace (s, f) = fold(s, empty, fn (x, s) =>
                             case f x
                               of NONE => s
                                | SOME x' => add (s, x'))
   fun map (s, f) = replace (s, fn x => SOME (f x))
   fun subset (s, p) = replace (s, fn x => if p x then SOME x else NONE)
   fun partition (s, p) = let val yes = subset (s, p)
                          in {yes = yes, no = difference (s, yes)}
                          end

   fun fromList l = List.fold (l, empty, fn (x, s) => add (s, x))
   fun toList s = fold (s, nil, op ::)
   fun layout s = List.layout Element.layout (toList s)

   val op + = union
   val op - = difference
   val op < = isSubset
   val op <= = isSubsetEq
   val op > = isSuperset
   val op >= = isSupersetEq

   fun power _ = Error.unimplemented "BitVectorSet.power"
   fun subsets _ = Error.unimplemented "BitVectorSet.subsets"
end
