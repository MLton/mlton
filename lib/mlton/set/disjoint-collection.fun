(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                        Disjoint Collection                        *)
(*-------------------------------------------------------------------*)

functor DisjointCollection(): DISJOINT_COLLECTION =
struct

structure S = DisjointSet
structure CL = CircularList
structure D = SimpleDoublyLinkedElement

structure Value :
   sig
      type 'a t
      val new: '1a -> '1a t
      val value: 'a t -> 'a
      val elt: 'a t -> 'a t S.t D.t
      val set: 'a t -> 'a t S.t
      val copy: 'a t * 'a -> 'a t
   end =
   struct
      datatype 'a t = T of {value: 'a,
                            elt: 'a t S.t D.t option ref}

      fun value(T{value, ...}) = value

      fun elt(T{elt=ref(SOME d), ...}) = d
        | elt _ = Error.error "DisjointCollection.Value.elt"

      fun set v = D.value(elt v)

      fun new v = let val r = ref NONE
                      val v = T{value = v, elt = r}
                      val d = D.new(S.singleton v)
                  in (r := SOME d ;
                      v)
                  end
      fun copy(T{elt, ...}, v) = T{value = v, elt = elt}
   end
structure V = Value

structure S =
   struct
      type 'a t = 'a V.t S.t

      fun value s = V.value(S.value s)

      fun elt s = V.elt(S.value s)

      val representative = S.representative
      val isRepresentative = S.isRepresentative
      val union = S.union

      fun setValue(s, v) = S.setValue(s, V.copy(S.value s, v))

      val equals = S.equals
   end

(* ------------------------------------------------- *)
(*                     Datatype                      *)
(* ------------------------------------------------- *)

datatype 'a t = T of {sets: 'a S.t CL.t,
                      numSets: int ref}

fun sets (T{sets, ...}) = sets
fun numSetsRef (T{numSets, ...}) = numSets
fun numSets c = !(numSetsRef c)
fun incNumSets c = numSetsRef c := numSets c + 1
fun decNumSets c = numSetsRef c := numSets c - 1

fun empty() = T{sets = CL.empty(),
                numSets = ref 0}

fun addSingleton(c, v) =
   let val v = V.new v
   in (incNumSets c ;
       CL.insert(sets c, V.elt v) ;
       V.set v)
   end

fun new vs = let val c = empty()
             in (c, List.map(vs, fn v => addSingleton(c, v)))
             end

fun randomSet(T{sets, ...}) = D.value(CL.first sets)

fun random c = S.value(randomSet c)

fun union(c, s, s') =
    let val r = S.representative s
       val d = S.elt r
       val r' = S.representative s'
       val d' = S.elt r'
    in if S.equals(r, r') then ()
       else (decNumSets c ;
             S.union(r, r') ;
             CL.delete(sets c,
                       if S.isRepresentative r then d' else d))
    end

end

structure DisjointCollection = DisjointCollection()
