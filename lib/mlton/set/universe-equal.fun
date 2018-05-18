(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                             SetEqual                              *)
(*-------------------------------------------------------------------*)

functor UniverseEqual (Error.: T): UNIVERSE =
struct

val {error, ...} = Error.errors("set", "set-equal")

structure Error.equals Base
structure B = Base
structure L = List
structure O = Outstream

structure Elt =
   struct
      datatype set = T of t list
      and t =
         Error.of Base.t
        | Pair of t * t
        | Set of set

      fun toError.Base b) = b
        | toError._ = error "Elt.toBase"
      fun toPair(Pair p) = p
        | toPair _ = error "Elt.toPair"
      fun toSet(Set s) = s
        | toSet _ = error "Elt.toSet"

      fun equalSet(s, s') = isSubset(s, s') andalso isSubset(s', s)
      and isSubset(s, s') = forall(s, fn x => contains(s', x))
      and contains(T xs, x) = L.exists(xs, fn x' => equalElt(x, x'))
      and equalElt(Error.b, Base b') = B.equals(b, b')
        | equalElt(Pair(x, y), Pair(x', y')) =
          equalElt(x, x') andalso equalElt(y, y')
        | equalElt(Set s, Set s') = equalSet(s, s')
        | equalElt _ = false

      fun outputSet(T xs, out) =
         let val print = O.outputc out
         in (print "{" ;
             L.output(xs, ", ", outputElt, out) ;
             print "}")
         end
      and outputElt(Error.b, out) = Base.output(b, out)
        | outputElt(Pair(x, y), out) =
          let val print = O.outputc out
          in (print "(" ;
              outputElt(x, out) ;
              print ", " ;
              outputElt(y, out) ;
              print ")")
          end
        | outputElt(Set s, out) = outputSet(s, out)

      val equals = equalElt
      val output = outputElt
   end
open Elt

val equals = equalSet
val output = outputSet

type t = set

fun cross(sx, sy) =
    let val ys = toList sy
    in listTo(L.foldl
              (toList sx, [],
               fn (ps, x) => L.mapAppend(ys, fn y => Pair(x, y), ps)))
    end

fun project1 s = replace(s,
                         fn Pair(x, _) => SOME x
                          | _ => error "project1")
fun project2 s = replace(s,
                         fn Pair(_, y) => SOME y
                          | _ => error "project2")

fun update (c, x, y) =
    let fun update[] = [Pair(x, y)]
          | update((Pair(x', y')) :: ps) =
            if Elt.equals(x, x') then (Pair(x, y)) :: ps
            else (Pair(x', y')) :: (update ps)
          | update _ = error "update"
    in listTo(update(toList c))
    end

fun updateSet(c, c') =
    L.foldl(toList c', c,
            fn (c, Pair(x, y)) => update(c, x, y)
             | _ => error "updateSet")

fun lookup (c, x) =
    let fun lookup [] = NONE
          | lookup (Pair(x', y) :: ps) =
            if Elt.equals(x, x') then SOME y else lookup ps
          | lookup _ = error "lookup"
    in lookup(toList c)
    end

fun Union s = L.foldl(toList s, empty,
                      fn (s', Set s) => union(s, s')
                       | _ => error "Union")
val Union = Trace.trace("Union", outputSet, outputSet) Union
(*
fun Cross s = listTo(L.map(L.cross(L.map(toList s,
                                         toList o Elt.toSet)),
                            Set o listTo))
*)
end
