(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                             SetEqual                              *)
(*-------------------------------------------------------------------*)

functor UnorderedUniverse(B: T): UNIVERSE =
struct

structure B = B
structure O = Outstream
structure L = ListSet.L

structure Rep =
   struct
      datatype elt =
         Base of B.t
       | Pair of elt * elt
       | Set of elt ListSet.t

      type t = elt

      fun makeEqual equalSet =
         let fun equalElt(Base b, Base b') = B.equals(b, b')
               | equalElt(Pair(x, y), Pair(x', y')) =
                 equalElt(x, x') andalso equalElt(y, y')
               | equalElt(Set s, Set s') = equalSet(s, s')
               | equalElt _ = false
         in equalElt
         end

      fun makeOutput outputSet =
         let fun outputElt(Base b, out) = B.output(b, out)
               | outputElt(Pair(x, y), out) =
                 let val print = O.outputc out
                 in (print "(" ;
                     outputElt(x, out) ;
                     print ", " ;
                     outputElt(y, out) ;
                     print ")")
                 end
               | outputElt(Set s, out) = outputSet(s, out)
         in outputElt
         end
   end

structure S = UnorderedSetMain(Rep)

open Rep S

fun toBase(Base b) = b
  | toBase _ = Error.error "UnorderedUniverse.toBase"
fun toPair(Pair p) = p
  | toPair _ = Error.error "UnorderedUniverse.toPair"
fun toSet(Set s) = s
  | toSet _ = Error.error "UnorderedUniverse.toSet"

fun cross(sx, sy) =
    let val ys = toList sy
    in fromList(L.foldl
                (toList sx, [],
                 fn (ps, x) => L.mapAppend(ys, fn y => Pair(x, y), ps)))
    end

fun project1 s = replace(s,
                         fn Pair(x, _) => SOME x
                          | _ => Error.error "UnorderedUniverse.project1")
fun project2 s = replace(s,
                         fn Pair(_, y) => SOME y
                          | _ => Error.error "UnorderedUniverse.project2")

fun update (c, x, y) =
    let fun update[] = [Pair(x, y)]
          | update((Pair(x', y')) :: ps) =
            if E.equals(x, x') then (Pair(x, y)) :: ps
            else (Pair(x', y')) :: (update ps)
          | update _ = Error.error "UnorderedUniverse.update"
    in fromList(update(toList c))
    end

fun updateSet(c, c') =
    L.foldl(toList c', c,
            fn (c, Pair(x, y)) => update(c, x, y)
             | _ => Error.error "UnorderedUniverse.updateSet")

fun lookup (c, x) =
    let fun lookup [] = NONE
          | lookup (Pair(x', y) :: ps) =
            if E.equals(x, x') then SOME y else lookup ps
          | lookup _ = Error.error "UnorderedUniverse.lookup"
    in lookup(toList c)
    end

fun Union s = L.foldl(toList s, empty,
                      fn (s', Set s) => s + s'
                       | _ => Error.error "UnorderedUniverse.Union")
val Union = Trace.trace("UnorderedUniverse.Union", output, output) Union
(*
fun Cross s = listTo(L.map(L.cross(L.map(toList s,
                                         toList o Elt.toSet)),
                            Set o listTo))
*)
end
