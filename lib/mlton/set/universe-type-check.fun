(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                             SetCheck                              *)
(*-------------------------------------------------------------------*)

functor UniverseTypeCheck(U: UNIVERSE): UNIVERSE =
struct

structure E = U.Element
structure T = Type

open U
structure L = List

fun typeOf s =
    L.foldl(toList s,
            T.EmptySet,
            fn (t, x: E.t) =>
            T.Set.combine(t, T.Set(typeOfElt x)))
and typeOfElt(Base _) = T.Base
  | typeOfElt(Pair(x, y)) = T.Pair(typeOfElt x, typeOfElt y)
  | typeOfElt(Set s) = T.EltSet(typeOf s)

fun setElt (name, f) (s, x) = 
   (Error.assert[(T.areCompatibleSetElt(typeOf s, typeOfElt x),
                 name ^ ": incompatible set and element")] ;
    f(s, x))

val add = setElt("UniverseTypeCheck.add", add)
val remove = setElt("UniverseTypeCheck.remove", remove)
val contains = setElt("UniverseTypeCheck.contains", contains)

fun setSet (name, f) (s, s') =
    (Error.assert[(T.Set.areCompatible(typeOf s, typeOf s'),
                  name ^ "incompatible sets")] ;
     f(s, s'))

val op - = setSet("UniverseTypeCheck.difference", op -)
val op + = setSet("UniverseTypeCheck.union", op +)
val intersect = setSet("UniverseTypeCheck.intersect", intersect)
val equals = setSet("UniverseTypeCheck.equals", op =)
val op <= = setSet("UniverseTypeCheck.<=", op <=)
val op >= = setSet("UniverseTypeCheck.>=", op >=)
val op < = setSet("UniverseTypeCheck.<", op <)
val op > = setSet("UniverseTypeCheck.>", op >)

fun isReasonable s = (typeOf s ;
                      true)
    handle T.Incompatible => false

fun returnSet (name, f) a =
    let val s = f a
    in (Error.assert[(isReasonable s, name ^ ": invalid set")] ;
        s)
    end

val replace = returnSet("UniverseTypeCheck.replace", replace)
val map = returnSet("UniverseTypeCheck.map", map)
val fromList = returnSet("UniverseTypeCheck.fromList", fromList)

fun lookup(s, x) =
    (case typeOf s of
         T.EmptySet => NONE
       | T.Set(T.Pair(x', _)) =>
             (T.Elt.combine(x', typeOfElt x) ;
              U.lookup(s, x))
       | _ => Error.error "UniverseTypeCheck.lookup")
         handle T.Incompatible => Error.error "UniverseTypeCheck.lookup"

fun update(s, x, y) =
    case typeOf s of
        T.EmptySet => U.update(s, x, y)
      | T.Set t =>
           (Error.assert[(T.Elt.areCompatible
                         (t, T.Pair(typeOfElt x, typeOfElt y)),
                         "update: incompatible pairs")] ;
            U.update(s, x, y))

val updateSet = setSet("UniverseTypeCheck.updateSet", updateSet)

end
