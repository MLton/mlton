(* Copyright (C) 2009,2014,2017-2020 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Prod (S: PROD_STRUCTS): PROD =
struct

open S

datatype 'a t = T of {elt: 'a, isMutable: bool} vector

fun dest (T p) = p

val make = T

fun empty () = T (Vector.new0 ())

local
   fun new1 {elt, isMutable} = T (Vector.new1 {elt = elt, isMutable = isMutable})
in
   fun new1Immutable elt = new1 {elt = elt, isMutable = false}
   fun new1Mutable elt = new1 {elt = elt, isMutable = true}
end

fun fold (p, b, f) =
   Vector.fold (dest p, b, fn ({elt, ...}, b) => f (elt, b))

fun foreach (p, f) = Vector.foreach (dest p, f o #elt)

fun isEmpty p = Vector.isEmpty (dest p)

fun allAreImmutable (T v) = Vector.forall (v, not o #isMutable)
fun allAreMutable (T v) = Vector.forall (v, #isMutable)
fun someIsImmutable (T v) = Vector.exists (v, not o #isMutable)
fun someIsMutable (T v) = Vector.exists (v, #isMutable)

fun sub (T p, i) = Vector.sub (p, i)

fun elt (p, i) = #elt (sub (p, i))

fun length p = Vector.length (dest p)

val equals: 'a t * 'a t * ('a * 'a -> bool) -> bool =
   fn (p1, p2, equals) =>
   Vector.equals (dest p1, dest p2,
                  fn ({elt = e1, isMutable = m1},
                      {elt = e2, isMutable = m2}) =>
                  m1 = m2 andalso equals (e1, e2))

fun layout (p, layoutElt) =
   let
      open Layout
   in
      seq [str "(",
           (mayAlign o separateRight)
           (Vector.toListMap (dest p, fn {elt, isMutable} =>
                              if isMutable
                                 then seq [layoutElt elt, str " mut"]
                                 else layoutElt elt),
            ","),
           str ")"]
   end

fun parse (parseElt: 'a Parse.t): 'a t Parse.t =
   let
      open Parse
      (* infix declarations for Parse.Ops *)
      infix  1 <|> >>=
      infix  3 <*> <* *>
      infixr 4 <$> <$$> <$$$> <$$$$> <$ <$?>
   in
      make <$>
      vector (parseElt >>= (fn elt =>
              optional (kw "mut") >>= (fn isMutable =>
              pure {elt = elt, isMutable = Option.isSome isMutable})))
   end

val map: 'a t * ('a -> 'b) -> 'b t =
   fn (p, f) =>
   make (Vector.map (dest p, fn {elt, isMutable} =>
                     {elt = f elt,
                      isMutable = isMutable}))

val keepAllMap: 'a t * ('a -> 'b option) -> 'b t =
   fn (p, f) =>
   make (Vector.keepAllMap (dest p, fn {elt, isMutable} =>
                            Option.map (f elt, fn elt =>
                                        {elt = elt,
                                         isMutable = isMutable})))
end
