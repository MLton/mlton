(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Relation0 =
struct

datatype t = datatype order

val equals: t * t -> bool = op =

val toString =
   fn EQUAL => "Equal"
    | GREATER => "Greater"
    | LESS => "Less"

fun lessEqual {<, equals} = 
   let
      fun a > b = b < a
      fun a <= b = a < b orelse equals (a, b)
      fun a >= b = b < a orelse equals (b, a)
      fun compare (a, b) = if a < b then LESS
                         else if equals (a, b) then EQUAL
                              else GREATER
      fun min (x, y) = if x < y then x else y
      fun max (x, y) = if x < y then y else x
   in {> = op >, <= = op <=, >= = op >=,
       compare = compare, min = min, max = max}
   end

fun compare c =
   let fun equals (x, y) = (case c (x, y) of
                             EQUAL => true
                           | _ => false)
      fun x < y = (case c (x, y) of
                      LESS => true
                    | _ => false)
      fun x <= y = (case c (x, y) of
                        LESS => true
                      | EQUAL => true
                      | _ => false)
      fun x > y = (case c (x, y) of
                      GREATER => true
                    | _ => false)
      fun x >= y = (case c (x, y) of
                       GREATER => true
                     | EQUAL => true
                     | _ => false)
      fun max (x, y) = (case c (x, y) of
                         GREATER => x
                       | _ => y)
      fun min (x, y) = (case c (x, y) of
                         GREATER => y
                       | _ => x)
   in {equals = equals,
       < = op <, > = op >, <= = op <=, >= = op >=,
       min = min, max = max}
   end

end
