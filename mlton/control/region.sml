(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Region: REGION =
struct

datatype t =
   Bogus
 | T of {left: SourcePos.t,
         right: SourcePos.t}

val bogus = Bogus

local
   fun make f r =
      case r of
         Bogus => NONE
       | T r => SOME (f r)
in
   val left = make #left
   val right = make #right
end

val extendRight =
   fn (Bogus, _) => Bogus
    | (T {left, ...}, right) => T {left = left, right = right}

val toString =
   fn Bogus => SourcePos.toString (SourcePos.bogus)
    | T {left, right} =>
         if SourcePos.isBogus left
            orelse SourcePos.isBogus right
            orelse not (SourcePos.fileEquals (left, right))
            then SourcePos.toString left
            else concat [SourcePos.toString left, "-",
                         SourcePos.posToString right]

val layout = Layout.str o toString

val make = T

val append =
   fn (Bogus, r) => r
    | (r, Bogus) => r
    | (T {left, ...}, T {right, ...}) => T {left = left, right = right}

fun compare (r, r') =
   case (left r, left r') of
      (NONE, NONE) => EQUAL
    | (NONE, _) => LESS
    | (_, NONE) => GREATER
    | (SOME p, SOME p') => SourcePos.compare (p, p')

val compare =
   Trace.trace2 ("Region.compare", layout, layout, Relation.layout) compare

fun equals (r, r') = compare (r, r') = EQUAL

fun r <= r' =
   case compare (r, r') of
      EQUAL => true
    | GREATER => false
    | LESS => true

structure Wrap =
   struct
      type region = t
      datatype 'a t = T of {node: 'a,
                            region: region}

      fun node (T {node, ...}) = node
      fun region (T {region, ...}) = region
      fun makeRegion (node, region) = T {node = node, region = region}
      fun makeRegion' (node, left, right) = T {node = node,
                                               region = make {left = left,
                                                              right = right}}

      fun dest (T {node, region}) = (node, region)
   end

end
