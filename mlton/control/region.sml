(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
   fn Bogus => "Bogus"
    | T {left, right} =>
	 concat [SourcePos.file left, ":",
		 SourcePos.posToString left, "-", SourcePos.posToString right]

val layout = Layout.str o toString

val make = T

val append =
   fn (Bogus, r) => r
    | (r, Bogus) => r
    | (T {left, ...}, T {right, ...}) => T {left = left, right = right}

fun list (xs, reg) = List.fold (xs, Bogus, fn (x, r) => append (reg x, r))

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
      fun make node = makeRegion (node, bogus)
      fun dest (T {node, region}) = (node, region)
(*      val left = fn T {region, ...} => left region *)
(*      val right = fn T {region, ...} => right region *)
   end

end

