(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Region: REGION = 
struct

datatype t = T of {left: int, right: int}

fun left (T {left, ...}) = left
fun right (T {right, ...}) = right
   
val bogus = T {left = ~1, right = ~1}

fun list (xs, reg) =
   case xs of	       
      [] => bogus
    | x :: _ => T {left = left (reg x),
		   right = right (reg (List.last xs))}

val isBogus =
   fn (T {left = ~1, ...}) => true
    | _ => false

fun layout (T {left, right}) =
   let
      open Layout
   in
      if left = ~1
	 then str "<bogus>"
      else seq [Int.layout left, str "-", Int.layout right]
   end

val make = T

structure Wrap =
   struct
      type region = t
      datatype 'a t = T of {node: 'a, region: region}

      fun node (T {node, ...}) = node
      fun region (T {region, ...}) = region
      fun makeRegion (node, region) = T {node = node, region = region}
      fun makeRegion' (node, left, right) = T {node = node,
					   region = make {left = left,
							 right = right}}
      fun make node = makeRegion (node, bogus)
      fun dest (T {node, region}) = (node, region)
      val left = fn T {region, ...} => left region
      val right = fn T {region, ...} => right region
   end
   
end
