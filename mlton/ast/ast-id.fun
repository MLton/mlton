(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor AstId (S: AST_ID_STRUCTS): AST_ID =
struct

open S

datatype t = T of {string: String.t,
		   hash: Word.t,
		   region: Region.t}
type obj = t
type node' = string
type region = Region.t

fun makeRegion (s, r) = T {string = s,
			  hash = String.hash s,
			  region = r}

fun makeRegion' (s, x, y) =
   makeRegion (s, Region.make {left = x, right = y})

fun make s = makeRegion (s, Region.bogus)

val fromString = make

fun dest (T {string, region, ...}) = (string, region)

val bogus = fromString "<bogus>"

fun toString (T {string, ...}) = string
val node = toString

val layout = String.layout o toString

fun region (T {region, ...}) = region

fun hash (T {hash, ...}) = hash
val hash = Trace.trace ("AstId.hash", layout, Word.layout) hash

(* val left = Region.left o region *)
(* val right = Region.left o region *)

local
   fun binary (f: string * string -> 'a) (x :t, y: t): 'a =
      f (toString x, toString y)
in val op < = binary String.<
   val op > = binary String.>
   val op >= = binary String.>=
   val op <= = binary String.<=
   val compare = binary String.compare
end

fun max (x, y) = if x > y then x else y
fun min (x, y) = if x < y then x else y

val equals = fn (T {string = s, hash = h, ...},
		 T {string = s', hash = h', ...}) =>
   h = h' andalso String.equals (s, s')

val equals = Trace.trace2 ("AstId.equals", layout, layout, Bool.layout) equals

fun unbound (x: t): unit =
   Control.error
   (region x,
    let open Layout
    in seq [str "unbound ", str className, str " ", layout x]
    end,
    Layout.empty)

end
