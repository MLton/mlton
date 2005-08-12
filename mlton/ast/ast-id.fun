(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstId (S: AST_ID_STRUCTS): AST_ID =
struct

open S

datatype t = T of {name: Symbol.t,
		   region: Region.t}

type obj = t
type node' = Symbol.t

local
   fun make f (T r) = f r
in
   val name = make #name
   val region = make #region
end

val node = name
val toSymbol = name

fun makeRegion (s, r) = T {name = s,
			   region = r}

val fromSymbol = makeRegion

fun makeRegion' (s, x, y) =
   makeRegion (s, Region.make {left = x, right = y})

fun dest (T {name, region, ...}) = (name, region)

val bogus = makeRegion (Symbol.bogus, Region.bogus)

fun isAlphaNumeric id =
   let
      val c = String.sub (Symbol.toString (name id), 0)
   in
      Char.isAlphaNum c orelse c = #"'"
   end

val isSymbolic = not o isAlphaNumeric

val toString = Symbol.toString o name

val layout = String.layout o toString

val hash = Symbol.hash o name
val hash = Trace.trace ("AstId.hash", layout, Word.layout) hash

(* val left = Region.left o region *)
(* val right = Region.left o region *)

local
   fun binary (f: string * string -> 'a) (x :t, y: t): 'a =
      f (toString x, toString y)
in
   val op < = binary String.<
   val op > = binary String.>
   val op >= = binary String.>=
   val op <= = binary String.<=
   val compare = binary String.compare
end

fun equals (x, x') = Symbol.equals (name x, name x')

val equals = Trace.trace2 ("AstId.equals", layout, layout, Bool.layout) equals

end
