(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor TypeOps (S: TYPE_OPS_STRUCTS): TYPE_OPS =
struct

open S

local
   open Tycon
in
   structure RealSize = RealSize
   structure WordSize = WordSize
end
datatype realSize = datatype RealSize.t
type tycon = Tycon.t
type wordSize = WordSize.t
   
local
   fun nullary tycon = con (tycon, Vector.new0 ())
in
   val bool = nullary Tycon.bool
   val exn = nullary Tycon.exn
   val intInf = nullary Tycon.intInf
   val real = RealSize.memoize (fn s => nullary (Tycon.real s))
   val thread = nullary Tycon.thread
   val word = WordSize.memoize (fn s => nullary (Tycon.word s))
end

val defaultReal = real RealSize.default
val defaultWord = word WordSize.default

local
   fun unary tycon t = con (tycon, Vector.new1 t)
in
   val array = unary Tycon.array
   val list = unary Tycon.list
   val reff = unary Tycon.reff
   val vector = unary Tycon.vector
   val weak = unary Tycon.weak
end

val word8 = word WordSize.byte
val word8Vector = vector word8
   
local
   fun binary tycon (t1, t2) = con (tycon, Vector.new2 (t1, t2))
in
   val arrow = binary Tycon.arrow
end

val arrow = Trace.trace ("arrow", Layout.tuple2 (layout, layout), layout) arrow

fun deUnaryOpt tycon t =
   case deConOpt t of
      SOME (c, ts) => if Tycon.equals (c, tycon)
			 then SOME (Vector.sub (ts, 0))
		      else NONE
    | _ => NONE

val deArrayOpt = deUnaryOpt Tycon.array
val deRefOpt = deUnaryOpt Tycon.reff
val deWeakOpt = deUnaryOpt Tycon.weak

fun deUnary tycon t =
   case deUnaryOpt tycon t of
      SOME t => t
    | NONE => Error.bug "deUnary"

val deArray = deUnary Tycon.array
val deRef = deUnary Tycon.reff
val deVector = deUnary Tycon.vector
val deWeak = deUnary Tycon.weak
   
fun tuple ts =
   if 1 = Vector.length ts
      then Vector.sub (ts, 0)
   else con (Tycon.tuple, ts)

val unit = tuple (Vector.new0 ())

fun deTupleOpt t =
   case deConOpt t of
      SOME (c, ts) => if Tycon.equals (c, Tycon.tuple) then SOME ts else NONE
    | NONE => NONE

val isTuple = Option.isSome o deTupleOpt

fun deTuple t =
   case deTupleOpt t of
      SOME t => t
    | NONE => Error.bug "detuple"

fun nth (t, n) = Vector.sub (deTuple t, n)

val unitRef = reff unit

fun deTycon t =
   case deConOpt t of
      SOME (c, _) => c
    | NONE => Error.bug "detycon"

fun deConConstOpt t =
   Option.map
   (deConOpt t, fn (c, ts) =>
    (c, Vector.map (ts, fn t =>
		    case deConOpt t of
		       SOME (c, _) => c
		     | NONE => Error.bug "deConConstOpt")))

fun deConConst t =
   case deConOpt t of
      NONE => Error.bug "deConConst"
    | SOME (c, ts) => (c, Vector.map (ts, fn t =>
				      case deConOpt t of
					 NONE => Error.bug "deConConst"
				       | SOME (c, _) => c))


fun deArrowOpt t =
   case deConOpt t of
      SOME (c, ts) => if Tycon.equals (c, Tycon.arrow)
			       then SOME (Vector.sub (ts, 0), Vector.sub (ts, 1))
			    else NONE
    | _ => NONE

fun deArrow t =
   case deArrowOpt t of
      SOME x => x
    | NONE => Error.bug "Type.deArrow"

val dearrow =
   Trace.trace ("deArrow", layout, Layout.tuple2 (layout, layout)) deArrow

val arg = #1 o dearrow
val result = #2 o dearrow

end
