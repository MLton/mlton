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
   structure IntSize = IntSize
   structure RealSize = RealSize
   structure WordSize = WordSize
end
datatype intSize = datatype IntSize.t
datatype realSize = datatype RealSize.t
type tycon = Tycon.t
datatype wordSize = datatype WordSize.t
   
local
   fun nullary tycon = con (tycon, Vector.new0 ())
in
   val bool = nullary Tycon.bool
   val exn = nullary Tycon.exn
   val int = IntSize.memoize (fn s => nullary (Tycon.int s))
   val intInf = nullary Tycon.intInf
   val preThread = nullary Tycon.preThread
   val real = RealSize.memoize (fn s => nullary (Tycon.real s))
   val thread = nullary Tycon.thread
   val word = WordSize.memoize (fn s => nullary (Tycon.word s))
end

val defaultInt = int IntSize.default
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

val word8 = word W8
val word8Vector = vector word8
   
local
   fun binary tycon (t1, t2) = con (tycon, Vector.new2 (t1, t2))
in
   val arrow = binary Tycon.arrow
end

val arrow = Trace.trace ("arrow", Layout.tuple2 (layout, layout), layout) arrow

fun deUnaryOpt tycon t =
   case deconOpt t of
      SOME (c, ts) => if Tycon.equals (c, tycon)
			 then SOME (Vector.sub (ts, 0))
		      else NONE
    | _ => NONE

val dearrayOpt = deUnaryOpt Tycon.array
val derefOpt = deUnaryOpt Tycon.reff
val deweakOpt = deUnaryOpt Tycon.weak

fun deUnary tycon t =
   case deUnaryOpt tycon t of
      SOME t => t
    | NONE => Error.bug "deUnary"

val dearray = deUnary Tycon.array
val deref = deUnary Tycon.reff
val devector = deUnary Tycon.vector
val deweak = deUnary Tycon.weak
   
fun tuple ts =
   if 1 = Vector.length ts
      then Vector.sub (ts, 0)
   else con (Tycon.tuple, ts)

val unit = tuple (Vector.new0 ())

fun detupleOpt t =
   case deconOpt t of
      SOME (c, ts) => if Tycon.equals (c, Tycon.tuple) then SOME ts else NONE
    | NONE => NONE

val isTuple = Option.isSome o detupleOpt

fun detuple t =
   case detupleOpt t of
      SOME t => t
    | NONE => Error.bug "detuple"

fun nth (t, n) = Vector.sub (detuple t, n)

val unitRef = reff unit

fun detycon t =
   case deconOpt t of
      SOME (c, _) => c
    | NONE => Error.bug "detycon"

fun deconConstOpt t =
   case deconOpt t of
      SOME (c, ts) => SOME (c, Vector.map (ts, fn t =>
					   case deconOpt t of
					      SOME (c, _) => c
					    | NONE => Error.bug "deconConstOpt"))
    | NONE => NONE
fun deconConst t =
   case deconOpt t of
      SOME (c, ts) => (c, Vector.map (ts, fn t =>
				      case deconOpt t of
					 SOME (c, _) => c
				       | NONE => Error.bug "deconConst"))
    | NONE => Error.bug "deconConst"


fun dearrowOpt t =
   case deconOpt t of
      SOME (c, ts) => if Tycon.equals (c, Tycon.arrow)
			       then SOME (Vector.sub (ts, 0), Vector.sub (ts, 1))
			    else NONE
    | _ => NONE

fun dearrow t =
   case dearrowOpt t of
      SOME x => x
    | NONE => Error.bug "Type.dearrow"

val dearrow =
   Trace.trace ("dearrow", layout, Layout.tuple2 (layout, layout)) dearrow

val arg = #1 o dearrow
val result = #2 o dearrow

end
