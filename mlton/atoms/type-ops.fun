(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor TypeOps (S: TYPE_OPS_STRUCTS): TYPE_OPS =
struct

open S

type tycon = Tycon.t
   
local
   fun nullary tycon = con (tycon, Vector.new0 ())
in
   val bool = nullary Tycon.bool
   val char = nullary Tycon.char
   val exn = nullary Tycon.exn
   val int = nullary Tycon.int
   val intInf = nullary Tycon.intInf
   val preThread = nullary Tycon.preThread
   val real = nullary Tycon.real
   val string = nullary Tycon.string
   val thread = nullary Tycon.thread
   val word = nullary Tycon.word
   val word8 = nullary Tycon.word8

   val defaultInt = nullary Tycon.defaultInt
   val defaultWord = nullary Tycon.defaultWord
end

local
   fun unary tycon t = con (tycon, Vector.new1 t)
in
   val array = unary Tycon.array
   val vector = unary Tycon.vector
   val list = unary Tycon.list
   val reff = unary Tycon.reff
end

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

fun deUnary tycon t =
   case deUnaryOpt tycon t of
      SOME t => t
    | NONE => Error.bug "deUnary"

val dearray = deUnary Tycon.array
val devector = deUnary Tycon.vector
val deref = deUnary Tycon.reff
   
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
