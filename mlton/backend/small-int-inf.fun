(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor SmallIntInf(S: SMALL_INT_INF_STRUCTS): SMALL_INT_INF = 
struct

open S

type t = Word.t
   
fun hash x = x
   
val equals = op =

fun toCstring w = "0x" ^ Word.toString w

val layout = Layout.str o toCstring

fun toMLstring w = Int.toString(Word.toIntX(Word.~>>(w, 0w1)))

fun fromString (str: string): t option =
   if IntInf.<= (minSmall, v) andalso IntInf.<= (v, maxSmall)
      then let val w = Word.fromInt (IntInf.toInt v)
	       val res = Word.orb (0w1, Word.<< (w, 0w1))
	   in SOME res
	   end
   else NONE

(* val fromString =
 *    Trace.trace("SmallIntInf.fromString",
 * 	       String.layout,
 * 	       Option.layout layout) fromString
 *)

(*
 * If you want to compile MLton using an ML implementation which does not
 * have IntInf, then use the following instead.  Note, in this case Overflow
 * MUST be raised.
 *
 * fun fromString (str: string): t option =
 *        let val size = String.size str
 *            fun reader offset =
 *                   if offset = size
 *                      then NONE
 *                      else SOME (String.sub (str, offset), offset + 1)
 *            val start = if String.sub (str, 0) = #"~"
 *                           then 1
 *                           else 0
 *            val base = if String.sub (str, start) = #"0"
 *                           then case reader (start + 1) of
 *                                SOME (#"x", next) => StringCvt.HEX
 *                                | _ => StringCvt.DEC
 *                           else StringCvt.DEC
 *        in (case Pervasive.Int.scan base reader 0 of
 *            SOME (resv, _) =>
 *               let val resw = Word.fromInt resv
 *                   val res = Word.orb (0w1, Word.<< (resw, 0w1))
 *               in if Word.toLargeIntX (Word.xorb (resw, res)) < 0
 *                  then NONE
 *                  else SOME res
 *               end
 *              | _ => Error.bug "SmallIntInf.fromString")
 *           handle Overflow => NONE
 *        end
 *)

fun toWord x = x
   
end
