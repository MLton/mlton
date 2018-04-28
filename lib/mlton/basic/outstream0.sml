(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Outstream0 =
struct

structure TextIO = Pervasive.TextIO
open TextIO

(*val output = fn (out, s) => (output (out, s); flushOut out) *)

type t = outstream

val standard = stdOut
val error = stdErr
val close = closeOut
fun outputc stream string = output (stream, string)
val flush = flushOut

fun newline s = output (s, "\n")

fun outputl (s, x) = (output (s, x); newline s)

fun print s = output (standard, s)

fun outputNothing _ = ()

fun set (o1: t, o2:t): unit =
   TextIO.setOutstream (o1, TextIO.getOutstream o2)

fun fluidLet (s1, s2, thunk) =
   let
      val old = TextIO.getOutstream s1
      val () = set (s1, s2)
   in
      Exn0.finally (thunk, fn () => TextIO.setOutstream (s1, old))
   end

fun withClose (out: t, f: t -> 'a): 'a =
   Exn0.finally (fn () => f out, fn () => close out)

local
   fun 'a withh (f, p: t -> 'a, openn): 'a =
      let
         val out = openn f handle IO.Io _ => Error.bug ("OutStream0.withh: cannot open " ^ f)
      in
         withClose (out, p)
      end
in
   fun 'a withOut (f, p: t -> 'a): 'a = withh (f, p, openOut)
   fun withAppend (f, p) = withh (f, p, openAppend)
end

fun 'a withNull (f: t -> 'a): 'a = withOut ("/dev/null", f)

fun ignore (out: t, f: unit -> 'a): 'a =
   withNull (fn out' => fluidLet (out, out', f))

end
