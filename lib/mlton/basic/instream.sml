structure Instream: INSTREAM =
struct

open Instream0

structure String = ZString

val input =
   Trace.trace ("In.input", layout, String.layout) input
	       
fun outputAll (ins: t, out: Out.t): unit =
   case input ins of
      "" => ()
    | s => Out.output (out, s)

val inputLine =
   Trace.trace ("In.inputLine", layout, String.layout) inputLine

fun 'a withClose (ins: t, f: t -> 'a): 'a =
   DynamicWind.wind (fn () => f ins, fn () => close ins)

fun 'a withIn (f: string, g: t -> 'a): 'a =
   withClose (openIn f handle IO.Io _ =>
	      Error.bug (concat ["cannot open ", f]), g)

fun withNull f = withIn ("/dev/zero", f)

fun lines ins = rev (foldLines (ins, [], op ::))

end

structure In = Instream
