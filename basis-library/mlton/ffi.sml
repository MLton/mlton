structure MLtonFFI: MLTON_FFI =
struct

structure Prim = Primitive.FFI

open Prim

val atomicBegin = MLtonThread.atomicBegin
val atomicEnd = MLtonThread.atomicEnd

val msg = Primitive.Stdio.print
   
val register: int * (unit -> unit) -> unit =
   let
      val exports = Array.array (Prim.numExports, fn () =>
				 raise Fail "undefined export\n")
      val _ =
	 MLtonThread.setCallFromCHandler
	 (fn () => Array.sub (exports, Prim.getOp ()) ())
   in
      fn (i, f) => Array.update (exports, i, f)
   end
   
end
