structure MLtonFFI: MLTON_FFI =
struct

structure Prim = Primitive.FFI

open Prim

val atomicBegin = MLtonThread.atomicBegin
val atomicEnd = MLtonThread.atomicEnd
   
val register =
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
