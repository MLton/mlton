structure MLtonFFI: MLTON_FFI =
struct

structure Prim = Primitive.FFI

open Prim

val atomicBegin = MLtonThread.atomicBegin
val atomicEnd = MLtonThread.atomicEnd
val register = MLtonThread.register
   
end
