signature MLTON_MONO_VECTOR = sig
   type t
   type elem
   val fromPoly: elem vector -> t
   val toPoly: t -> elem vector
end
