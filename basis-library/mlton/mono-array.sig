signature MLTON_MONO_ARRAY = sig
   type t
   type elem
   val fromPoly: elem array -> t
   val toPoly: t -> elem array
end
