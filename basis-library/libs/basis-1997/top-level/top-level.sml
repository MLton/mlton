
(* Non-standard signatures *)
signature MLTON = MLTON
signature SML_OF_NJ = SML_OF_NJ
signature UNSAFE = UNSAFE

(* Non-standard structures *)
structure Primitive = Primitive
local
   open Basis2002
in
   structure MLton = MLton
   structure SMLofNJ = SMLofNJ
   structure Unsafe = Unsafe
end

open Basis1997

val op = = op =

_basis_done MLtonFFI
