functor TyconKind (S: TYCON_KIND_STRUCTS): TYCON_KIND = 
struct

open S

datatype t =
   Arity of int
 | Nary

val layout =
   fn Arity n => Int.layout n
    | Nary => Layout.str "n-ary"
	 
end

