functor TyconKind (S: TYCON_KIND_STRUCTS): TYCON_KIND = 
struct

open S

datatype t =
   Arity of int
 | Nary

val layout =
   fn Arity n => Int.layout n
    | Nary => Layout.str "n-ary"

val equals =
   fn (Arity n, Arity n') => n = n'
    | (Nary, Nary) => true
    | _ => false
	 
end

