(* typespec.sml *)

(* Checks scoping of definitional type specifications. *)

type t = int

signature S =
sig
    type t = bool
    and  u = t
end

structure X : S =
struct
    type t = bool
    and  u = bool
end;
