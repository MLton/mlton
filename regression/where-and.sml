(* where-and.sml *)

(* Checks parsing of where type ... and derived form. *)

signature S = sig type t and u end

signature T = S where type t = int and type u = bool
and       U = S where type t = int and type u = bool

signature V =
sig
    structure A : S where type t = int and type u = bool
    and       B : S where type t = int and type u = bool
end

structure A : S where type t = int and type u = bool =
struct
    type t = int
    type u = bool
end

structure B = A : S where type t = int and type u = bool
and       C = A : S where type t = int and type u = bool

functor F(X : S where type t = int and type u = bool) :
    S where type t = int and type u = bool =
X : S where type t = int and type u = bool
and     G(Y : S where type t = int and type u = bool) :
    S where type t = int and type u = bool =
Y : S where type t = int and type u = bool

signature W =
sig
    type t and u and v
end

signature W' = W where type t = int and type u = int where type v = int
and W'' = W;
