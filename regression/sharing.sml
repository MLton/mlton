(* sharing.sml *)

(* Checks treatment of sharing constraints. *)

signature S =
sig
    type t
    type s = t
    sharing type t = s
end;

signature T =	(* from SML/NJ doc *)
sig
    type s
    structure A :
    sig
        datatype d = D of s
        datatype t = K
    end
    sharing type s = A.t
end;
