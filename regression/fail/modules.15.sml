(* u admits equality, but t does not, hence cannot substitute t for u. *)
signature S =
   sig
      type t
      structure Z:
         sig
            datatype u = U
         end where type u = t
   end
