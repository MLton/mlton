(* This should fail because v is an eqtype and s does not admit equality.
 * Hence, the side condition on rule 64 fails.
 *)
signature T =   
   sig
      type s
      structure V:
         sig
            datatype v = V
         end where type v = s
   end 
