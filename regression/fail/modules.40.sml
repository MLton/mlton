signature SIG =
   sig
      type t
      structure S:
         sig
            type u = t
            type v
            sharing type u = v
         end
   end
