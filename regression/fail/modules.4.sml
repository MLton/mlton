signature S =
   sig
      type t
      type u
      type v = t * t
      sharing type u = v
   end
