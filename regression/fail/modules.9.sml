signature S =
   sig
      type t = int
      type u = int
      sharing type t = u
   end
