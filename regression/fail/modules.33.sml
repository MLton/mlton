signature S =
   sig
      type t = int
   end
signature S =
   sig
      structure S1: S
      structure S2: S
      sharing type S1.t = S2.t
   end
