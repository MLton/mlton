signature S =
   sig
      structure T: sig type t = int end
      sharing T = T
   end
