signature S =
   sig
      type ('a, 'b) t
      type ('a, 'b) u
      type ('a, 'b) v = ('b, 'a) t
      sharing type u = v
   end
