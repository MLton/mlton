signature S = sig type t end
signature S1 = S where type t = int
signature S2 = S where type t = real

structure S1: S1 =
   struct
      type t = int
   end

structure S2: S2 = S1

