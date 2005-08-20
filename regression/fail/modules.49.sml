signature SIG =
   sig
      type t
   end

functor F (structure S1: SIG
           structure S2: SIG
           sharing S1 = S2) =
   struct
   end

structure S1: SIG = struct type t = int end
structure S2: SIG = struct type t = real end
structure Z = F (structure S1 = S1
                 structure S2 = S2)
