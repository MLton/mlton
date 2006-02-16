signature SIG =
   sig
      type t
   end

structure S:
   sig
      structure S1: SIG
   end where type S1.t = int =
   struct
      structure S1: SIG =
         struct
            type t = real
         end
   end
