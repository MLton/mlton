signature SIG =
   sig
      type u
      type v = u
   end where type v = int
structure S: SIG =
   struct
      type u = real
      type v = real
   end
