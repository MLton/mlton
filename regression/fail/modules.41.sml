signature SIG =
   sig
      type t
      structure S: sig type u = t end where type u = t * t
   end
