signature SIG =
   sig
      structure S: sig type t end where type t = int
   end where type S.t = bool
