structure S:
   sig
      type t
   end where type t = int * int
   =
   struct
      type t = int
   end
