structure S:
   sig
      datatype 'a t = T of int
   end =
   struct
      datatype 'a t = T of 'a
   end
