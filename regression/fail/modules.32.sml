structure S:>
   sig
      type t
      val x: t
   end =
   struct
      type t = real
      val x = 13.0
   end
val _ = S.x = S.x
