structure S:>
   sig
      type t
      val x: t
   end =
   struct
      type t = unit
      val x = ()
   end
val _ = S.x = S.x
