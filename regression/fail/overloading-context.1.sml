(* This must fail, because the overloading context can be no larger than the
 * smallest enclosing strdec.  So, the declaration of double must be resolved
 * (with type int -> int) before continuing.
 *)
structure S =
   struct
      fun double x = x + x
   end
val _ = S.double 2.0
