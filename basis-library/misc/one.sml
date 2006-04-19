structure One:
   sig
      type 'a t

      val make: (unit -> 'a) -> 'a t
      val use: 'a t * ('a -> 'b) -> 'b
   end =
   struct
      datatype 'a t = T of {more: unit -> 'a,
                            static: 'a,
                            staticIsInUse: bool ref}

      fun make f = T {more = f,
                      static = f (),
                      staticIsInUse = ref false}

      fun use (T {more, static, staticIsInUse}, f) =
         let
            val () = Primitive.Thread.atomicBegin ()
            val b = ! staticIsInUse
            val d =
               if b then
                  (Primitive.Thread.atomicEnd ();
                   more ())
               else
                  (staticIsInUse := true;
                   Primitive.Thread.atomicEnd ();
                   static)
        in
           DynamicWind.wind (fn () => f d,
                             fn () => if b then () else staticIsInUse := false)
        end

   end
  
