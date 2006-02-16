val b = ref false
val r = ref NONE
functor F (type t
           val x: t
           val f: t -> string) =
   struct
      val _ =
         if !b
            then print (concat [f (valOf (! r)), "\n"])
         else (b := true; r := SOME x)
   end
structure S = F (type t = int
                 val x = 13
                 val f = Int.toString)
structure S = F (type t = real
                 val x = 13.0
                 val f = Real.toString)
