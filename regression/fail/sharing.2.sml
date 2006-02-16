functor F (structure A: sig type t end
           structure B: sig end
           structure C: sig type t end
           sharing A = B
           sharing B = C) =
   struct
      val _: A.t -> C.t = fn x => x
   end
