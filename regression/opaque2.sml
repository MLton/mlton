structure S :> sig type ('a,'b) t 
                   val f : ('a,'b) t -> ('b,'a) t
                   val mk : 'a * 'b -> ('a,'b) t 
               end
             =
           struct
             type ('a,'b) t = 'b * 'a
             fun f (x,y) = (y,x)
             fun mk (a,b) = (b,a)
           end

val a = S.mk (5, "hello")

val b = S.f a