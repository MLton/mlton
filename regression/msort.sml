       fun cp [] =[]
         | cp (x::xs)= x :: cp xs

       (* exormorphic merge *)
       fun merge(xs, []):int list = cp xs
         | merge([], ys) = cp ys
         | merge(l1 as x::xs, l2 as y::ys) = 
               if x<y then x :: merge(xs, l2) 
               else y :: merge(l1, ys)

       (* splitting a list *)
       fun split(x::y::zs, l, r) = split(zs, x::l, y::r)
         | split([x], l, r) = (x::l, r)
         | split([], l, r) = (l, r)

       (* exomorphic merge sort *)
       fun msort []  = []
         | msort [x] = [x]
         | msort xs = let val (l, r) = split(xs, [], [])
                      in merge(msort l, msort r)
                      end;
