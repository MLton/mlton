(*life.sml*)

(*based on kitlifeopt.sml, but with copying
  to avoid many generations in the same region*)

local
  fun map f l = 
    let fun loop [] = []
          | loop (x::xs) = f x :: loop xs
    in loop l
    end
  
  fun rev l =
    let fun rev_rec(p as ([], acc)) = p
          | rev_rec(x::xs, acc) = rev_rec(xs, x::acc)
    in #2 (rev_rec(l,nil))
    end
  fun length [] = 0
    | length (x::xs) = 1 + length xs

  fun app f [] = ()
    | app f (x::xs) = (f x; app f xs)
  
  
  fun eq_integer_curry(x)(y:int) = x= y
  fun eq_int_pair_curry (x:int,x':int)(y,y'): bool =
    x=y andalso x'=y'
  
  exception ex_undefined of string
  fun error str = raise ex_undefined str
  
  fun accumulate f a [] = a   (* this now has no escaping regions, although still an escaping arrow effect*)
    | accumulate f a (b::x) = accumulate f (f a b) x
  
  fun accumulate' (f, a, []) = a
    | accumulate' (f, a, b::x) = accumulate'(f, f(a,b), x)
  
  fun filter pred l = 
    let fun loop [] = []
          | loop (x::xs) = 
             if pred(x) then x:: loop xs else loop xs
    in
        loop l
    end
  
  
  fun exists pred l = 
    let fun loop [] = false
          | loop (x::xs) = 
              pred(x) orelse loop xs
    in
        loop l
    end
    
  fun member eq x a = exists (eq a) x
  
  fun cons a x = a::x

  fun revonto x y = accumulate' ((fn (x,y) => y::x), x, y)

  local
    fun check n = if n<0 then error "repeat<0" else n
  in
    fun repeat f x y = 
      let fun loop(p as (0,x)) = p
            | loop(n,x) = loop(n-1, f x)
      in
          #2(loop(check x, y))
      end
  end
  
  fun copy n x = repeat (cons x) n []
  
  fun spaces n = implode (copy n #" ")


  fun cp_list[] = []
    | cp_list((x,y)::rest) = 
          let val l = cp_list rest
          in (x,y):: l
          end
  
  fun lexless(a2,b2)(a1:int,b1:int) = 
    if a2<a1 then true else if a2=a1 then b2<b1 else false
  
  
  local
      fun copy [] = []
        | copy (x::xs) = x :: copy xs
      fun take(i,l) = 
          case l of [] => []
           |  x::xs=> if i>0 then x::take(i-1,xs) else nil
      fun drop(i,l) = case l of [] => []
        | x::xs => if i>0 then drop(i-1,xs) else l
      fun merge(lp as (left, right)) =
        case left of [] => right
        | x::xs => (case right of
                    [] => left
                  | y::ys => if lexless x y then x::merge(xs, right)
                             else if lexless y x then y:: merge(left,ys)
                                  else (*x=y*) merge(xs, right)
                 )
  in
      fun tmergesort l =
        case l of [] => []
        | x::xs => (case xs of []=> l 
                    | _ => let val k = length l div 2
                           in merge(copy (tmergesort(take(k,l))),
                                    copy (tmergesort(drop(k,l))))
                           end
                   )
      fun lexordset x = tmergesort x
  end
  
  
  fun collect f list =
               let fun accumf sofar [] = sofar
                     | accumf sofar (a::x) = accumf (revonto sofar (f a)) x
                in accumf [] list        (* note: this worked without changes!*)
               end
  
  fun occurs3 x = 
            (* finds coords which occur exactly 3 times in coordlist x *)
            let fun f (q) =
                  case q of (_,_,_,_,[]) => q
                  | ( xover, x3, x2, x1, (a::x)) =>
                     if member eq_int_pair_curry xover a then f( xover, x3, x2, x1, x) else
                     if member eq_int_pair_curry x3 a then f ((a::xover), x3, x2, x1, x) else
                   if member eq_int_pair_curry x2 a then f (xover, (a::x3), x2, x1, x) else
                     if member eq_int_pair_curry x1 a then f (xover, x3, (a::x2), x1, x) else
                                       f (xover, x3, x2, (a::x1), x)
                fun diff x y = filter (fn x => not(member eq_int_pair_curry y x)) x  (* unfolded o *)
                val (xover, x3, _, _, _) = f ([],[],[],[],x)
             in diff x3 xover end
  
  
  fun neighbours (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),
                            (i,j-1),(i,j+1),
                            (i+1,j-1),(i+1,j),(i+1,j+1)]
  
  infix footnote
  fun x footnote y = x

  abstype generation = GEN of (int*int) list
  with 
    fun copy (GEN l) = GEN(cp_list l)
    fun alive (GEN livecoords) = livecoords
    and mkgen coordlist = GEN (lexordset coordlist)
    and nextgen gen =
      let
        val living = alive gen
        fun isalive x = member eq_int_pair_curry living x
        fun liveneighbours x = length( filter isalive ( neighbours x))
        fun twoorthree n = n=2 orelse n=3
        val survivors = filter (twoorthree o liveneighbours) living
        val newnbrlist = collect (fn z => filter (fn x => not(isalive x)) 
                                                 (neighbours z)
                                 ) living
        val newborn = occurs3 newnbrlist
      in mkgen (cp_list(survivors @ newborn))
      end
  end
  
  local 
    val xstart = 0 and ystart = 0
    fun markafter n string = string ^ spaces n ^ "0"
    fun plotfrom (x,y) (* current position *)
                 str   (* current line being prepared -- a string *)
                 ((x1:int,y1)::more)  (* coordinates to be plotted *)
        = if x=x1
          then (* same line so extend str and continue from y1+1 *)
               plotfrom(x,y1+1)(markafter(y1-y)str)more
          else (* flush current line and start a new line *)
               str :: plotfrom(x+1,ystart)""((x1,y1)::more)
      | plotfrom (x,y) str [] = [str]
    fun good (x,y) = x>=xstart andalso y>=ystart
  in  
    fun plot coordlist = plotfrom(xstart,ystart) "" 
                                 (filter good coordlist)
  end
  
  (* the initial generation *)

  fun gun() = mkgen         
       [(2,20),(3,19),(3,21),(4,18),(4,22),(4,23),(4,32),(5,7),(5,8),(5,18),
        (5,22),(5,23),(5,29),(5,30),(5,31),(5,32),(5,36),(6,7),(6,8),(6,18),
        (6,22),(6,23),(6,28),(6,29),(6,30),(6,31),(6,36),(7,19),(7,21),(7,28),
        (7,31),(7,40),(7,41),(8,20),(8,28),(8,29),(8,30),(8,31),(8,40),(8,41),
        (9,29),(9,30),(9,31),(9,32)]
  
  

  fun show(x) = app (fn s => (print s; print "\n"))(plot(alive x));
  
  local 
    fun nthgen'(p as(0,g)) = p 
      | nthgen'(p as(i,g)) = 
          nthgen' (i-1, let val g' = nextgen  g
                        in  show g;
                            (*resetRegions g;*)  (* resetRegions g can actually be omitted here, since *)
                            copy g'          (* copy will reset the regions of g!                  *)
                        end)
  
  in 
    fun iter n = #2(nthgen'(n,gun()))
  end

  fun testit _ = show(iter 200)    

in
  val _ = testit ()
end
 
