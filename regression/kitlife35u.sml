(*kitlife35u.sml*)

(*based on kitlifeopt.sml, but with copying
  to avoid many generations in the same region*)

local

fun eq_integer (x: int, y: int): bool = x = y
fun eq_string  (x: string, y: string): bool = x = y

fun eq_integer_curry(x)(y:int) =eq_integer(x,y)
fun eq_int_pair_curry (x,x')(y,y'): bool =
  eq_integer(x,y) andalso eq_integer(x',y')

fun app f [] = ()
  | app f (x::xs) = (f x; app f xs)


    fun map f [] = []
      | map f (a::x) = f a :: map f x

    fun map_rec(f, []) = []
      | map_rec(f, x::xs) = f x:: map_rec(f, xs)

    exception ex_undefined of string
    fun error str = raise ex_undefined str

    fun accumulate f a [] = a   (* this now has no escaping regions, although still an escaping arrow effect*)
      | accumulate f a (b::x) = accumulate f (f a b) x

    fun accumulate' (f, a, []) = a
      | accumulate' (f, a, b::x) = accumulate'(f, f(a,b), x)

    fun filter p l= 
      rev (accumulate (fn x => fn a => if p a then a::x else x) [] l)
             (*builds an intermediate list; the regions of this list 
               are now made local (unlike in escape.sml) *)


    fun equal a b = a=b 

    fun exists p [] = false
      | exists p (a::x) =  if p a then true else exists p x

    fun exists' (p, []) = false
      | exists' (p, (a::x)) =  p a orelse exists'(p,x)


    fun member eq x a = exists' (eq a, x)

    fun C f x y = f y x

    fun cons a x = a::x


    fun revonto x y = accumulate' ((fn (x,y) => y::x), x, y)

    fun copy_int n = n+0

    fun  length x = copy_int(let fun count (n, a) = n+1 in accumulate'(count, 0, x) end)
                    (* eta expanded*)

    fun repeat f = let (* rptf moved into inner let *)
                       fun check n = if n<0 then error "repeat<0" else n
                    in fn x => fn y => let fun rptf n x = if n=0 then x else rptf(n-1)(f x)
                                       in rptf (check x) y 
                                       end
                   end

    fun copy n x = repeat (cons x) n []

    fun spaces n = implode (copy n #" ")

      fun copy_list[] = []
        | copy_list((x,y)::rest) = (x+0,y+0):: copy_list rest


      fun lexless(a2,b2)(a1:int,b1:int) = 
           if a2<a1 then true else if a2=a1 then b2<b1 else false



fun length l = case l of
  [] => 0 
| x::xs => 1 + length xs
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

fun tmergesort l =
  case l of [] => []
  | x::xs => (case xs of []=> l 
              | _ => let val k = length l div 2
                   in merge(copy (tmergesort(take(k,l))),
                            copy (tmergesort(drop(k,l))))
                   end
             )
fun lexordset x = tmergesort x




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

      fun copy_string s= implode(explode s)
      fun copy_bool true = true
        | copy_bool false = false

      fun neighbours (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),
                            (i,j-1),(i,j+1),
                            (i+1,j-1),(i+1,j),(i+1,j+1)]


      abstype generation = GEN of (int*int) list
        with 
          fun copy (GEN l) = GEN( copy_list l)
          fun alive (GEN livecoords) = livecoords
          and mkgen coordlist = GEN (lexordset coordlist)
          and mk_nextgen_fn gen =
              if true then 
              let val living = alive gen
                  fun isalive x = copy_bool(member eq_int_pair_curry living x) (* eta *)
                  fun liveneighbours x = length( filter isalive ( neighbours x)) (*eta*)
                  fun twoorthree n = eq_integer(n,2) orelse eq_integer(n,3)
                  val survivors = copy_list(filter (twoorthree o liveneighbours) living)
                  val newnbrlist = copy_list(collect (fn z => filter (fn x => not( isalive x)) ( neighbours z)) living) (* unfolded o twice*)
                  val newborn = copy_list(occurs3 newnbrlist)
               in mkgen (survivors @ newborn) end
              else gen
     end


    local val xstart = 0 and ystart = 0
          fun markafter n string = string ^ spaces n ^ "0"
          fun plotfrom (x,y) (* current position *)
                       str   (* current line being prepared -- a string *)
                       ((x1,y1)::more)  (* coordinates to be plotted *)
              = if eq_integer(x,x1)
                 then (* same line so extend str and continue from y1+1 *)
                      plotfrom(x,y1+1)(markafter(y1-y)str)more
                 else (* flush current line and start a new line *)
                      str :: plotfrom(x+1,ystart)""((x1,y1)::more)
            | plotfrom (x,y) str [] = [str]
           fun good (x,y) = x>=xstart andalso y>=ystart
     in  fun plot coordlist = map_rec(copy_string,(plotfrom(xstart,ystart) "" 
                                 (copy_list(filter good coordlist))))
    end


    infix 6 at
    fun coordlist at (x:int,y:int) = let fun move(a,b) = (a+x,b+y) 
                                      in map move coordlist end
    fun rotate x = map (fn (x:int,y:int) => (y,~x)) x  (* eta converted*)

    val glider = [(0,0),(0,2),(1,1),(1,2),(2,1)]
    val bail = [(0,0),(0,1),(1,0),(1,1)]
    fun barberpole n =
       let fun f i = if eq_integer(i,n) then (n+n-1,n+n)::(n+n,n+n)::nil
                       else (i+i,i+i+1)::(i+i+2,i+i+1)::f(i+1)
        in (0,0)::(1,0):: f 0
       end

    val genB = mkgen(glider at (2,2) @ bail at (2,12)
                     @ rotate (barberpole 4) at (5,20))

    fun copy_whole_arg (p, g) = (copy_int p, copy g)

    fun nthgen'(p as(0,g)) = p 
      | nthgen'(p as(i,g)) = (print ".\n";
                              nthgen' (copy_whole_arg(let val arg = (i-1,mk_nextgen_fn  g)
                                                          val arg' = copy_whole_arg arg
                                                      in  (*resetRegions arg; *)
                                                          arg'
                                                      end)))

    fun gun() = mkgen         (* turned into function *)
     [(2,20),(3,19),(3,21),(4,18),(4,22),(4,23),(4,32),(5,7),(5,8),(5,18),
      (5,22),(5,23),(5,29),(5,30),(5,31),(5,32),(5,36),(6,7),(6,8),(6,18),
      (6,22),(6,23),(6,28),(6,29),(6,30),(6,31),(6,36),(7,19),(7,21),(7,28),
      (7,31),(7,40),(7,41),(8,20),(8,28),(8,29),(8,30),(8,31),(8,40),(8,41),
      (9,29),(9,30),(9,31),(9,32)]


    fun iter n = #2(nthgen'(n+0,gun()))

    fun pr x = print x

    fun show(x) = (pr "starting printing\n";
                       app (fn s => (pr s; pr "\n"))(plot(alive x));
                       ()
                      )  (* had to uncurry show, as iter 50 gave attop
                            also made it return a different unit *)
      
    fun testit _ = show(iter 250)    (* inserted call of iter *)
    
    val _ = testit ()
in
    val done = "done";
end
 
