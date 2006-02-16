(*klife_eq.sml*)

(*based on kitlifeopt.sml, but with copying
  to avoid many generations in the same region*)
val _ =
let

(*
structure Main : BMARK = 
  struct
*)

    fun map f [] = []
      | map f (a::x) = f a :: map f x

    fun map_rec(f, []) = []
      | map_rec(f, x::xs) = f x:: map_rec(f, xs)

    exception ex_undefined of string
    fun error str = raise ex_undefined str

    fun accumulate f a [] = a   (* this now has no escaping regions, although still an escaping arrow effect*)
      | accumulate f a (b::x) = accumulate f (f a b) x

    fun filter p l= 
      rev (accumulate (fn x => fn a => if p a then a::x else x) [] l)
             (*builds an intermediate list; the regions of this list 
               are now made local (unlike in escape.sml) *)

    fun member x [] = false
      | member x (y::ys) = x =y orelse member x ys

    fun C f x y = f y x

    fun cons a x = a::x

    fun revonto x y = accumulate (C cons) x y  (* eta expanded*)

    fun length x = (let fun count n a = n+1 in accumulate count 0 end) x
                    (* eta expanded*)

    fun repeat f = let (* rptf moved into inner let *)
                       fun check n = if n<0 then error "repeat<0" else n
                    in fn x => fn y => let fun rptf n x = if n=0 then x else rptf(n-1)(f x)
                                       in rptf (check x) y 
                                       end
                   end

    fun copy n x = repeat (cons x) n []

    fun spaces n = implode (copy n #" ")
(*mads
    local 
mads*)
      fun copy_list[] = []
        | copy_list((x,y)::rest) = (x,y):: copy_list rest

      fun lexordset [] = []
        | lexordset (a::x) = lexordset (filter (lexless a) x) @ [a] @
                             lexordset (filter (lexgreater a) x)
      and lexless(a1:int,b1:int)(a2,b2) = 
           if a2<a1 then true else if a2=a1 then b2<b1 else false
      and lexgreater pr1 pr2 = lexless pr2 pr1

      fun collect f list =
             let fun accumf sofar [] = sofar
                   | accumf sofar (a::x) = accumf (revonto sofar (f a)) x
              in accumf [] list        (* note: this worked without changes!*)
             end

      fun occurs3 x = 
          (* finds coords which occur exactly 3 times in coordlist x *)
          let
            fun diff x y = filter (fn x => not(member x y)) x  (* unfolded o *)
            fun f xover x3 x2 x1 [] = diff x3 xover
                | f xover x3 x2 x1 (a::x) = 
                   if member a xover then f xover x3 x2 x1 x else
                   if member a x3 then f (a::xover) x3 x2 x1 x else
                   if member a x2 then f xover (a::x3) x2 x1 x else
                   if member a x1 then f xover x3 (a::x2) x1 x else
                                       f xover x3 x2 (a::x1) x
           in f [] [] [] [] x end
(*     in 
*)


      fun copy_string s= implode(explode s)
      fun copy_bool true = true
        | copy_bool false = false


      abstype generation = GEN of (int*int) list
        with 
          fun copy (GEN l) = GEN( copy_list l)
          fun alive (GEN livecoords) = livecoords
          and mkgen coordlist = GEN (lexordset coordlist)
          and mk_nextgen_fn neighbours gen =
              if true then 
              let val living = alive gen
                  fun isalive x = copy_bool(member x living) (* eta *)
                  fun liveneighbours x = length( filter isalive ( neighbours x)) (*eta*)
                  fun twoorthree n = n = 2 orelse n = 3
                  val survivors = copy_list(filter (twoorthree o liveneighbours) living)
                  val newnbrlist = copy_list(collect (fn z => filter (fn x => not( isalive x)) ( neighbours z)) living) (* unfolded o twice*)
                  val newborn = copy_list(occurs3 newnbrlist)
               in mkgen (survivors @ newborn) end
              else gen
     end
(*    end*)

    fun neighbours (i,j) = [(i-1,j-1),(i-1,j),(i-1,j+1),
                            (i,j-1),(i,j+1),
                            (i+1,j-1),(i+1,j),(i+1,j+1)]

    local val xstart = 0 and ystart = 0
          fun markafter n string = string ^ spaces n ^ "0"
          fun plotfrom (x,y) (* current position *)
                       str   (* current line being prepared -- a string *)
                       ((x1,y1)::more)  (* coordinates to be plotted *)
              = if x = x1
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
       let fun f i = if i = n then (n+n-1,n+n)::(n+n,n+n)::nil
                     else (i+i,i+i+1)::(i+i+2,i+i+1)::f(i+1)
        in (0,0)::(1,0):: f 0
       end

    val genB = mkgen(glider at (2,2) @ bail at (2,12)
                     @ rotate (barberpole 4) at (5,20))

    fun copy_whole_arg (p, g) = (p, copy g)

    fun nthgen'(p as(0,g)) = p 
      | nthgen'(p as(i,g)) = (print ".\n";
                              nthgen' (copy_whole_arg(copy_whole_arg(i-1,mk_nextgen_fn neighbours g))))

    fun gun() = mkgen         (* turned into function *)
     [(2,20),(3,19),(3,21),(4,18),(4,22),(4,23),(4,32),(5,7),(5,8),(5,18),
      (5,22),(5,23),(5,29),(5,30),(5,31),(5,32),(5,36),(6,7),(6,8),(6,18),
      (6,22),(6,23),(6,28),(6,29),(6,30),(6,31),(6,36),(7,19),(7,21),(7,28),
      (7,31),(7,40),(7,41),(8,20),(8,28),(8,29),(8,30),(8,31),(8,40),(8,41),
      (9,29),(9,30),(9,31),(9,32)]


    fun iter n = #2(nthgen'(n,gun()))

    fun pr x = print x

    fun show(x) = (pr "starting printing\n";
                       app (fn s => (pr s; pr "\n"))(plot(alive x));
                       ()
                      )  (* had to uncurry show, as iter 50 gave attop
                            also made it return a different unit *)

(*    fun doit () = show((fn _ => ()), (iter 50))           (* inserted call of iter *)*)

      
    fun testit _ = show(iter 50)    (* inserted call of iter *)

(*
  end (* Life *)
*)


(* val _ = (doit (); doit(); doit()); *)

in
  testit (); testit (); testit ()
end
