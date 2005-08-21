(*kitsimple.sml*)

exception Overflow

fun digit n = chr(ord #"0" + n)
fun digits(n,acc) =
      if n >=0 andalso n<=9 then digit n:: acc
      else digits (n div 10, digit(n mod 10) :: acc)

fun int_to_string(n) = 
      if n<0 then implode(#"~"::digits(~n,[]))
      else implode(digits(n,[]))


exception Hd
fun hd [] = raise Hd
  | hd (x::xs) = x

(*
structure Array =   (* Interface as in SML/NJ *)
struct
*)

  infix sub

  type 'a array = 'a ref list

  exception Size 

  exception Subscript 

  fun tabulate' (i,f) = 
        let fun tab j = if j < i then f j :: tab (j+1) else nil
        in if i < 0 then raise Size else (tab 0)
        end

  fun array (n, x) = tabulate' (n, fn _ => ref x)

  fun arrayoflist l = map ref l

  fun tabulate (n, f) = tabulate' (n, fn x => ref(f x))

  fun sub'(nil,i) = raise Subscript
    | sub' (a::r,i) = if i > 0 then sub' (r,i-1)
                      else if i < 0 then raise Subscript
                           else a 

  fun op sub (a, i) =  !(sub'(a,i))

  fun update (a, i, v) = sub'(a, i) := v

  fun length [] = 0
    | length (x::xs) = 1 + length xs

(*
end (* Array *)
*)

(*
structure List =  (* Interface as in SML/NJ *)
struct
 *)

exception Nth and NthTail

fun null [] = true
  | null _ = false

(* this lenght overrides the one in Array, but that was also the
   case in the original lexgen.sml (see the order in the open declaration below)
*)

fun length [] = 0
  | length (x::xs) = 1 + length xs

fun rev l = (* linear-time reversal of lists! *)
  let fun loop([], acc) = acc
        | loop(x::xs, acc) = loop(xs, x::acc)
  in
     loop(l, [])
  end

fun fold f [] b = b
  | fold f (x::xs) b = f(x,fold f xs b)

fun revfold f [] b = b
  | revfold f (x::xs) b = revfold f xs (f(x,b))

fun app f [] = ()
  | app f (x::xs) = (f x; app f xs)

fun revapp f [] = ()
  | revapp f (x::xs) = (revapp f xs; f x; ())

fun nth ([],_) = raise Nth
  | nth (x::xs,0) = x
  | nth (x::xs,k) = nth(xs,k-1)

fun nthtail ([],_) = raise NthTail
  | nthtail (l,0) = l
  | nthtail (l::ls,n) = nthtail(ls,n-1)

fun exists p [] = false
  | exists p (x::xs) = p x orelse exists p xs

(* 
end; (* List *)
*)


(*
structure Control =
  struct
*)
    val trace = ref true
(*
  end;
*)

(*
structure Array2 : sig
    type 'a array2
    exception Subscript
    val array: (int*int) * '1a -> '1a array2
    val sub : 'a array2 * (int*int) -> 'a
    val update : 'a array2 * (int*int) * 'a -> unit
    val length : 'a array2 -> (int*int)
  end = struct
*)
    type 'a array2 = {size : (int*int), value : 'a array}
    exception Subscript = Subscript
    fun index22 ((i1:int,i2:int),(s1,s2)) =
        if i1>=0 andalso i1<s1 andalso i2>=0 andalso i2<s2 then i1*s2+i2 
        else raise Subscript
    fun array22(bnds as (i1,i2), v) = {size=bnds, value=array(i1*i2, v)}
    fun op sub22 ({size,value}, indx) = (op sub) (value, index22(indx,size))
    fun update22 ({size=size,value=A},i,v) = update(A,index22(i,size),v)
    fun length22{size=size,value=A} = size
(*
  end; (* Array2 *)
*)

(* Simple
 * error: grid_max < 5
 *)
(*
functor Simple(val grid_max: int val step_count: int) (* : BMARK *) =
  struct
*)

      (* lars: unfolding the simple functor application *)
    val grid_max = 30
    val step_count = 1

(*    open Array List
*)
    infix 9 sub


    
    fun min (x:real,y:real) = if x<y then x else y
    fun max (x:real,y:real) = if x<y then y else x
    exception MaxList
    exception MinList
    exception SumList
    fun max_list [] = raise MaxList | max_list l = fold max l (hd l)
    fun min_list [] = raise MinList | min_list l = fold min l (hd l)
    fun sum_list [] = raise SumList
      | sum_list (l:real list) = fold (op +) l 0.0

fun for {from=start:int,step=delta:int, to=endd:int} body =
    if delta>0 andalso endd>=start then 
        let fun f x = if x > endd then () else (body x; f(x+delta))
        in f start
        end
    else if endd<=start then
        let fun f x = if x < endd then () else (body x; f(x+delta))
        in f start
        end
    else ()
fun from(n,m) = if n>m then [] else n::from(n+1,m)
fun flatten [] = []
  | flatten (x::xs) = x @ flatten xs
fun pow(x:real,y:int) = if y=0 then 1.0 else x * pow(x,y-1)
fun array2(bounds as ((l1,u1),(l2,u2)),v) =  
    (array22((u1-l1+1, u2-l2+1),v), bounds)
fun sub2((A,((lb1:int,ub1:int),(lb2:int,ub2:int))),(k,l)) = 
    sub22(A, (k-lb1, l-lb2)) 
fun update2((A,((lb1,_),(lb2,_))),(k,l), v) = update22(A,(k-lb1,l-lb2),v)
fun bounds2(_,b) = b
fun printarray2 (A as (M:real array2,((l1,u1),(l2,u2)))) =
    for {from=l1,step=1,to=u1} (fn i =>
        (print "[";
         for {from=l2,step=1,to=u2-1} (fn j => 
              print ( (* makestring(sub2(A,(i,j))) ^ *) ", "));
         print ( (* makestring (sub2(A,(i,u2))) ^ *) "]\n")))
fun array1((l,u),v) = (array(u-l+1,v),(l,u))
fun sub1((A,(l:int,u:int)),i:int) = (op sub)(A,i-l) 
fun update1((A,(l,_)),i,v) = update(A,i-l,v)
fun bounds1(_,b) = b

(*
 * Specification of the state variable computation
 *)
val grid_size = ((2,grid_max), (2,grid_max))

fun north (k,l) = (k-1,l)       
fun south (k,l) = (k+1,l)               

fun east (k,l) = (k,l+1)
fun west (k,l) = (k,l-1)

val northeast = north o east
val southeast = south o east
val northwest = north o west            
val southwest = south o west

type dir = int * int -> int * int

val farnorth : dir = north o north 
val farsouth : dir = south o south
val fareast : dir = east o east         
val farwest : dir = west o west

fun zone_A(k,l) = (k,l)
fun zone_B(k,l) = (k+1,l)

fun zone_C(k,l) = (k+1,l+1)             
fun zone_D(k,l) = (k,l+1)

val  zone_corner_northeast = north   
val  zone_corner_northwest = northwest
fun  zone_corner_southeast zone = zone
val  zone_corner_southwest = west

val ((kmin,kmax),(lmin,lmax))   = grid_size
val dimension_all_nodes         = ((kmin-1,kmax+1),(lmin-1,lmax+1))
fun for_all_nodes f =
    for {from=kmin-1, step=1, to=kmax+1} (fn k =>
       for {from=lmin-1, step=1, to=lmax+1} (fn l => f k l))

val dimension_interior_nodes    = ((kmin,kmax),(lmin,lmax))
fun for_interior_nodes f =
    for {from=kmin, step=1, to=kmax} (fn k =>
       for {from=lmin, step=1, to=lmax} (fn l => f k l))

val dimension_all_zones         = ((kmin,kmax+1),(lmin,lmax+1))
fun for_all_zones f =
    for {from=kmin, step=1, to=kmax+1} (fn k =>
       for {from=lmin, step=1, to=lmax+1} (fn l => f (k,l)))

val dimension_interior_zones    = ((kmin+1,kmax),(lmin+1,lmax))
fun for_interior_zones f =
    for {from=kmin+1, step=1, to=kmax} (fn k =>
       for {from=lmin+1, step=1, to=lmax} (fn l => f (k,l)))

fun map_interior_nodes f =
    flatten(map (fn k => (map (fn l => f (k,l)) 
                              (from(lmin,lmax))))
                (from(kmin,kmax)))
fun map_interior_zones f =
    flatten(map (fn k => (map (fn l => f (k,l)) 
                               (from(lmin+1,lmax))))
                 (from(kmin+1,kmax)))

fun for_north_ward_interior_zones f =
    for {from=kmax, step= ~1, to=kmin+1} (fn k =>
       for {from=lmin+1, step=1, to=lmax} (fn l => f (k,l)))
fun for_west_ward_interior_zones f = 
    for {from=kmin+1, step=1, to=kmax} (fn k =>
       for {from=lmax, step= ~1, to=lmin+1} (fn l => f (k,l)))


fun for_north_zones f = for {from=lmin, step=1, to=lmax+1} (fn l => f (kmin,l))
fun for_south_zones f = for {from=lmin+1, step=1, to=lmax} (fn l => f (kmax+1,l))
fun for_east_zones f = for {from=kmin+1, step=1, to=kmax+1}(fn k => f (k,lmax+1))
fun for_west_zones f = for {from=kmin+1, step=1, to=kmax+1}(fn k => f (k,lmin))

type 'a reflect_dir = int * int -> {size: int * int, value: 'a ref list}
        * ((int * int) * (int * int)) -> 'a
  
fun reflect dir node A = sub2(A, dir node)
val reflect_north : real reflect_dir = reflect north
val reflect_south : real reflect_dir = reflect south
val reflect_east  : real reflect_dir = reflect east
val reflect_west  : real reflect_dir = reflect west

fun for_north_nodes f =
    for {from=lmin, step=1, to=lmax-1} (fn l => f (kmin-1,l))
fun for_south_nodes f =
    for {from=lmin, step=1, to=lmax-1} (fn l => f (kmax+1,l))
fun for_east_nodes f  = 
    for {from=kmin, step=1, to=kmax-1} (fn k => f (k,lmax+1))
fun for_west_nodes f =
    for {from=kmin, step=1, to=kmax-1} (fn k => f (k,lmin-1))

val north_east_corner = (kmin-1,lmax+1)
val north_west_corner = (kmin-1,lmin-1)
val south_east_corner = (kmax+1,lmax+1)
val south_west_corner = (kmax+1,lmin-1)

val west_of_north_east = (kmin-1, lmax)
val west_of_south_east = (kmax+1, lmax)
val north_of_south_east = (kmax, lmax+1)
val north_of_south_west = (kmax, lmin-1)



(*
 * Initialization of parameters
 *)
val  constant_heat_source = 0.0
val  deltat_maximum = 0.01
val  specific_heat = 0.1
val  p_coeffs = let val M = array2(((0,2),(0,2)), 0.0)
                in update2(M, (1,1), 0.06698); M
                end
val e_coeffs = let val M = array2(((0,2),(0,2)), 0.0)
               in update2(M, (0,1), 0.1); M
               end
val p_poly   = array2(((1,4),(1,5)),p_coeffs)

val e_poly   = array2(((1,4),(1,5)), e_coeffs)

val rho_table = let val V = array1((1,3), 0.0)
                in  update1(V,2,1.0);
                    update1(V,3,100.0);
                    V
                end
val theta_table = let val V = array1((1,4), 0.0)
                  in update1(V,2,3.0);
                      update1(V,3,300.0);
                      update1(V,4,3000.0);
                      V
                  end

val extract_energy_tables_from_constants  = (e_poly,2,rho_table,theta_table)
val extract_pressure_tables_from_constants = (p_poly,2,rho_table,theta_table)

val nbc = let val M = array2(dimension_all_zones, 1)
          in for {from=lmin+1,step=1,to=lmax} (fn j => update2(M,(kmax+1, j),2));
              update2(M,(kmin,lmin),4);
              update2(M,(kmin,lmax+1),4);
              update2(M,(kmax+1,lmin),4);
              update2(M,(kmax+1,lmax+1),4);
              M
          end
val pbb = let val A = array1((1,4), 0.0)
          in update1(A,2,6.0); A
          end
val pb  = let val A = array1((1,4), 1.0)
          in update1(A,2,0.0); update1(A,3,0.0); A
          end
val qb  =     pb

val all_zero_nodes = array2(dimension_all_nodes, 0.0)

val all_zero_zones = array2(dimension_all_zones, 0.0)


(*
 * Positional Coordinates. (page 9-10)
 *)

fun make_position_matrix interior_function =
    let val r' = array2(dimension_all_nodes, 0.0)
        val z' = array2(dimension_all_nodes, 0.0)
        fun boundary_position (rx,zx,ry,zy,ra,za) =
            let val (rax, zax)  =  (ra - rx, za - zx)
                val (ryx, zyx)  =  (ry - rx, zy - zx)
                val omega       =  2.0*(rax*ryx + zax*zyx)/(ryx*ryx + zyx*zyx)
                val rb          =  rx - rax + omega*ryx
                val zb          =  zx - zax + omega*zyx
            in (rb, zb)
            end
        
        fun reflect_node (x_dir, y_dir, a_dir, node) =
            let val rx = reflect x_dir  node  r'
                val zx = reflect x_dir  node  z'
                val ry = reflect y_dir  node  r'
                val zy = reflect y_dir  node  z'
                val ra = reflect a_dir  node  r'
                val za = reflect a_dir  node  z'
            in boundary_position (rx, zx, ry, zy, ra, za)
            end
        fun u2 (rv,zv) n = (update2(r',n,rv); update2(z',n,zv))
    in
        for_interior_nodes (fn k => fn l => u2 (interior_function (k,l)) (k,l));
        for_north_nodes(fn n => u2 (reflect_node(south,southeast,farsouth,n)) n);
        for_south_nodes (fn n => u2(reflect_node(north,northeast,farnorth,n)) n);
        for_east_nodes (fn n => u2(reflect_node(west, southwest, farwest, n)) n);
        for_west_nodes (fn n => u2(reflect_node(east, southeast, fareast, n)) n);
        u2 (reflect_node(south, southwest, farsouth, west_of_north_east))
           west_of_north_east;
        u2 (reflect_node(north, northwest, farnorth, west_of_south_east)) 
           west_of_south_east;
        u2 (reflect_node(west, northwest, farwest, north_of_south_east))
           north_of_south_east;
        u2 (reflect_node(east, northeast, fareast, north_of_south_west))
           north_of_south_west;
        u2 (reflect_node(southwest, west, farwest, north_east_corner))
           north_east_corner;
        u2 (reflect_node(northwest, west, farwest, south_east_corner))
           south_east_corner;
        u2 (reflect_node(southeast, south, farsouth, north_west_corner))
           north_west_corner;
        u2 (reflect_node(northeast, east, fareast, south_west_corner))
           south_west_corner; 
        (r',z')
    end



(*
 * Physical Properties of a Zone (page 10)
 *)
fun zone_area_vol ((r,z), zone) =
    let val (r1,z1)=(sub2(r,zone_corner_southwest zone),
                     sub2(z,zone_corner_southwest zone))
        val (r2,z2)=(sub2(r,zone_corner_southeast zone),
                     sub2(z,zone_corner_southeast zone))
        val (r3,z3)=(sub2(r,zone_corner_northeast zone),
                     sub2(z,zone_corner_northeast zone))
        val (r4,z4)=(sub2(r,zone_corner_northwest zone),
                     sub2(z,zone_corner_northwest zone))
        val area1  =  (r2-r1)*(z3-z1) - (r3-r2)*(z3-z2)
        val radius1  =  0.3333  *(r1+r2+r3)
        val volume1  =  area1 * radius1
        val area2  =  (r3-r1)*(z4-z3) - (r4-r3)*(z3-z1)
        val radius2  =  0.3333 *(r1+r3+r4)
        val volume2  =  area2 * radius2
    in  (area1+area2, volume1+volume2)
    end

(*
 * Velocity (page 8)
 *)
fun make_velocity((u,w),(r,z),p,q,alpha,rho,delta_t: real) =
    let fun line_integral (p,z,node) : real =
            sub2(p,zone_A node)*(sub2(z,west node) - sub2(z,north node)) +
            sub2(p,zone_B node)*(sub2(z,south node) - sub2(z,west node)) +
            sub2(p,zone_C node)*(sub2(z,east node) - sub2(z,south node)) +
            sub2(p,zone_D node)*(sub2(z,north node) - sub2(z,east node))
        fun regional_mass node =
            0.5 * (sub2(rho, zone_A node)*sub2(alpha,zone_A node) +
                   sub2(rho, zone_B node)*sub2(alpha,zone_B node) +
                   sub2(rho, zone_C node)*sub2(alpha,zone_C node) +
                   sub2(rho, zone_D node)*sub2(alpha,zone_D node))
        fun velocity node =
            let val d = regional_mass node
                val n1 = ~(line_integral(p,z,node)) - line_integral(q,z,node)
                val n2 = line_integral(p,r,node) + line_integral(q,r,node)
                val u_dot = n1/d
                val w_dot = n2/d
            in (sub2(u,node)+delta_t*u_dot, sub2(w,node)+delta_t*w_dot)
            end
        val U = array2(dimension_interior_nodes,0.0)
        val W = array2(dimension_interior_nodes,0.0)
    in for_interior_nodes (fn k => fn l => let val (uv,wv) = velocity (k,l)
                                           in update2(U,(k,l),uv);
                                              update2(W,(k,l),wv)
                                           end);
       (U,W)
    end



fun make_position ((r,z),delta_t:real,(u',w')) =
    let fun interior_position node = 
            (sub2(r,node) + delta_t*sub2(u',node), 
             sub2(z,node) + delta_t*sub2(w',node))
    in make_position_matrix interior_position
    end
        

fun make_area_density_volume(rho, s, x') =
    let val alpha' = array2(dimension_all_zones, 0.0)
        val s' = array2(dimension_all_zones, 0.0)
        val rho' = array2(dimension_all_zones, 0.0)
        fun interior_area zone = 
            let val (area, vol) = zone_area_vol (x', zone)
                val density =  sub2(rho,zone)*sub2(s,zone) / vol
            in (area,vol,density)
            end
        fun reflect_area_vol_density reflect_function = 
            (reflect_function alpha',reflect_function s',reflect_function rho')
        fun update_asr (zone,(a,s,r)) = (update2(alpha',zone,a);
                                         update2(s',zone,s);
                                         update2(rho',zone,r))
        fun r_area_vol_den (reflect_dir,zone) =
            let val asr =  reflect_area_vol_density (reflect_dir zone)
            in update_asr(zone, asr)
            end
    in
        for_interior_zones (fn zone => update_asr(zone, interior_area zone));
        for_south_zones (fn zone => r_area_vol_den(reflect_north, zone));
        for_east_zones (fn zone => r_area_vol_den(reflect_west, zone));
        for_west_zones (fn zone => r_area_vol_den(reflect_east, zone));
        for_north_zones (fn zone => r_area_vol_den(reflect_south, zone));
        (alpha', rho', s')
    end 


(*
 * Artifical Viscosity (page 11)
 *)
fun make_viscosity(p,(u',w'),(r',z'), alpha',rho') = 
    let fun interior_viscosity zone =
        let fun upper_del f = 
            0.5 * ((sub2(f,zone_corner_southeast zone) - 
                    sub2(f,zone_corner_northeast zone)) +
                   (sub2(f,zone_corner_southwest zone) - 
                    sub2(f,zone_corner_northwest zone)))
            fun lower_del f = 
                0.5 * ((sub2(f,zone_corner_southeast zone) - 
                        sub2(f,zone_corner_southwest zone)) +
                       (sub2(f,zone_corner_northeast zone) - 
                        sub2(f,zone_corner_northwest zone)))
            val xi      = pow(upper_del   r',2) + pow(upper_del   z',2)
            val eta = pow(lower_del   r',2) + pow(lower_del   z',2)
            val upper_disc =  (upper_del r')*(lower_del w') - 
                              (upper_del z')*(lower_del u')
            val lower_disc = (upper_del u')*(lower_del z') -
                             (upper_del w') * (lower_del r')
            val upper_ubar = if upper_disc<0.0   then upper_disc/xi    else 0.0
            val lower_ubar = if lower_disc<0.0   then lower_disc/eta    else 0.0
            val gamma    = 1.6
            val speed_of_sound  =  gamma*sub2(p,zone)/sub2(rho',zone)
            val ubar  =  pow(upper_ubar,2) + pow(lower_ubar,2)
            val viscosity   =  
                sub2(rho',zone)*(1.5*ubar + 0.5*speed_of_sound*(Math.sqrt ubar))
            val length   = Math.sqrt(pow(upper_del r',2) + pow(lower_del r',2))
            val courant_delta = 0.5* sub2(alpha',zone)/(speed_of_sound*length)
        in (viscosity, courant_delta)
        end
        val q' = array2(dimension_all_zones, 0.0)
        val d  = array2(dimension_all_zones, 0.0)
        fun reflect_viscosity_cdelta (direction, zone) =
            sub2(q',direction zone) * sub1(qb, sub2(nbc,zone))
        fun do_zones (dir,zone) = 
            update2(q',zone,reflect_viscosity_cdelta (dir,zone))
    in
        for_interior_zones (fn zone => let val (qv,dv) = interior_viscosity zone
                                       in update2(q',zone,qv);
                                           update2(d,zone,dv)
                                       end);
        for_south_zones (fn zone => do_zones(north,zone));
        for_east_zones (fn zone => do_zones(west,zone));
        for_west_zones (fn zone => do_zones(east,zone));
        for_north_zones (fn zone => do_zones(south,zone));
        (q', d) 
    end

(*
 * Pressure and Energy Polynomial (page 12)
 *)

fun polynomial(G,degree,rho_table,theta_table,rho_value,theta_value) =
    let fun table_search (table, value : real) =
            let val (low, high) = bounds1  table
                fun search_down  i =
                   if  value > sub1(table,i-1)  then i
                                      else search_down (i-1)
            in  
                if  value>sub1(table,high)  then  high+1
                else if  value <= sub1(table,low)  then low
                     else search_down   high
            end
        val rho_index = table_search(rho_table, rho_value)
        val theta_index = table_search(theta_table, theta_value)
        val A =  sub2(G, (rho_index, theta_index))
        fun from(n,m) = if n>m then [] else n::from(n+1,m)
        fun f(i,j) = sub2(A,(i,j))*pow(rho_value,i)*pow(theta_value,j)
    in
        sum_list (map (fn i => sum_list(map (fn j => f (i,j)) (from(0,degree))))
                      (from (0,degree)))
    end
fun zonal_pressure  (rho_value:real,  theta_value:real)  =
    let
       val (G,degree,rho_table,theta_table) = 
                            extract_pressure_tables_from_constants
    in polynomial(G, degree, rho_table, theta_table, rho_value, theta_value)
    end


fun zonal_energy (rho_value, theta_value) = 
    let val (G, degree, rho_table, theta_table) = 
                            extract_energy_tables_from_constants
    in polynomial(G, degree, rho_table, theta_table, rho_value, theta_value)
    end
val dx =   0.000001
val tiny = 0.000001


fun newton_raphson (f,x) =
    let fun iter (x,fx) =
            if fx > tiny then 
                let val fxdx = f(x+dx)
                    val denom = fxdx - fx
                in if denom < tiny then iter(x,tiny)
                   else iter(x-fx*dx/denom, fxdx)
                end
            else x
    in iter(x, f x)
    end

(*
 * Temperature (page 13-14)
 *)

fun make_temperature(p,epsilon,rho,theta,rho_prime,q_prime) =
    let fun interior_temperature   zone =
            let val qkl = sub2(q_prime,zone)
                val rho_kl = sub2(rho,zone)
                val rho_prime_kl = sub2(rho_prime,zone)
                val tau_kl = (1.0 /rho_prime_kl - 1.0/rho_kl)
                fun energy_equation epsilon_kl   theta_kl =
                    epsilon_kl -  zonal_energy(rho_kl,theta_kl)
                val epsilon_0 = sub2(epsilon,zone)
                fun revised_energy pkl =  epsilon_0 - (pkl + qkl) * tau_kl
                fun revised_temperature  epsilon_kl   theta_kl =
                    newton_raphson ((energy_equation epsilon_kl), theta_kl)
                fun revised_pressure  theta_kl = zonal_pressure(rho_kl, theta_kl)
                val p_0 = sub2(p,zone)
                val theta_0 = sub2(theta,zone)
                val epsilon_1 =  revised_energy    p_0
                val theta_1 =  revised_temperature    epsilon_1    theta_0
                val p_1 =  revised_pressure    theta_1
                val epsilon_2 =  revised_energy    p_1
                val theta_2 =  revised_temperature    epsilon_2    theta_1
            in  theta_2
            end
        val M = array2(dimension_all_zones, constant_heat_source)
    in
        for_interior_zones
              (fn zone => update2(M, zone, interior_temperature zone));
        M
    end


(*
 * Heat conduction
 *)

fun make_cc(alpha_prime, theta_hat) =
    let fun interior_cc zone =  
            (0.0001 * pow(sub2(theta_hat,zone),2) *
            (Math.sqrt (abs(sub2(theta_hat,zone)))) / sub2(alpha_prime,zone))
            handle Sqrt => (print ("<real>" (*Real.makestring (sub2(theta_hat, zone))*));
                            print ("\nzone =(" (* ^ makestring (#1 zone) *) ^ "," ^ 
                                   (* makestring (#2 zone) ^ *) ")\n");
                            printarray2 theta_hat;
                            raise Sqrt)
        val cc = array2(dimension_all_zones, 0.0)
    in
       for_interior_zones(fn zone => update2(cc,zone, interior_cc zone));
       for_south_zones(fn zone => update2(cc,zone, reflect_north zone cc));
       for_west_zones(fn zone => update2(cc,zone,reflect_east zone cc));
       for_east_zones(fn zone => update2(cc,zone,reflect_west zone cc));
       for_north_zones(fn zone => update2(cc,zone, reflect_south zone cc)); 
       cc
    end

fun make_sigma(deltat, rho_prime, alpha_prime) =
    let fun interior_sigma   zone =  
            sub2(rho_prime,zone)*sub2(alpha_prime,zone)*specific_heat/ deltat
        val M = array2(dimension_interior_zones, 0.0)
        fun ohandle zone =
            (print ( (* makestring (sub2(rho_prime, zone)) ^ *)" ");
             print ( (* makestring (sub2(alpha_prime, zone)) ^ *)" ");
             print ( (* makestring specific_heat ^ *) " ");
             print ( (* makestring deltat ^ *) "\n");
             raise Overflow)
                    
    in  if !trace 
        then print ("\t\tmake_sigma:deltat = " (* ^ makestring deltat *) ^ "\n")
        else ();
(***    for_interior_zones(fn zone => update2(M,zone, interior_sigma zone)) **)
        for_interior_zones(fn zone => (update2(M,zone, interior_sigma zone)
                                       handle _ => (*old: Overflow => *)
                                         ohandle zone));
        M
    end

fun make_gamma  ((r_prime,z_prime), cc, succeeding, adjacent) =
    let fun interior_gamma   zone =
            let val r1 = sub2(r_prime, zone_corner_southeast   zone)
                val z1 = sub2(z_prime, zone_corner_southeast   zone)
                val r2 = sub2(r_prime, zone_corner_southeast (adjacent   zone))
                val z2 = sub2(z_prime, zone_corner_southeast (adjacent   zone))
                val cross_section = 0.5*(r1+r2)*(pow(r1 - r2,2)+pow(z1 - z2,2))
                val (c1,c2) = (sub2(cc, zone), sub2(cc, succeeding zone))
                val specific_conductivity =  2.0 * c1 * c2 / (c1 + c2)
            in cross_section  *  specific_conductivity
            end
        val M = array2(dimension_all_zones, 0.0)
    in
        for_interior_zones(fn zone => update2(M,zone,interior_gamma zone));
        M
    end

fun make_ab(theta, sigma, Gamma, preceding) =
    let val a = array2(dimension_all_zones, 0.0)
        val b = array2(dimension_all_zones, 0.0)
        fun interior_ab   zone =
            let val denom = sub2(sigma, zone) + sub2(Gamma, zone) +
                            sub2(Gamma, preceding zone) * 
                            (1.0 - sub2(a, preceding zone))
                val nume1 = sub2(Gamma,zone)
                val nume2 = sub2(Gamma,preceding zone)*sub2(b,preceding zone) +
                            sub2(sigma,zone) * sub2(theta,zone)
            in  (nume1/denom,  nume2 / denom)  
            end
        val f  = fn zone => update2(b,zone,sub2(theta,zone))
    in
        for_north_zones f;
        for_south_zones f;
        for_west_zones f;
        for_east_zones f;
        for_interior_zones(fn zone => let val ab = interior_ab zone
                                      in update2(a,zone,#1 ab);
                                          update2(b,zone,#2 ab)
                                      end);
        (a,b)
    end

fun make_theta (a, b, succeeding, int_zones) =
    let val theta = array2(dimension_all_zones, constant_heat_source)
        fun interior_theta zone =  
            sub2(a,zone) * sub2(theta,succeeding zone)+ sub2(b,zone)
    in
        int_zones (fn (k,l) => update2(theta, (k,l), interior_theta (k,l)));
        theta
    end

fun compute_heat_conduction(theta_hat, deltat, x', alpha', rho') =
    let val sigma       = make_sigma(deltat,  rho',  alpha')
        val _ = if !trace then print "\tdone make_sigma\n" else ()

        val cc          = make_cc(alpha',  theta_hat)
        val _ = if !trace then print "\tdone make_cc\n" else ()

        val Gamma_k     = make_gamma(  x', cc, north, east)
        val _ = if !trace then print "\tdone make_gamma\n" else ()

        val (a_k,b_k)   = make_ab(theta_hat, sigma, Gamma_k, north)
        val _ = if !trace then print "\tdone make_ab\n" else ()

        val theta_k     = make_theta(a_k,b_k,south,for_north_ward_interior_zones)
        val _ = if !trace then print "\tdone make_theta\n" else ()

        val Gamma_l     = make_gamma(x', cc, west, south)
        val _ = if !trace then print "\tdone make_gamma\n" else ()

        val (a_l,b_l)   = make_ab(theta_k, sigma, Gamma_l, west)
        val _ = if !trace then print "\tdone make_ab\n" else ()

        val theta_l     = make_theta(a_l,b_l,east,for_west_ward_interior_zones)
        val _ = if !trace then print "\tdone make_theta\n" else ()
    in  (theta_l, Gamma_k, Gamma_l)
    end


(*
 * Final Pressure and Energy calculation
 *)
fun make_pressure(rho', theta')  =
    let val p = array2(dimension_all_zones, 0.0)
        fun boundary_p(direction, zone) = 
            sub1(pbb, sub2(nbc, zone)) + 
            sub1(pb,sub2(nbc,zone)) * sub2(p, direction zone)
    in
        for_interior_zones
            (fn zone =>
             update2(p,zone,zonal_pressure(sub2(rho',zone),
                                                      sub2(theta',zone))));
        for_south_zones(fn zone => update2(p,zone,boundary_p(north,zone)));
        for_east_zones(fn zone => update2(p,zone,boundary_p(west,zone)));
        for_west_zones(fn zone => update2(p,zone,boundary_p(east,zone)));
        for_north_zones(fn zone => update2(p,zone,boundary_p(south,zone)));
        p
    end

fun make_energy(rho', theta')  =
    let val epsilon' = array2(dimension_all_zones, 0.0)
    in
        for_interior_zones
          (fn zone => update2(epsilon', zone, zonal_energy(sub2(rho',zone),
                                                           sub2(theta',zone))));
        for_south_zones
          (fn zone => update2(epsilon',zone, reflect_north zone epsilon'));
        for_west_zones
          (fn zone => update2(epsilon',zone, reflect_east zone epsilon'));
        for_east_zones
          (fn zone => update2(epsilon',zone, reflect_west  zone epsilon'));
        for_north_zones
          (fn zone => update2(epsilon',zone, reflect_south zone epsilon'));
        epsilon'
    end


(*
 * Energy Error Calculation (page 20)
 *)

fun compute_energy_error  ((u',w'),(r',z'),p',q',epsilon',theta',rho',alpha',
                           Gamma_k,Gamma_l,deltat)  =
    let fun mass zone =  sub2(rho',zone) * sub2(alpha',zone):real
        val internal_energy = 
            sum_list (map_interior_zones (fn z => sub2(epsilon',z)*(mass z)))
        fun kinetic   node =
            let val average_mass =  0.25*((mass (zone_A  node)) + 
                                          (mass (zone_B  node)) +
                                          (mass (zone_C  node)) + 
                                          (mass (zone_D  node)))
                val v_square = pow(sub2(u',node),2) + pow(sub2(w',node),2)
            in 0.5 * average_mass * v_square
            end
        val kinetic_energy =  sum_list (map_interior_nodes kinetic)
      fun work_done (node1, node2)  =
          let val (r1, r2) = (sub2(r',node1), sub2(r',node2))
              val (z1, z2) = (sub2(z',node1), sub2(z',node2))
              val (u1, u2) = (sub2(p',node1), sub2(p',node2))
              val (w1, w2) = (sub2(z',node1), sub2(z',node2))
              val (p1, p2) = (sub2(p',node1), sub2(p',node2))
              val (q1, q2) = (sub2(q',node1), sub2(q',node2))
              val force =  0.5*(p1+p2+q1+q2)
              val radius = 0.5* (r1+r2)
              val area =   0.5* ((r1-r2)*(u1-u2) - (z1-z2)*(w1-w2))
          in  force * radius * area * deltat
          end

      fun from(n,m) = if n > m then [] else n::from(n+1,m)
      val north_line = 
          map (fn l => (west(kmin,l),(kmin,l))) (from(lmin+1,lmax))
      val south_line =
          map (fn l => (west(kmax,l),(kmax,l))) (from(lmin+1,lmax))
      val east_line  =
          map (fn k => (south(k,lmax),(k,lmax))) (from(kmin+1,kmax))
      val west_line  =
          map (fn k => (south(k,lmin+1),(k,lmin+1))) (from(kmin+1,kmax))

      val w1 = sum_list (map work_done north_line)
      val w2  = sum_list (map work_done south_line)
      val w3  = sum_list (map work_done east_line)
      val w4  = sum_list (map work_done west_line)
      val boundary_work =  w1 + w2 + w3 + w4

      fun heat_flow  Gamma (zone1,zone2)  =
        deltat * sub2(Gamma, zone1) * (sub2(theta',zone1) - sub2(theta',zone2))

      val north_flow =
          let val k = kmin+1 
          in map (fn l => (north(k,l),(k,l))) (from(lmin+1,lmax))
          end
      val south_flow =
          let val k = kmax
          in map (fn l => (south(k,l),(k,l))) (from(lmin+2,lmax-1))
          end
      val east_flow  =
          let val l = lmax
          in map (fn k => (east(k,l),(k,l))) (from(kmin+2,kmax))
          end
      val west_flow  =
          let val l = lmin+1
          in map (fn k => (west(k,l),(k,l))) (from(kmin+2,kmax))
          end

     val h1  = sum_list    (map (heat_flow  Gamma_k)   north_flow)
     val h2  = sum_list    (map (heat_flow  Gamma_k)   south_flow)
     val h3  = sum_list    (map (heat_flow  Gamma_l)   east_flow)
     val h4  = sum_list    (map (heat_flow  Gamma_l)   west_flow)
     val boundary_heat =  h1 + h2 + h3 + h4
    in 
        internal_energy  +  kinetic_energy  -  boundary_heat  -  boundary_work
    end

fun compute_time_step(d, theta_hat,  theta') =
    let val deltat_courant =
            min_list (map_interior_zones (fn zone => sub2(d,zone)))
        val deltat_conduct =
            max_list (map_interior_zones 
                        (fn z => (abs(sub2(theta_hat,z) - sub2(theta', z))/
                                  sub2(theta_hat,z))))
        val deltat_minimum = min (deltat_courant, deltat_conduct)
    in min   (deltat_maximum,  deltat_minimum)
    end


fun compute_initial_state () = 
    let 
        val v  = (all_zero_nodes, all_zero_nodes)
        val x  = let fun interior_position  (k,l)  =
                         let val pi = 3.1415926535898
                             val rp = real (lmax - lmin)
                             val z1 = real(10 + k - kmin)
                             val zz = (~0.5 + real(l - lmin) / rp) * pi
                         in (z1 * Math.cos zz,  z1 * Math.sin zz)
                         end
                 in  make_position_matrix interior_position
                 end
        val (alpha,s) = 
            let val (alpha_prime,s_prime) = 
                    let val A = array2(dimension_all_zones, 0.0)
                        val S = array2(dimension_all_zones, 0.0)
                        fun reflect_area_vol f = (f A, f S)

                        fun u2 (f,z) = 
                            let val (a,s) = reflect_area_vol(f z)
                            in update2(A,z,a);
                                update2(S,z,s)
                            end
                    in
                        for_interior_zones 
                           (fn z => let val (a,s) = zone_area_vol(x, z)
                                    in update2(A,z,a);
                                        update2(S,z,s)
                                    end);
                        for_south_zones (fn z => u2 (reflect_north, z));
                        for_east_zones (fn z => u2 (reflect_west, z));
                        for_west_zones (fn z => u2 (reflect_east, z));
                        for_north_zones (fn z => u2 (reflect_south, z));
                        (A,S)
                    end
            in  (alpha_prime,s_prime)
            end
        val rho  = let val R = array2(dimension_all_zones, 0.0)
                   in for_all_zones (fn z => update2(R,z,1.4)); R
                   end
        val theta = 
            let val T = array2(dimension_all_zones, constant_heat_source)
            in for_interior_zones(fn z => update2(T,z,0.0001));
                T
            end
        val p       = make_pressure(rho, theta)
        val q       = all_zero_zones
        val epsilon = make_energy(rho, theta)
        val  deltat  = 0.01
        val  c       = 0.0
    in
        (v,x,alpha,s,rho,p,q,epsilon,theta,deltat,c)
    end


fun compute_next_state state =
    let
        val (v,x,alpha,s,rho,p,q,epsilon,theta,deltat,c) = state
        val v'  = make_velocity (v, x, p, q, alpha, rho, deltat)
        val _ = if !trace then print "done make_velocity\n" else ()

        val x'  = make_position(x,deltat,v') 
                  handle _ => ( (* old: handle Overflow => *)
                                     printarray2 (#1 v'); 
                                     printarray2 (#2 v');
                                     raise Overflow)
        val _ = if !trace then print "done make_position\n" else ()

        val (alpha',rho',s')  = make_area_density_volume (rho,  s , x')
        val _ = if !trace then print "done make_area_density_volume\n"
                else ()

        val (q',d)  = make_viscosity (p,  v',  x',  alpha',  rho')
        val _ = if !trace then print "done make_viscosity\n" else ()

        val theta_hat  = make_temperature (p, epsilon, rho, theta, rho', q')
        val _ = if !trace then print "done make_temperature\n" else ()

        val (theta',Gamma_k,Gamma_l) =
            compute_heat_conduction (theta_hat, deltat, x', alpha', rho')
        val _ = if !trace then print "done compute_heat_conduction\n"
                else ()

        val p'  = make_pressure(rho', theta')
        val _ = if !trace then print "done make_pressure\n" else ()

        val epsilon'  = make_energy (rho', theta')
        val _ = if !trace then print "done make_energy\n" else ()

        val c'  = compute_energy_error (v', x', p', q', epsilon', theta', rho', 
                                        alpha', Gamma_k, Gamma_l,  deltat)
        val _ = if !trace then print "done compute_energy_error\n" 
                else ()

        val deltat'  = compute_time_step (d, theta_hat, theta')
        val _ = if !trace then print "done compute_time_step\n\n" else ()
    in
        (v',x',alpha',s',rho',p',q',  epsilon',theta',deltat',c')
    end

fun runit () = 
    let fun iter (i,state) = if i = 0 then state
                             else (print ".";
                                   iter(i-1, compute_next_state state))
    in iter(step_count, compute_initial_state())
    end

fun print_state ((v1,v2),(r,z),alpha,s,rho,p,q,epsilon,theta,deltat,c) = (
      print "Velocity matrices = \n";
      printarray2 v1; print "\n\n";
      printarray2 v2;

      print "\n\nPosition matrices = \n";
      printarray2 r; print "\n\n";
      printarray2 z;
     
      print "\n\nalpha = \n";
      printarray2 alpha;

      print "\n\ns = \n";
      printarray2 s;

      print "\n\nrho = \n";
      printarray2 rho;

      print "\n\nPressure = \n";
      printarray2 p;

      print "\n\nq = \n";
      printarray2 q;
    
      print "\n\nepsilon = \n";
      printarray2 epsilon;

      print "\n\ntheta = \n";
      printarray2 theta;

      print ("delatat = " (* ^ Real.makestring deltat *) ^ "\n");
      print ("c = " (* ^ Real.makestring c *) ^ "\n"))

    fun testit outstrm = print_state (runit())

    fun doit () = let
          val (_, _, _, _, _, _, _, _, _, delta', c') = runit()
          val delta : int = floor (* truncate *) delta'
          val c : int = floor (* truncate *) (c' * 10000.0)
          val _ = print(int_to_string(c))
          val _ = print("\n")
          val _ = print(int_to_string(delta))
          val _ = print("\n")
          in
            if (c = 3072 andalso delta = ~61403) (* for grid_max = 30 *)
              (* (c = 6787 andalso delta = ~33093) *)
              then ()
              else print("*** ERROR ***\n")
                  (*old : IO.output (IO.std_err, "*** ERROR ***\n") *)
          end

(*
  end; (* functor Simple *)

structure Main = Simple(val grid_max=100 val step_count=1);
*)

val _ = doit();

