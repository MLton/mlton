(* From the SML/NJ benchmark suite. *)
fun print _ = ()
signature BMARK =
  sig
    val doit : int -> unit
  end;
structure Main: BMARK = struct

local
open Array Math

val printr = print o (Real.fmt (StringCvt.SCI(SOME 14)))
val printi = print o Int.toString
in

val PI = 3.14159265358979323846

val tpi = 2.0 * PI
(*
fun trace(name,f) x =
   (print name ;
    print "(" ;
    printr x ;
    print ") = " ;
    let val y = f x
    in printr y ;
       print "\n" ;
       y
    end)

fun trace2(name,f) (x,y) =
   (printr x ;
    print " ";
    print name ;
    print " ";
    printr y ;
    print " = " ;
    let val z = f(x,y)
    in printr z ;
       print "\n" ;
       z
    end)

fun trace2(_,f) = f

val op * = trace2("*", Real.* )
val op - = trace2("-", Real.-)
val op + = trace2("+", Real.+)

local
   nonfix * + -
in
   _overload * : ('a * 'a -> 'a)
   as Int.*
   and *

      _overload + : ('a * 'a -> 'a)
      as Int.+
   and +

      _overload - : ('a * 'a -> 'a)
      as Int.-
   and -
end

val sin = trace("sin", sin)
val cos = trace("cos", cos)

val sub =
   fn (a,i) =>
   let val x = sub(a,i)
   in print "sub(_, " ;
      printi i ;
      print ") = ";
      printr x ;
      print "\n" ;
      x
   end

val update =
   fn (a,i,x) =>
   (update(a,i,x);
    print "update(_, " ;
    printi i ;
    print ", " ;
    printr x ;
    print ")\n")

*)
fun fft px py np =
  let
     fun find_num_points i m =
        if i < np
           then find_num_points (i+i) (m+1)
        else (i,m)
     val (n,m) = find_num_points 2 1
(*     val _ = (printi n ;
              print "\n" ;
              printi m ;
              print "\n") *)
  in
     if n <> np then
        let
           fun loop i =
              if i > n then ()
              else (update(px, i, 0.0);
                    update(py, i, 0.0);
                    loop (i+1))
        in
           loop (np+1);
           print "Use "; printi n; print " point fft\n"
        end
     else ();
        
     let
        fun loop_k k n2 =
           if k >= m then ()
           else
              let
                 val n4 = n2 div 4
                 val e  = tpi / (real n2)
                 fun loop_j j a =
                    if j > n4 then ()
                    else
                       let val a3 = 3.0 * a
                          val cc1 = cos(a)
                          val ss1 = sin(a)
                          val cc3 = cos(a3)
                          val ss3 = sin(a3)
                          fun loop_is is id =
                             if is >= n
                                then ()
                             else
                                let
                                   fun loop_i0 i0 =
                                      if i0 >= n
                                         then ()
                                      else
                                         let val i1 = i0 + n4
                                            val i2 = i1 + n4
                                            val i3 = i2 + n4
                                            val r1 = sub(px, i0) - sub(px, i2)
                                            val _ = update(px, i0, sub(px, i0) + sub(px, i2))
                                            val r2 = sub(px, i1) - sub(px, i3)
                                            val _ = update(px, i1, sub(px, i1) + sub(px, i3))
                                            val s1 = sub(py, i0) - sub(py, i2)
                                            val _ = update(py, i0, sub(py, i0) + sub(py, i2))
                                            val s2 = sub(py, i1) - sub(py, i3)
                                            val _ = update(py, i1, sub(py, i1) + sub(py, i3))
                                            val s3 = r1 - s2
                                            val r1 = r1 + s2
                                            val s2 = r2 - s1
                                            val r2 = r2 + s1
                                            val _ = update(px, i2, r1*cc1 - s2*ss1)
                                            val _ = update(py, i2, ~s2*cc1 - r1*ss1)
                                            val _ = update(px, i3, s3*cc3 + r2*ss3)
                                            val _ = update(py, i3, r2*cc3 - s3*ss3)
                                         in
                                            loop_i0 (i0 + id)
                                         end
                                in
                                   loop_i0 is;
                                   loop_is (2 * id - n2 + j) (4 * id)
                                end
                       in
                          loop_is j (2 * n2);
                          loop_j (j+1) (e * real j)
                       end
              in
                 loop_j 1 0.0;
                 loop_k (k+1) (n2 div 2)
              end
     in
        loop_k 1 n
     end;
     
(************************************)
(*  Last stage, length=2 butterfly  *)
(************************************)

let fun loop_is is id = if is >= n then () else
   let fun loop_i0 i0 = if i0 > n then () else
      let val i1 = i0 + 1
         val r1 = sub(px, i0)
         val _ = update(px, i0, r1 + sub(px, i1))
         val _ = update(px, i1, r1 - sub(px, i1))
          val r1 = sub(py, i0)
          val _ = update(py, i0, r1 + sub(py, i1))
          val _ = update(py, i1, r1 - sub(py, i1))
      in
        loop_i0 (i0 + id)
      end
    in
      loop_i0 is;
      loop_is (2*id - 1) (4 * id)
    end
  in
    loop_is 1 4
  end;

(*************************)
(*  Bit reverse counter  *)
(*************************)

  let
     fun loop_i i j =
        if i >= n
           then ()
        else
           (if i < j
               then (let val xt = sub(px, j)
                     in update(px, j, sub(px, i)); update(px, i, xt)
                     end;
                     let val xt = sub(py, j)
                     in update(py, j, sub(py, i)); update(py, i, xt)
                     end)
            else ();
            let
               fun loop_k k j =
                  if k < j then loop_k (k div 2) (j-k) else j+k
               val j' = loop_k (n div 2) j
            in
               loop_i (i+1) j'
            end)
  in
     loop_i 1 1
  end;

  n
  
  end

fun abs x = if x >= 0.0 then x else ~x

fun test np =
  let val _ = (printi np; print "... ")
      val enp = real np
      val npm = (np div 2) - 1
      val pxr = array (np+2, 0.0)
      val pxi = array (np+2, 0.0)
      val t = PI / enp
      val _ = update(pxr, 1, (enp - 1.0) * 0.5)
      val _ = update(pxi, 1, 0.0)
      val n2 = np  div  2
      val _ = update(pxr, n2+1, ~0.5)
      val _ = update(pxi, n2+1,  0.0)
      fun loop_i i = if i > npm then () else
        let val j = np - i
            val _ = update(pxr, i+1, ~0.5)
            val _ = update(pxr, j+1, ~0.5)
            val z = t * real i
            val y = ~0.5*(cos(z)/sin(z))
            val _ = update(pxi, i+1,  y)
            val _ = update(pxi, j+1, ~y)
        in
          loop_i (i+1)
        end
      val _ = loop_i 1

(*      val _ = print "\n"
      fun loop_i i = if i > 15 then () else
        (printi i; print "\t";
         printr (sub(pxr, i+1)); print "\t";
         printr (sub(pxi, i+1)); print "\n"; loop_i (i+1))
      val _ = loop_i 0
*)
      val _ = fft pxr pxi np
(*
      fun loop_i i = if i > 15 then () else
        (printi i; print "\t";
         printr (sub(pxr, i+1)); print "\t";
         printr (sub(pxi, i+1)); print "\n"; loop_i (i+1))
      val _ = loop_i 0
*)
      fun loop_i i zr zi kr ki = if i >= np then (zr,zi) else
        let val a = abs(sub(pxr, i+1) - real i)
            val (zr, kr) =
              if zr < a then (a, i) else (zr, kr)
            val a = abs(sub(pxi, i+1))
            val (zi, ki) =
              if zi < a then (a, i) else (zi, ki)
        in
          loop_i (i+1) zr zi kr ki
        end
      val (zr, zi) = loop_i 0 0.0 0.0 0 0
      val zm = if abs zr < abs zi then zi else zr
  in
    printr zm; print "\n"
  end

fun loop_np i np = if i > 15 then () else
  (test np; loop_np (i+1) (np*2))

fun doit n =
   if n = 0
      then ()
   else (loop_np 1 256; doit (n - 1))

end
end;
