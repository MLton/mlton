fun die str = (
       print (str ^ "\n");
       raise Overflow
       )

local fun loop (big: IntInf.int, small: IntInf.int): IntInf.int =
             if small = 0
                then big
                else loop (small,
                           IntInf.rem (big, small))
in fun gcd (x: IntInf.int, y: IntInf.int): IntInf.int =
          let val x = IntInf.abs x
              val y = IntInf.abs y
              val (x, y) = if x >= y
                              then (x, y)
                              else (y, x)
          in loop (x, y)
          end
end

fun reduce (num: IntInf.int, den: IntInf.int) : IntInf.int * IntInf.int =
       let val g = gcd (num, den)
           val gs = if den >= 0
                       then g
                       else ~ g
       in if gs = 1
             then (num, den)
          else let val rnum = IntInf.quot (num, gs)
                   val badn = IntInf.rem (num, gs)
                   val rden = IntInf.quot (den, gs)
                   val badd = IntInf.rem (den, gs)
               in if badn <> 0
                  orelse num <> rnum * gs
                  orelse badd <> 0
                  orelse den <> rden * gs
                     then die ("Bad: num " ^ (IntInf.toString num)
                               ^ ", den " ^ (IntInf.toString den)
                               ^ ", gcds " ^ (IntInf.toString gs)
                               ^ ", rnum " ^ (IntInf.toString rnum)
                               ^ ", rden " ^ (IntInf.toString rden)
                               ^ ", badn " ^ (IntInf.toString badn)
                               ^ ", badd " ^ (IntInf.toString badd))
                  else ();
                       (rnum, rden)
               end
       end

fun addrecip (xxx: int, (num: IntInf.int, den: IntInf.int))
             : IntInf.int * IntInf.int =
       let val xxx = IntInf.fromInt xxx
           val xnum = xxx * num + den
           val xden = xxx * den
       in reduce (xnum, xden)
       end

fun printRat (num: IntInf.int, den: IntInf.int): unit =
        print (IntInf.toString num ^ "/" ^ IntInf.toString den ^ "\n")

fun spin (limit: int): IntInf.int * IntInf.int =
       let fun loop (n: int, res: IntInf.int * IntInf.int)
                    : IntInf.int * IntInf.int =
                  if n = limit
                     then res
                  else loop (n + 1,
                             addrecip (n, res))
       in if limit <= 0
             then die "Bad limit"
             else loop (1, (0, 1))
       end

val (n, d) = spin 3000
val _ = printRat (n, d)
val _ = printRat (reduce (n * d * n, d * d * n))
val _ = printRat (reduce (n + 1, d + 1))
val _ = printRat (reduce (n * (d + 1) + d * (n + 1), d * (d + 1)))
