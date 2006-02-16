(* Written by Stephen Weeks (sweeks@sweeks.com). *)
structure Array = Array2
   
fun 'a fold (n : int, b : 'a, f : int * 'a -> 'a) =
   let
      fun loop (i : int, b : 'a) : 'a =
         if i = n
            then b
         else loop (i + 1, f (i, b))
   in loop (0, b)
   end

fun foreach (n : int, f : int -> unit) : unit =
   fold (n, (), f o #1)
      
fun mult (a1 : real Array.array, a2 : real Array.array) : real Array.array =
   let
      val r1 = Array.nRows a1
      val c1 = Array.nCols a1
      val r2 = Array.nRows a2
      val c2 = Array.nCols a2
   in if c1 <> r2
         then raise Fail "mult"
      else
         let val a = Array2.array (r1, c2, 0.0)
            fun dot (r, c) =
               fold (c1, 0.0, fn (i, sum) =>
                    sum + Array.sub (a1, r, i) * Array.sub (a2, i, c))
         in foreach (r1, fn r =>
                    foreach (c2, fn c =>
                            Array.update (a, r, c, dot (r,c))));
            a
         end
   end

structure Main =
   struct
      fun doit () =
         let
            val dim = 500
            val a = Array.tabulate Array.RowMajor (dim, dim, fn (r, c) =>
                                                   Real.fromInt (r + c))
         in
            if Real.== (41541750.0, Array2.sub (mult (a, a), 0, 0))
               then ()
            else raise Fail "bug"
         end
      
      val doit =
         fn size =>
         let
            fun loop n =
               if n = 0
                  then ()
               else (doit ();
                     loop (n-1))
         in loop size
         end
   end
