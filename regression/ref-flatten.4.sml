structure CList =
   struct
      datatype 'a clist' = Cons of 'a * 'a clist ref
      withtype 'a clist = 'a clist' option

      fun cnil () = NONE
      fun ccons (h, t) = SOME (Cons (h, ref t))

      fun match cl nilCase consCase =
         case cl of
            NONE => nilCase ()
          | SOME (Cons (h, t)) => consCase (h, !t)

      fun fromList l =
         case l of
            [] => cnil ()
          | h::t => ccons (h, fromList t)

      fun repeat x =
         let
            val r = ref NONE
            val cl = SOME (Cons (x, r))
            val () = r := cl
         in
            cl
         end

      local
         val max = 1000
         fun length' (cl, n) =
            if n >= max
               then NONE
               else match cl
                          (fn () => SOME n)
                          (fn (_,t) => length' (t, n + 1))
      in
         fun length cl = length' (cl, 0)
      end
   end

val cl = CList.repeat #"x"
val n = CList.length cl
val () =
   case n of
      NONE => print "NONE\n"
    | SOME n => print (concat ["SOME ", Int.toString n, "\n"])

val cl = CList.fromList [1,2,3,4,5,6,7,8,9]
val n = CList.length cl
val () =
   case n of
      NONE => print "NONE\n"
    | SOME n => print (concat ["SOME ", Int.toString n, "\n"])
