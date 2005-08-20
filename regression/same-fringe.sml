structure Z =
struct

structure Thread = MLton.Thread

fun generate(f: ('a -> unit) -> unit): unit -> 'a option =
   let
      val paused: 'a option Thread.t option ref = ref NONE
      val gen: unit Thread.t option ref = ref NONE
      fun return(a: 'a option): unit =
         Thread.switch(fn t' =>
                       let val _ = gen := SOME t'
                          val t = valOf(!paused)
                          val _ = paused := NONE
                       in Thread.prepare (t, a)
                       end)
      val _ =
         gen := SOME(Thread.new(fn () => (f (return o SOME)
                                          ; return NONE)))
   in fn () => Thread.switch(fn t => (paused := SOME t
                                      ; Thread.prepare (valOf(!gen), ())))
   end
   
datatype 'a tree =
    L of 'a
  | N of 'a tree * 'a tree

fun foreach(t: 'a tree, f: 'a -> unit): unit =
   let
      val rec loop =
         fn L a => f a
          | N(l, r) => (loop l; loop r)
   in loop t
   end

fun same(f: unit -> 'a option,
         g: unit -> 'a option,
         eq: 'a * 'a -> bool): bool =
   let
      fun loop() =
         case (f(), g()) of
            (NONE, NONE) => true
          | (SOME x, SOME y) => eq(x, y) andalso loop()
          | _ => false
   in loop()
   end

fun fringe(t: 'a tree): unit -> 'a option =
   generate(fn f => foreach(t, f))

fun sameFringe(t1: 'a tree, t2: 'a tree, eq: 'a * 'a -> bool): bool =
   same(fringe t1, fringe t2, eq)

val t1 = N(N(L 1, L 2), N(N(L 3, L 4), L 5))
val t2 = N(L 1, N(N(L 2, L 3), N(L 4, L 5)))

val _ =
   if sameFringe(t1, t2, op =)
      then print "success\n"
   else print "failure\n"
         
end
