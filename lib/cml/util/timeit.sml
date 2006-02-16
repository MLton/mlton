(* timeit.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

structure TimeIt : TIMEIT =
   struct
      val timeitFlg = true

      fun timeit (name: string) (f: 'a -> 'b) (a: 'a) : 'b =
         if timeitFlg 
            then let
                    val start = Time.now ()
                    fun done () =
                       let
                          val finish = Time.now ()
                          val diff = Time.-(finish, start)
                       in
                          Debug.sayDebug 
                          ([], fn () =>
                           concat [name, ": ",
                                   LargeInt.toString (Time.toMilliseconds diff),
                                   " ms"])
                       end
                 in
                    (f a before done ())
                    handle e => (done (); raise e)
                 end
            else f a
   end
