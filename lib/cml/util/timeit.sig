(* timeit.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

signature TIMEIT =
   sig
      val timeit : string -> ('a -> 'b) -> 'a -> 'b
   end
