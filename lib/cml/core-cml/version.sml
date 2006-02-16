(* version.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* version.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Version : VERSION = 
   struct
      val version = {
                     system = "Concurrent ML (MLton)",
                     version_id = [1, 0, 10],
                     date = "March, 2004"
                     }

      fun f ([], l) = l
        | f ([x], l) = (Int.toString x)::l
        | f (x::r, l) = (Int.toString x) :: "." :: f(r, l)

      val banner = 
         concat (
                 #system version :: 
                 ", Version " ::
                 f (#version_id version, [", ", #date version])
                 )
   end

