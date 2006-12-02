(* local-debug.fun
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

functor LocalDebug(val debug: bool): DEBUG =
   struct
      fun make f =
         if debug then f else fn _ => ()
      val sayDebug' = make Debug.sayDebug'
      val sayDebug = make Debug.sayDebug
   end
