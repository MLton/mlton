(* debug.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* debug.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Debugging support for the CML core.
 *)

structure Debug : DEBUG =
   struct
      structure C = Critical
      val debugFlg = false

      fun sayDebug (msgs: (unit -> string) list, 
                    msg: unit -> string) =
         if debugFlg
            then let
                    val msgs = List.map (fn f => f ()) msgs
                    val msg = concat [String.concatWith " " msgs, " :: ", msg ()]
                 in
                    C.atomicBegin ();
                    TextIO.print (concat [msg, "\n"]);
                    C.atomicEnd ()
                 end
            else ()
      fun sayDebug' (msg: string) = sayDebug ([], fn () => msg)
   end
