(* critical.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

signature CRITICAL =
   sig
      val atomicBegin : unit -> unit
      val atomicEnd : unit -> unit
      val atomicMsg : unit -> string
      val doAtomic : (unit -> unit) -> unit

      val maskBegin : unit -> unit
      val maskEnd : unit -> unit
      val doMasked : (unit -> unit) -> unit
   end
