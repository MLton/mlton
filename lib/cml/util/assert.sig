(* assert.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

signature ASSERT =
   sig
      val assert: (unit -> string) list * (unit -> string) * (unit -> bool) -> unit
      val assert': string * (unit -> bool) -> unit
      val assertAtomic: (unit -> string) * int option -> unit
      val assertNonAtomic: (unit -> string) -> unit
      val assertAtomic': string * int option -> unit
      val assertNonAtomic': string -> unit
   end
