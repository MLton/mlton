(* simple-rpc.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* simple-rpc-sig.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *
 * Generators for simple RPC protocols.
 *)

signature SIMPLE_RPC = 
   sig
      type 'a event = 'a CML.event

      val mkRPC : ('a -> 'b) -> 
         {call     : 'a -> 'b,
          entryEvt : unit event}

      val mkRPC_In : (('a * 'c) -> 'b) -> 
         {call     : 'a -> 'b,
          entryEvt : 'c -> unit event}

      val mkRPC_Out : ('a -> ('b * 'c)) -> 
         {call     : 'a -> 'b,
          entryEvt : 'c event}

      val mkRPC_InOut : (('a * 'c) -> ('b * 'd)) -> 
         {call     : 'a -> 'b,
          entryEvt : 'c -> 'd event}
  end
