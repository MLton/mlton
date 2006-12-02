(* multicast.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* multicast-sig.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Asynchronous multicast (one-to-many) channels.
 *)

signature MULTICAST =
   sig
      type 'a mchan
      type 'a port
      type 'a event = 'a CML.event

      (* create a new multicast channel *)
      val mChannel : unit -> 'a mchan
      (* create a new output port on a channel *)
      val port : 'a mchan -> 'a port
      (* create a new output port on a channel that has the same state as the
       * given port.  I.e., the stream of messages seen on the two ports will
       * be the same.
       * NOTE: if two (or more) independent threads are reading from the
       * same port, then the copy operation may not be accurate.
       *)
      val copy : 'a port -> 'a port
      (* receive a message from a port *)
      val recv : 'a port -> 'a
      val recvEvt : 'a port -> 'a event
      (* send a message to all of the ports of a channel *)
      val multicast : ('a mchan * 'a) -> unit
   end
