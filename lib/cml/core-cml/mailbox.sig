(* mailbox.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* mailbox-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Asynchronous channels (called mailboxes).
 *)

signature MAILBOX =
   sig
      type 'a mbox

      val mailbox     : unit -> 'a mbox
      val sameMailbox : ('a mbox * 'a mbox) -> bool

      val send     : ('a mbox * 'a) -> unit
      val recv     : 'a mbox -> 'a
      val recvEvt  : 'a mbox -> 'a CML.event
      val recvPoll : 'a mbox -> 'a option
   end

signature MAILBOX_EXTRA =
   sig
      include MAILBOX
      (*val resetMbox : 'a mbox -> unit*)
   end
