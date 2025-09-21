(* io-manager.sig
 *  Ported to MLton threads.
 *)

(* io-manager.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * This is a generic I/O manager for CML.  It uses the OS.IO polling
 * mechanism.
 * NOTE: it currently does not work if more than one thread blocks on the same
 * descriptor.
 *)
signature IO_MANAGER =
sig
  type iodesc
  type poll_desc
  type poll_info
  datatype preempt_type = READIED | INQUEUE of poll_desc list | EMPTYQUEUE

  val ioEvt: poll_desc -> poll_info Event.event

  val preempt: unit -> preempt_type
end
