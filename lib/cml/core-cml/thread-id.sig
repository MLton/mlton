(* thread-id.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* threads-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

type word = Word.word
signature THREAD_ID =
   sig
      datatype thread_id = datatype RepTypes.thread_id

      val sameTid    : (thread_id * thread_id) -> bool
      val compareTid : (thread_id * thread_id) -> order
      val hashTid    : thread_id -> word

      val tidToString : thread_id -> string
   end

signature THREAD_ID_EXTRA =
   sig
      include THREAD_ID
      val new : unit -> thread_id
      val bogus : string -> thread_id

      val mark     : thread_id -> unit
      val unmark   : thread_id -> unit
      val isMarked : thread_id -> bool

      val reset : unit -> unit
   end
