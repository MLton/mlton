(* sync-var.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* sync-var-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The implementation of Id-style synchronizing memory cells (I-structures
 * and M-structures).
 *)

signature SYNC_VAR =
   sig

      type 'a ivar  (* I-structure variable *)
      type 'a mvar  (* M-structure variable *)

      exception Put (* raised on put operations to full cells *)

      val iVar     : unit -> 'a ivar
      val iPut     : ('a ivar * 'a) -> unit
      val iGet     : 'a ivar -> 'a
      val iGetEvt  : 'a ivar -> 'a CML.event
      val iGetPoll : 'a ivar -> 'a option
      val sameIVar : ('a ivar * 'a ivar) -> bool

      val mVar      : unit -> 'a mvar
      val mVarInit  : 'a -> 'a mvar
      val mPut      : ('a mvar * 'a) -> unit
      val mTake     : 'a mvar -> 'a
      val mTakeEvt  : 'a mvar -> 'a CML.event
      val mTakePoll : 'a mvar -> 'a option
      val mGet      : 'a mvar -> 'a
      val mGetEvt   : 'a mvar -> 'a CML.event
      val mGetPoll  : 'a mvar -> 'a option
      val mSwap     : ('a mvar * 'a) -> 'a
      val mSwapEvt  : ('a mvar * 'a) -> 'a CML.event
      val sameMVar  : ('a mvar * 'a mvar) -> bool
   end

signature SYNC_VAR_EXTRA =
   sig
      include SYNC_VAR
   end
