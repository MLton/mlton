(* trans-id.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* ???
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

signature TRANS_ID =
   sig
      datatype trans_id = datatype RepTypes.trans_id
      datatype trans_id_state = datatype RepTypes.trans_id_state

      (* create a new transaction ID. *)
      val mkTxId : unit -> trans_id
      (* create a transaction flag (ID and cleanUp). *)
      val mkFlg  : unit -> (trans_id * (unit -> unit))
      (* given a transaction ID, mark it cancelled. *)
      val force : trans_id -> unit

      (*val toString : trans_id -> string*)
   end
