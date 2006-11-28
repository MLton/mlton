(* trans-id.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* ???
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure TransID : TRANS_ID =
   struct 
      structure Assert = LocalAssert(val assert = false)

      structure R = RepTypes


      (* Transaction IDs are used to mark blocked threads in the various waiting
       * queues.  They are "cancelled" when some other event is selected.
       *)
      datatype trans_id = datatype R.trans_id
      datatype trans_id_state = datatype R.trans_id_state

      (* create a new transaction ID. *)
      fun mkTxId () = TXID(ref TRANS)

      (* create a transaction flag (ID and cleanUp). *)
      fun mkFlg () =
         let 
            val txid as TXID txst = mkTxId ()
            val cleanUp = fn () =>
               (Assert.assertAtomic' ("TransID.mkFlg.cleanUp", NONE)
                ; txst := CANCEL)
         in 
            (txid, cleanUp)
         end

      (* given a transaction ID, mark it cancelled. *)
      fun force (TXID txst) =
         (Assert.assertAtomic' ("TransID.force", NONE)
          ; case !txst of
               TRANS => txst := CANCEL
             | CANCEL => raise Fail "TransID.force")

      (*
      fun toString (TXID txst) =
         case !txst of
            TRANS => "TRANS"
          | CANCEL => "CANCEL"
      *)
   end
