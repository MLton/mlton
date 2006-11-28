(* multicast.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* multicast.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Asynchronous multicast (one-to-many) channels.  This implementation
 * is based on a condition variable implementation of multicast channels.
 * See Chapter 5 of "Concurrent Programming in ML" for details.
 *)

structure Multicast : MULTICAST =
   struct

      structure SV = SyncVar

      type 'a event = 'a CML.event

      datatype 'a request = 
         Message of 'a 
       | NewPort 
      datatype 'a mc_state = MCState of ('a * 'a mc_state SV.ivar)
      datatype 'a port =
         Port of (('a * 'a mc_state SV.ivar) CML.chan * 'a mc_state SV.ivar SV.mvar)
      datatype 'a mchan = 
         MChan of ('a request CML.chan * 'a port CML.chan)

    fun mkPort cv = 
       let
          val outCh = CML.channel()
          val stateVar = SV.mVarInit cv
          fun tee cv = 
             let
                val (MCState(v, nextCV)) = SV.iGet cv
             in
                CML.send (outCh, (v, nextCV))
                ; tee nextCV
             end
          val _ = CML.spawn (fn () => tee cv)
       in
          Port(outCh, stateVar)
       end

    fun mChannel () = 
       let
          val reqCh = CML.channel() 
          and replyCh = CML.channel()
          fun server cv = 
             case (CML.recv reqCh) of 
                NewPort => 
                   (CML.send (replyCh, mkPort cv)
                    ; server cv)
              | (Message m) => 
                   let
                      val nextCV = SV.iVar()
                   in
                      SV.iPut (cv, MCState(m, nextCV))
                      ; server nextCV
                   end
          val _ = CML.spawn (fn () => server (SV.iVar()))
       in
          MChan(reqCh, replyCh)
       end

    fun multicast (MChan(ch, _), m) = CML.send (ch, Message m)

    fun port (MChan(reqCh, replyCh)) = 
       (CML.send (reqCh, NewPort)
        ; CML.recv replyCh)

    fun copy (Port(_, stateV)) = mkPort(SV.mGet stateV)

    fun recvMsg stateV (v, nextCV) = 
       let val _ = SV.mSwap (stateV, nextCV)
       in v
       end

    fun recv (Port(ch, stateV)) = recvMsg stateV (CML.recv ch)
    fun recvEvt (Port(ch, stateV)) = CML.wrap(CML.recvEvt ch, recvMsg stateV)
   end

