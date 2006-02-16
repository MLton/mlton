(* result.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* result.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 *)

structure Result :> RESULT =
   struct

      structure SV = SyncVar

      datatype 'a result_val = EXN of exn | RES of 'a

      type 'a result = 'a result_val SV.ivar

      fun result () = SV.iVar()
      fun put (iv, v) = SV.iPut(iv, RES v)
      fun putExn (iv, ex) = SV.iPut(iv, EXN ex)
      fun wrap (RES v) = v
        | wrap (EXN ex) = raise ex
      fun get iv = wrap(SV.iGet iv)
      fun getEvt iv = CML.wrap(SV.iGetEvt iv, wrap)
  end
