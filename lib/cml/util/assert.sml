(* assert.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

structure Assert: ASSERT =
   struct
      structure C = Critical
      val assertFlg = false

      fun fail msg =
         (C.atomicBegin ();
          TextIO.print (concat ["ASSERT: ", msg, "\n"]);
          OS.Process.exit OS.Process.failure)

      fun assert (msgs: (unit -> string) list, 
                  msg: unit -> string, 
                  f: unit -> bool): unit =
         if assertFlg andalso not (f () handle _ => false)
            then let
                    val msgs = List.map (fn f => f ()) msgs
                    val msg = concat [String.concatWith " " msgs, " :: ", msg ()]
                 in
                    fail msg
                 end
            else ()
      fun assert' (msg: string, f: unit -> bool): unit =
         assert ([], fn () => msg, f)

      datatype z = datatype MLton.Thread.AtomicState.t
      fun assertAtomic (msg: unit -> string, n: int option): unit =
         assert ([C.atomicMsg], msg, fn () =>
                 case MLton.Thread.atomicState () of
                    Atomic m => (case n of NONE => true | SOME n => n = m)
                  | NonAtomic => false)
      fun assertNonAtomic (msg: unit -> string): unit =
         assert ([C.atomicMsg], msg, fn () =>
                 case MLton.Thread.atomicState () of
                    Atomic _ => false
                  | NonAtomic => true)
      fun assertAtomic' (msg, n) = assertAtomic (fn () => msg, n)
      fun assertNonAtomic' msg = assertNonAtomic (fn () => msg)
   end
