(* local-assert.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

functor LocalAssert(val assert: bool): ASSERT =
   struct
      fun make f =
         if assert then f else fn _ => ()
      val assert = make Assert.assert
      val assert' = make Assert.assert'
      val assertAtomic = make Assert.assertAtomic
      val assertNonAtomic = make Assert.assertNonAtomic
      val assertAtomic' = make Assert.assertAtomic'
      val assertNonAtomic' = make Assert.assertNonAtomic'
   end
