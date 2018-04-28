(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Assert: ASSERT =
   struct
      val debug = MLton.debug orelse (not MLton.isMLton)

      fun fail msg = Error.bug (concat ["assertion failure: ", msg])

      fun assert (msg: string, f: unit -> bool): unit =
         if debug andalso not (f () handle _ => false)
            then fail msg
         else ()

      fun assert' (msg, b) = assert (msg, fn () => b)

      val ('a, 'b) assertFun':
         string
         * ('a -> 'b)
         * ('a -> bool * ('b -> bool * 'b))
         -> 'a -> 'b =
         (* Can't do what I really want because of the value restriction.
          * Would like to write:
          *   if debug then (fn ... => ...) else (fn ... => ...).
          *)
         fn (msg, f, check) =>
         if debug
            then (fn a =>
                  let val (yes, check) = check a
                     val _ = assert' (concat [msg, " argument"], yes)
                     val (yes, b) = check (f a)
                  in assert' (concat [msg, " result"], yes)
                     ; b
                  end)
         else f

      fun assertFun (msg,
                    f: 'a -> 'b,
                    check: 'a -> bool * ('b -> bool)): 'a -> 'b =
         assertFun' (msg, f,
                    fn a => let val (yes, check) = check a
                            in (yes, fn b => (check b, b))
                            end)

      fun assertFun2 (msg,
                     f: 'a -> 'b -> 'c,
                     check: 'a -> bool * ('b -> (bool * ('c -> bool)))) =
         assertFun'
         (msg, f,
          fn a => let val (yes, check) = check a
                  in (yes,
                      fn bc => (true, assertFun (msg, bc, check)))
                  end)
   end
