(* Copyright (C) 2001-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonExn =
   struct
      open Primitive.MLton.Exn

      type t = exn

      val addExnMessager = General.addExnMessager

      val history: t -> string list =
         if keepHistory then
            (setInitExtra (NONE: extra)
             ; setExtendExtra (fn e =>
                               case e of
                                  NONE => SOME (MLtonCallStack.current ())
                                | SOME _ => e)
             ; (fn e =>
                case extra e of
                   NONE => []
                 | SOME cs =>
                      let
                         (* Gets rid of the anonymous function passed to
                          * setExtendExtra above.
                          *)
                         fun loop xs =
                            case xs of
                               [] => []
                             | x :: xs =>
                                  if String.isPrefix "MLtonExn.fn " x then
                                     xs
                                  else
                                     loop xs
                      in
                         loop (MLtonCallStack.toStrings cs)
                      end))
         else fn _ => []

      local
         val message = PrimitiveFFI.Stdio.print
      in
         fun 'a topLevelHandler (exn: exn): 'a =
            (message (concat ["unhandled exception: ", exnMessage exn, "\n"])
             ; (case history exn of
                   [] => ()
                 | l =>
                      (message "with history:\n"
                       ; (List.app (fn s => message (concat ["\t", s, "\n"]))
                          l)))
             ; Exit.exit Exit.Status.failure)
            handle _ => (message "Toplevel handler raised exception.\n"
                         ; Primitive.MLton.halt Exit.Status.failure
                         (* The following raise is unreachable, but must be there
                          * so that the expression is of type 'a.
                          *)
                         ; raise Fail "bug")
      end
   end
