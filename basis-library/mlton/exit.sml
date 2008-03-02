(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Exit =
   struct
      structure Status = 
         struct
            open PreOS.Process.Status
            val fromInt = fromC o C_Status.fromInt
            val toInt = C_Status.toInt o toC
            val failure = fromInt 1
            val success = fromInt 0
         end

      val exiting = ref false

      fun atExit f =
         if !exiting
            then ()
         else Cleaner.addNew (Cleaner.atExit, f)

      fun halt (status: Status.t) =
         Primitive.MLton.halt (Status.toC status)

      fun exit (status: Status.t): 'a =
         if !exiting
            then raise Fail "exit"
         else
            let
               val _ = exiting := true
               val i = Status.toInt status
            in
               if 0 <= i andalso i < 256
                  then (let open Cleaner in clean atExit end
                        ; halt status
                        ; raise Fail "exit")
               else raise Fail (concat ["exit must have 0 <= status < 256: saw ",
                                        Int.toString i])
            end
   end
