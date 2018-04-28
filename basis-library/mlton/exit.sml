(* Copyright (C) 2004-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Exit =
   struct
      structure Status = 
         struct
            open PreOS.Status
            val fromInt = fromRep o C_Status.fromInt
            val toInt = C_Status.toInt o toRep
            val failure = fromInt 1
            val success = fromInt 0
         end

      val exiting = ref false

      fun atExit f =
         if !exiting
            then ()
         else Cleaner.addNew (Cleaner.atExit, f)

      fun halt (status: Status.t) =
         Primitive.MLton.halt (Status.toRep status)

      fun exit (status: Status.t): 'a =
         if !exiting
            then raise Fail "MLton.Exit.exit"
         else
            let
               val _ = exiting := true
               val i = Status.toInt status
            in
               if 0 <= i andalso i < 256
                  then (let open Cleaner in clean atExit end
                        ; halt status
                        ; raise Fail "MLton.Exit.exit")
               else raise Fail (concat ["MLton.Exit.exit(", Int.toString i, "): ",
                                        "exit must have 0 <= status < 256"])
            end

      local
         val message = PrimitiveFFI.Stdio.print
         fun 'a wrapSuffix (suffix: unit -> unit) () : 'a =
            (suffix ()
             ; message "Top-level suffix returned.\n"
             ; exit Status.failure)
            handle _ => (message "Top-level suffix raised exception.\n"
                         ; halt Status.failure
                         ; raise Fail "MLton.Exit.wrapSuffix")

         fun suffixArchiveOrLibrary () =
            let
               (* Return to 'lib_open'. *)
               val () = Primitive.MLton.Thread.returnToC ()
               (* Enter from 'lib_close'. *)
               val _ = exiting := true
               val () = let open Cleaner in clean atExit end
               (* Return to 'lib_close'. *)
               val () = Primitive.MLton.Thread.returnToC ()
            in
               ()
            end
         fun suffixExecutable () = exit Status.success
         val defaultSuffix =
            let open Primitive.MLton.Platform.Format
            in
               case host of
                  Archive => suffixArchiveOrLibrary
                | Executable => suffixExecutable
                | LibArchive => suffixArchiveOrLibrary
                | Library => suffixArchiveOrLibrary
            end
      in
         val getTopLevelSuffix = Primitive.TopLevel.getSuffix
         val setTopLevelSuffix = Primitive.TopLevel.setSuffix o wrapSuffix
         fun 'a defaultTopLevelSuffix ((): unit): 'a =
            wrapSuffix defaultSuffix ()
         fun 'a topLevelSuffix ((): unit) : 'a =
            (getTopLevelSuffix () ()
             ; raise Fail "MLton.Exit.topLevelSuffix")
      end

   end
