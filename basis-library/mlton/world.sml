(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonWorld: MLTON_WORLD =
   struct
      structure Prim = Primitive.World
         
      datatype status = Clone | Original

      (* Need to worry about:
       *   - open file descriptors
       *   - redetermine buffer status when restart
       *)
      fun save' (file: string): status =
         let
            val fd =
               let
                  open Posix.FileSys
                  val flags =
                     O.flags [O.trunc,
                              PosixPrimitive.FileSys.O.binary]
                  val mode =
                     let
                        open S
                     in
                        flags [irusr, iwusr, irgrp, iwgrp, iroth, iwoth]
                     end
               in
                  createf (file, O_WRONLY, flags, mode)
                  handle e =>
                     raise Fail (concat ["MLton.World.save unable to open ",
                                         file, " due to ",
                                         General.exnMessage e])
               end
            val _ = Prim.save (Posix.FileSys.fdToWord fd)
         in
            if Prim.isOriginal ()
               then (Posix.IO.close fd; Original)
            else (Prim.makeOriginal ()
                  ; Cleaner.clean Cleaner.atLoadWorld
                  ; Clone)
         end

      fun saveThread (file: string, t: MLtonThread.Runnable.t): unit =
         case save' file of
            Clone => MLtonThread.switch (fn _ => t)
          | Original => ()
         
      fun save (file: string): status =
         if MLtonThread.amInSignalHandler ()
            then raise Fail "cannot call MLton.World.save within signal handler"
         else save' file

      fun load (file: string): 'a =
         if let open OS_FileSys
            in access (file, [A_READ])
            end
            then 
               let val c = CommandLine.name ()
               in Posix.Process.exec (c, [c, "@MLton", "load-world", file, "--"])
               end
         else raise Fail (concat ["World.load can not read ", file])
   end
