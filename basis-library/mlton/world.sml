(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonWorld: MLTON_WORLD =
   struct
      structure Prim = Primitive.MLton.World
      structure Error = PosixError
      structure SysCall = Error.SysCall

      val gcState = Primitive.MLton.GCState.gcState

      datatype status = Clone | Original

      (* Need to worry about:
       *   - open file descriptors
       *   - redetermine buffer status when restart
       *)
      fun save' (file: string): status =
         let
            val () = 
               SysCall.simple' 
               ({errVal = false}, 
                fn () => (Prim.save (NullString.nullTerm file)
                          ; Prim.getSaveStatus (gcState)))
         in
            if Prim.getAmOriginal gcState
               then Original
            else (Prim.setAmOriginal (gcState, true)
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
