(* Copyright (C) 2013,2019,2022 Matthew Fluet.
 * Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonProcess =
   struct
      type pid = Posix.Process.pid

      local
         fun mk (exec, args) =
            case Posix.Process.fork () of
               NONE => exec args
             | SOME pid => pid
      in
         fun spawne {path, args, env} =
            mk (Posix.Process.exece, (path, args, env))
         fun spawnp {file, args} =
            mk (Posix.Process.execp, (file, args))
      end

      fun spawn {path, args} =
         spawne {path = path, args = args,
                 env = Posix.ProcEnv.environ ()}
   end
