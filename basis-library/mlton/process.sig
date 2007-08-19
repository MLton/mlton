(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_PROCESS =
   sig
      type pid

      val spawn: {args: string list, path: string} -> pid
      val spawne: {args: string list, env: string list, path: string} -> pid
      val spawnp: {file: string, args: string list} -> pid

      (* Process handle *)
      type ('stdin, 'stdout, 'stderr) t

      (* is the io 'dir input or output *)
      type input
      type output

      (* to what use can the stdio channel be put *)
      type none  (* it's not connected to a pipe *)
      type chain (* connect one child to another *)
      type any   (* any use is allowed -- dangerous *)

      exception MisuseOfForget   (* you avoided the type safety and broke it *)
      exception DoublyRedirected (* you tried to reuse a Param.child *)

      structure Child:
        sig
          type ('use, 'dir) t

          val binIn: (BinIO.instream, input) t -> BinIO.instream
          val binOut: (BinIO.outstream, output) t -> BinIO.outstream
          (* not necessarily available on all systems; may raise an exception *)
          val fd: (Posix.FileSys.file_desc, 'dir) t -> Posix.FileSys.file_desc
          (* used for situations where 'forget' was needed for arbitrary redir *)
          val remember: (any, 'dir) t -> ('use, 'dir) t
          val textIn: (TextIO.instream, input) t -> TextIO.instream
          val textOut: (TextIO.outstream, output) t -> TextIO.outstream
        end

      structure Param:
        sig
          type ('use, 'dir) t

          (* {child,fd} close their parameter when create is called.
           * therefore they may only be used once!
           *)
          val child: (chain, 'dir) Child.t -> (none, 'dir) t
          (* Not necessarily available on all systems; may raise an exception *)
          val fd: Posix.FileSys.file_desc -> (none, 'dir) t
          val file: string -> (none, 'dir) t
          (* used if you want to return two posibilities; use with care *)
          val forget: ('use, 'dir) t -> (any, 'dir) t
          val null: (none, 'dir) t
          val pipe: ('use, 'dir) t
          val self: (none, 'dir) t
        end

      val create:
         {args: string list, 
          env: string list option, 
          path: string, 
          stderr: ('stderr, output) Param.t,
          stdin: ('stdin, input) Param.t,
          stdout: ('stdout, output) Param.t}
         -> ('stdin, 'stdout, 'stderr) t
      val getStderr: ('stdin, 'stdout, 'stderr) t -> ('stderr, input) Child.t
      val getStdin:  ('stdin, 'stdout, 'stderr) t -> ('stdin, output) Child.t
      val getStdout: ('stdin, 'stdout, 'stderr) t -> ('stdout, input) Child.t
      val kill: ('stdin, 'stdout, 'stderr) t * Posix.Signal.signal -> unit
      val reap: ('stdin, 'stdout, 'stderr) t -> Posix.Process.exit_status
   end
