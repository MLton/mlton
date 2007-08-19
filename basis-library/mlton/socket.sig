(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_SOCKET =
   sig
      structure Address:
         sig
            type t = NetHostDB.in_addr
         end

      structure Ctl:
         sig
            val getERROR:
               ('af, 'sock_type) Socket.sock
               -> (string * Posix.Error.syserror option) option
         end

      structure Host:
         sig
            type t = {name: string}

            val getByAddress: Address.t -> t option
            val getByName: string -> t option
         end

      structure Port:
         sig
            type t = int
         end

      type t

      val accept: t -> Address.t * Port.t * TextIO.instream * TextIO.outstream
      val connect: string * Port.t -> TextIO.instream * TextIO.outstream
      val listen: unit -> Port.t * t
      val listenAt: Port.t -> t
      val shutdownRead: TextIO.instream -> unit
      val shutdownWrite: TextIO.outstream -> unit

      val fdToSock: Posix.FileSys.file_desc -> ('af, 'sock_type) Socket.sock
   end
