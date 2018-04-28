(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature NET =
   sig
      type port = int

      (* canPing m returns true if a ping to machine m succeeds. *)
      val canPing: string -> bool
      (* connect{host, port} connects to host on port and
       * returns the streams to read to and write from the socket.
       *)
      val connect: {host: string, port: port} -> In.t * Out.t
      val ethernetIsUp: unit -> bool
      val fullHostname: string
      val hostname: string
      val repeat: {limit: Time.t, tries: int} option ref
      (* server(p, f) starts a server listening on port p.  Whenever a connection
       * is received, f is run with streams corresponding to the socket.
       *)
      val server: port * (In.t * Out.t -> unit) -> unit
   end
