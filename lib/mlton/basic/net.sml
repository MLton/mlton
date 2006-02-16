(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Net: NET =
struct

type port = int

val repeat: {limit: Time.t, tries: int} option ref = ref NONE

local
   val numTries: int = 2
in
   (* also need to ignore stderr *)
   fun canPing (machine: string): bool =
      Process.doesSucceed
      (fn () =>
       In.withNull
       (fn ins =>
        Out.withNull
        (fn out =>
         Process.call ("ping", ["-U", "-q", "-c",
                                Int.toString numTries, machine])
         (ins, out))))
end

local
   val z = Posix.ProcEnv.uname ()
   fun lookup s =
      case List.peek (z, fn (s', _) => s = s') of
         NONE => Process.fail (concat [s, " unknown"])
       | SOME (_, s) => s
in
   val fullHostname = lookup "nodename"
end

val hostname = hd (String.tokens (fullHostname, fn c => c = #"."))

fun ethernetIsUp (): bool =
   String.hasSubstring (Process.collect (Process.call ("ifconfig", [])),
                        {substring = "eth0"})

val message = Trace.Immediate.messageStr

structure Socket = MLton.Socket
   
fun connect {host: string, port: port}: In.t * Out.t =
   let
      val _ = message (concat ["connect ", host, ":", Int.toString port])
      fun con () = Socket.connect (host, port)
      val io =
         case !repeat of
            NONE => (SOME (con ()) handle _ => NONE)
          | SOME {limit, tries} =>
               Engine.repeat {thunk = con, tries = tries, limit = limit}
   in case io of
      SOME io => io
    | NONE => Process.fail (concat ["unable to connect to ",
                                  host, ":", Int.toString port])
   end

fun server (p: port, c: In.t * Out.t -> unit): unit =
   let
      val socket =
         Process.try (fn () => Socket.listenAt p,
                     concat ["server unable to bind port ", Int.toString p])
      val _ = message (concat ["server listening on ", Int.toString p])
      fun loop () =
         let
            val (a, port, ins, out) =
               Process.try (fn () => Socket.accept socket, "accept failed")
            val name =
               case Socket.Host.getByAddress a of
                  NONE => Word.toString a
                | SOME {name, ...} => name
            val _ =
               Process.doubleFork
               (fn () => (message (concat ["accept from ",
                                           name, ":", Int.toString port])
                          ; c (ins, out)))
            val _ = In.close ins
            val _ = Out.close out
         in loop ()
         end
   in Process.watch loop
   end
   
end
