(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure MLton: MLTON =
struct

structure Prim = Primitive.MLton

datatype hostType = datatype Prim.hostType

val hostType = Prim.hostType
   
val isMLton = true
   
(* The ref stuff is so that the (de)serializer always deals with pointers
 * to heap objects.
 *)
(*       val serialize = fn x => serialize (ref x)
 *       val deserialize = fn x => !(deserialize x)
 *)

fun size x =
   let val refOverhead = 8 (* header + indirect *)
   in Primitive.MLton.size (ref x) - refOverhead
   end

fun cleanAtExit () = let open Cleaner in clean atExit end

val debug = Primitive.debug
val eq = Primitive.eq
val errno = Primitive.errno
val safe = Primitive.safe

structure Array = Array
structure Cont = Cont
structure Exn = Exn
structure GC = GC
structure IntInf = IntInf
structure Itimer = Itimer
structure ProcEnv = ProcEnv
structure Process = Process
structure Ptrace = Ptrace
structure Profile = Profile (structure Cleaner = Cleaner
			     structure Profile = Prim.Profile)
structure Random = Random
structure Rlimit = Rlimit
structure Rusage = Rusage
structure Signal = Signal
structure Socket = Socket
structure Syslog = Syslog
structure TextIO = TextIO
structure Thread = Thread
structure Vector = Vector
structure World = World
structure Word = Primitive.Word32
structure Word8 = Primitive.Word8

end
