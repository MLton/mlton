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

val eq = Primitive.eq
val errno = Primitive.errno
val safe = Primitive.safe

structure Array = Array
structure BinIO =
   struct
      local
	 structure S = MLtonIO (BinIO)
      in
	 open S
      end
      local
	 open BinIO
      in
	 val stdErr = stdErr
	 val stdIn = stdIn
	 val stdOut = stdOut
      end
   end
structure Cont = MLtonCont
structure Exn = MLtonExn
structure FFI = MLtonFFI
structure GC = MLtonGC
structure IntInf = IntInf
structure Itimer = MLtonItimer
structure Platform = MLtonPlatform
structure ProcEnv = MLtonProcEnv
structure Process = MLtonProcess
structure Ptrace = MLtonPtrace
structure Profile = MLtonProfile
structure Random = MLtonRandom
structure Rlimit = MLtonRlimit
structure Rusage = MLtonRusage
structure Signal = MLtonSignal
structure Socket = MLtonSocket
structure Syslog = MLtonSyslog
structure TextIO = MLtonIO (TextIO)
structure Thread = MLtonThread
structure Vector = Vector
structure Weak = MLtonWeak
structure World = MLtonWorld
structure Word = Primitive.Word32
structure Word8 = Primitive.Word8

end
