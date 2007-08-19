(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLton: MLTON =
struct

val isMLton = true

(* The ref stuff is so that the (de)serializer always deals with pointers
 * to heap objects.
 *)
(*       val serialize = fn x => serialize (ref x)
 *       val deserialize = fn x => !(deserialize x)
 *)

val share = Primitive.MLton.share

structure GC = MLtonGC

fun shareAll () =
   (GC.setHashConsDuringGC true
    ; GC.collect ())

fun size x =
   let
      val refOverhead =
         Int.div (HeaderWord.wordSize + ObjptrWord.wordSize, 8)
   in 
      C_Size.toInt (Primitive.MLton.size (ref x)) - refOverhead
   end

(* fun cleanAtExit () = let open Cleaner in clean atExit end *)

val debug = Primitive.Controls.debug
val eq = Primitive.MLton.eq
(* val errno = Primitive.errno *)
val safe = Primitive.Controls.safe

structure Array = Array
structure BinIO = MLtonIO (BinIO)
(*structure CallStack = MLtonCallStack*)
structure CharArray = struct
   open CharArray
   type t = array
end
structure CharVector = struct
   open CharVector
   type t = vector
end
structure Cont = MLtonCont
structure Exn = MLtonExn
structure Finalizable = MLtonFinalizable
structure IntInf =
   struct
      open IntInf
      type t = int
   end
structure Itimer = MLtonItimer
structure LargeReal =
   struct
      open LargeReal
      type t = real
   end
structure LargeWord =
   struct
      open LargeWord
      type t = word
   end
structure Platform = MLtonPlatform
structure Pointer = MLtonPointer
structure ProcEnv = MLtonProcEnv
structure Process = MLtonProcess
(* structure Ptrace = MLtonPtrace *)
structure Profile = MLtonProfile
structure Random = MLtonRandom
structure Real = 
   struct
      open Real
      type t = real
   end
structure Real32 = 
   struct
      open Real32
      type t = real
      open Primitive.PackReal32
   end
structure Real64 = 
   struct
      open Real64 
      type t = real
      open Primitive.PackReal64
   end
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
structure Word =
   struct
      open Word
      type t = word
   end
structure Word8 =
   struct
      open Word8
      type t = word
   end
structure Word16 =
   struct
      open Word16
      type t = word
   end
structure Word32 =
   struct
      open Word32
      type t = word
   end
structure Word64 =
   struct
      open Word64
      type t = word
   end
structure Word8Array = struct
   open Word8Array
   type t = array
end
structure Word8Vector = struct
   open Word8Vector
   type t = vector
end

val _ = 
   (Primitive.TopLevel.setHandler MLtonExn.defaultTopLevelHandler
    ; Primitive.TopLevel.setSuffix 
      (fn () => MLtonProcess.exit MLtonProcess.Status.success))
end

(* Patch OS.FileSys.tmpName to use mkstemp. *)
structure OS =
   struct
      open OS

      structure FileSys =
         struct
            open FileSys

            fun tmpName () =
               let
                  val (f, out) =
                      MLton.TextIO.mkstemp (MLton.TextIO.tempPrefix "file")
                  val _ = TextIO.closeOut out
               in
                  f
               end
         end
   end
