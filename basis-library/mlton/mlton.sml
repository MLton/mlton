(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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

val debug = Primitive.debug
val safe = Primitive.safe

structure Cont = Cont
structure GC = GC
structure Itimer = Itimer
structure ProcEnv = ProcEnv
structure Ptrace = Ptrace
structure Random = Random
structure Rlimit = Rlimit
structure Rusage = Rusage
structure Signal = Signal
structure Socket = Socket
structure Syslog = Syslog
structure TextIO =
   struct
      open TextIO

      fun mkstemps {prefix, suffix}: string * outstream =
	 let
	    fun loop () =
	       let
		  val name = concat [prefix, Random.alphaNumString 6, suffix]
		  open Posix.FileSys
	       in
		  (name,
		   newOut (createf (name, O_WRONLY, O.flags [O.excl],
				    let open S
				    in flags [irusr, iwusr]
				    end)))
	       end handle e as PosixError.SysErr (_, SOME s) =>
		  if s = Posix.Error.exist
		     then loop ()
		  else raise e
	 in
	    loop ()
	 end

      fun mkstemp s = mkstemps {prefix = s, suffix = ""}
   end

structure Thread = Thread
structure World = World

structure Word =
   struct
      open Word
      val ~ = Primitive.Word32.~
   end

structure Word8 =
   struct
      open Word8
      val ~ = Primitive.Word8.~
   end

end
