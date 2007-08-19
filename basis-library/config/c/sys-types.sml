(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature C_SYSTYPE =
   sig
      type t
      val castFromSysWord: SysWord.word -> t
      val castToSysWord: t -> SysWord.t
   end
signature C_FLAGTYPE =
   sig
      include C_SYSTYPE
      val andb: t * t -> t
      val notb: t -> t
      val orb: t * t -> t
   end

(* from <dirent.h> *)
structure C_DirP : C_SYSTYPE = C_DirP

(* from <poll.h> *)
structure C_NFds : sig
                      include C_SYSTYPE 
                      val fromInt: Int.int -> t
                   end = C_NFds

(* from <resource.h> *)
structure C_RLim : C_SYSTYPE = C_RLim

(* from <sys/types.h> *)
structure C_Clock : sig
                       include C_SYSTYPE
                       val castFromFixedInt: FixedInt.int -> t
                       val toLargeInt: t -> LargeInt.int
                    end = C_Clock
structure C_Dev : C_SYSTYPE = C_Dev
structure C_GId : C_SYSTYPE = C_GId
structure C_INo : C_SYSTYPE = C_INo
structure C_Mode : C_FLAGTYPE = C_Mode
structure C_NLink : sig
                       include C_SYSTYPE
                       val toInt: t -> int
                    end = C_NLink
structure C_Off = C_Off
structure C_PId : sig 
                     include C_SYSTYPE 
                     val castFromFixedInt: FixedInt.int -> t
                     val ~ : t -> t
                  end = C_PId
structure C_SSize : sig
                       include C_SYSTYPE
                       val castFromFixedInt: FixedInt.int -> t
                       val toInt: t -> Int.int
                    end = C_SSize
structure C_SUSeconds : sig
                           include C_SYSTYPE
                           val fromLargeInt: LargeInt.int -> t
                           val toLargeInt: t -> LargeInt.int
                        end = C_SUSeconds
structure C_Time : sig
                      include C_SYSTYPE
                      val fromInt: Int.int -> t
                      val toInt: t -> Int.int
                      val fromLargeInt: LargeInt.int -> t
                      val toLargeInt: t -> LargeInt.int
                   end = C_Time
structure C_UId : C_SYSTYPE = C_UId

(* from <sys/socket.h> *)
structure C_Socklen = C_Socklen

(* from <termios.h> *)
structure C_CC : C_SYSTYPE = C_CC
structure C_Speed : sig
                       include C_SYSTYPE
                       val compare: t * t -> order
                    end = C_Speed
structure C_TCFlag : C_FLAGTYPE = C_TCFlag
