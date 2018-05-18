(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ProfileExp (S: PROFILE_EXP_STRUCTS): PROFILE_EXP =
struct

open S

datatype t =
   Enter of SourceInfo.t
 | Leave of SourceInfo.t

val toString =
   fn Enter si => concat ["Enter ", SourceInfo.toString si]
    | Leave si => concat ["Leave " , SourceInfo.toString si]

val layout = Layout.str o toString

val equals =
   fn (Enter si, Enter si') => SourceInfo.equals (si, si')
    | (Leave si, Leave si') => SourceInfo.equals (si, si')
    | _ => false

local
   val newHash = Random.word
   val enter = newHash ()
   val leave = newHash ()
in
   val hash =
      fn Enter si => Word.xorb (enter, SourceInfo.hash si)
       | Leave si => Word.xorb (leave, SourceInfo.hash si)
end

end
