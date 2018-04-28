(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MakeMonoEnv(S: sig
                           structure Env: ENV
                           structure Range: T
                        end): MONO_ENV =
struct

open S
open Env

type t = Range.t t

val empty: t = empty()

val equals = equals Range.equals

val layout = layout Range.layout

end

(* THIS ISN'T HERE BECAUSE (no surprise) there is an NJ BUG

functor MonoEnv(S: MONO_ENV_STRUCTS): MONO_ENV =
   MakeMonoEnv(structure Env = Env(S)
               structure Range = S.Range)
*)

functor MonoEnv(S: MONO_ENV_STRUCTS): MONO_ENV =
BasicEnvToEnv
(open S

 datatype t = T of (Domain.t * Range.t) List.t

 val fromList = T

 fun toList(T l) = l

 fun equalTo d (d', _) = Domain.equals(d, d')

 fun extend(T drs, d, r) =
    T(List.cons((d, r), List.remove(drs, equalTo d)))

 fun peek(T l, d) =
    case List.peek(l, equalTo d) of
       NONE => NONE
     | SOME (_, r) => SOME r
          )
