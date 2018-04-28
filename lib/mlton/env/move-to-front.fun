(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MoveToFrontEnv(S: MONO_ENV_STRUCTS): MONO_ENV =
BasicEnvToEnv
(open S

 datatype t = T of (Domain.t * Range.t) list ref

 val fromList = T o ref

 fun toList(T(ref drs)) = drs

 fun extend(T(ref drs), d, r) =
    T(ref((d, r) ::
          (* poor man's profiling *)
          let fun f() = List.remove(drs, fn (d', _) => Domain.equals(d, d'))
          in (*f() ;*) f()
          end))

 fun peek(T reff, d) =
    let
       fun loop(drs, accum) =
          case drs of
             (d', r) :: drs =>
                if Domain.equals(d, d')
                   then (reff := (d, r) :: List.appendRev(accum, drs)
                         ; SOME r)
                else loop(drs, (d', r) :: accum)
           | [] => NONE
       (* poor man's profiling *)
       fun f() = loop(!reff, [])
    in (*f() ;*) f()
    end)
