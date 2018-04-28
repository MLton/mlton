(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor BasicEnvToEnv(S: BASIC_MONO_ENV): MONO_ENV =
struct

open S

val isEmpty = List.isEmpty o toList

fun layout e =
   let open Layout
   in seq[str "[",
          align(List.map(toList e, fn (d, r) =>
                         seq[Domain.layout d, str " -> ", Range.layout r])),
          str"]"]
   end

val size = List.length o toList

val empty = fromList []

fun domain e = List.revMap (toList e, #1)

fun single(d, r) = extend(empty, d, r)

fun new (ds, f) = fromList (List.map (ds, fn d => (d, f d)))

fun map (e, f) = fromList (List.map (toList e, fn (d, r) => (d, f r)))

fun mapi (e, f) = fromList (List.map(toList e, fn (d, r) => (d, f(d, r))))

fun env + env' =
   List.fold (toList env', env, fn ((d, r), env) => extend (env, d, r))

fun plus es = List.fold(es, empty, fn (e, accum) => accum + e)

val plus = Trace.trace("BasicEnvToEnv.plus", List.layout layout, layout) plus

fun remove(env, d) =
   fromList(List.remove(toList env, fn (d', _) => Domain.equals(d, d')))

fun lookup(env, d) = case peek(env, d) of
   SOME r => r
 | NONE => (Layout.output(Domain.layout d, Out.error) ;
            Out.newline Out.error ;
            Error.bug "BasicEnvToEnv.lookup")

fun restrict(env, ds) = new(ds, fn d => lookup(env, d))

fun multiExtend(env, ds, rs) =
   case (ds, rs) of
      ([], []) => env
    | (d :: ds, r :: rs) => multiExtend(extend(env, d, r), ds, rs)
    | _ => Error.bug "BasicEnvToEnv.multiExtend"

fun fold(e, b, f) = List.fold(toList e, b, fn ((_, r), b) => f(r, b))
fun foldi(e, b, f) = List.fold(toList e, b, fn ((d, r), b) => f(d, r, b))

fun foreach(e, f) = List.foreach(toList e, f o #2)
fun foreachi(e, f) = List.foreach(toList e, f)

fun foralli(e, f) = List.forall(toList e, f)

val equals =
   fn (e1, e2) =>
   size e1 = size e2
   andalso foralli(e1, fn (d, r) =>
                   case peek(e2, d) of
                      NONE => false
                    | SOME r' => Range.equals(r, r'))

end
