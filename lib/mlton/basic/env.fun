(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Env (S: ENV_STRUCTS): ENV =
struct

open S

datatype 'a t = T of (Domain.t * 'a) List.t

fun size (T l) = List.length l

fun domain (T drs) = List.revMap (drs, #1)

val fromList = T
fun toList (T l) = l

fun empty () = T []

fun single (d, r) = T [(d, r)]

fun isEmpty (T l) = List.isEmpty l

fun singleton dr = T [dr]

fun new (ds, f) = T (List.map (ds, fn d => (d, f d)))

fun map (T drs, f) = T (List.map (drs, fn (d, r) => (d, f r)))
fun mapi (T drs, f) = T (List.map (drs, fn (d, r) => (d, f (d, r))))

fun fold (T drs, b, f) = List.fold (drs, b, fn ((_, r), b) => f (r, b))
fun foldi (T drs, b, f) = List.fold (drs, b, fn ((d, r), b) => f (d, r, b))

fun equal d (d', _) = Domain.equals (d, d')

fun remove (T drs, d) = T (List.remove (drs, equal d))

fun extend (T drs, d, r) =
   T (List.cons ((d, r), List.remove (drs, equal d)))

fun env + (T l) = List.fold (l, env, fn ((d, r), env) => extend (env, d, r))

fun plus es = List.fold (es, empty (), fn (e, accum) => accum + e)

fun peek (T l, d) =
   case List.peek (l, equal d) of
      NONE => NONE
    | SOME (_, r) => SOME r
fun lookup (env, d) = case peek (env, d) of
   SOME r => r
 | NONE => (Layout.output (Domain.layout d, Out.error) ;
            Out.newline Out.error ;
            Error.bug "Env.lookup")

fun restrict (env, ds) = new (ds, fn d => lookup (env, d))

fun multiExtend (env, ds, rs) =
   case (ds, rs) of
      ([], []) => env
    | (d :: ds, r :: rs) => multiExtend (extend (env, d, r), ds, rs)
    | _ => Error.bug "Env.multiExtend"

fun foreach (e, f) = List.foreach (toList e, f o #2)
fun foreachi (e, f) = List.foreach (toList e, f)

fun forall (e, f) = List.forall (toList e, f o #2)
fun foralli (e, f) = List.forall (toList e, f)

fun equals rangeEqual (e1, e2) =
   size e1 = size e2
   andalso foralli (e1, fn (d, r) =>
                   case peek (e2, d) of
                      NONE => false
                    | SOME r' => rangeEqual (r, r'))

fun layout layoutR (T ps) =
   let open Layout
   in seq [str "[",
          align (List.map (ps, fn (d, r) =>
                         seq [Domain.layout d, str " -> ", layoutR r])),
          str"]"]
   end

fun maybeLayout (name, layoutR) env =
   if isEmpty env then Layout.empty
   else let open Layout
        in seq [str name, str " = ", layout layoutR env]
        end

end

functor PolyEnv (S: ENV_STRUCTS): ENV = Env (S)
