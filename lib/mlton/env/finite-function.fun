(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                                Env                                *)
(*-------------------------------------------------------------------*)

functor Env(D: T): ENV =
struct

structure D = D
structure L = List

datatype 'a t = T of (D.t * 'a) L.t

fun empty() = T(L.empty())

fun isEmpty(T l) = L.isEmpty l

fun equal d (d', _) = D.equals(d, d')

fun singleton dr = T(L.single dr)

fun add(T l, d, r) = T((d, r) :: (L.maybeRemoveFirst(l, equal d)))

fun peek(T l, d) =
   case L.keepFirst(l, equal d) of
      NONE => NONE
    | SOME (_, r) => SOME r

fun lookup ed = case peek ed of
   SOME r => r
 | NONE => Error.error "Env.lookup"

fun dom(T l) = L.map(l, #1)

fun range(T l) = L.map(l, #2)

fun remove(T ps, d) = T(L.removeFirst(ps, equal d))

fun foldl(T l, b, f) = L.foldl(l, b, fn (b, (d, r)) => f(b, d, r))

fun e + e' = foldl(e', e, add)

fun foreach(T ps, f) = L.foreach(ps, f)

fun multiExtend(env, [], []) = env
  | multiExtend(env, d :: ds, r :: rs) = multiExtend(extend(env, d, r), ds, rs)
  | multiExtend _ = Error.error "Env.multiExtend"

fun merge(e as T p, e' as T p', f) =
   let val leftAndBoth = L.map(p, fn (d, r) =>
                               case peek(e', d) of
                                  NONE => (d, r)
                                | SOME r' => (d, f(r, r')))
      val right = L.keepAll(p',
                            fn (d, _) =>
                            case peek(e, d) of
                               NONE => true
                             | SOME _ => false)
   in T(leftAndBoth @ right)
   end

fun output(T ps, outputR, out) =
   let val print = Out.outputc out
      fun outputDR((d, r), out) = (D.output(d, out) ;
                                  print "->" ;
                                  outputR(r, out))
   in (print "[" ;
       L.output(ps, ", ", outputDR, out) ;
       print "]")
   end

end
