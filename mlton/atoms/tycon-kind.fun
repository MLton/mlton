(* Copyright (C) 2003-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor TyconKind (S: TYCON_KIND_STRUCTS): TYCON_KIND = 
struct

open S

datatype t =
   Arity of int
 | Nary

val layout =
   fn Arity n => Int.layout n
    | Nary => Layout.str "n-ary"

val equals =
   fn (Arity n, Arity n') => n = n'
    | (Nary, Nary) => true
    | _ => false

val equals = Trace.trace2 ("TyconKind.equals", layout, layout, Bool.layout) equals

end
