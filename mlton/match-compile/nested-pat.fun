(* Copyright (C) 2015 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor NestedPat (S: NESTED_PAT_STRUCTS): NESTED_PAT = 
struct

open S

datatype t = T of {pat: node, ty: Type.t}
and node =
    Con of {arg: t option,
            con: Con.t,
            targs: Type.t vector}
  | Const of {const: Const.t,
              isChar: bool,
              isInt: bool}
  | Layered of Var.t * t
  | Or of t vector
  | Tuple of t vector
  | Var of Var.t
  | Wild

local
   fun make f (T r) = f r
in
   val node = make #pat
   val ty = make #ty
end

fun tuple ps =
   if 1 = Vector.length ps
      then Vector.sub (ps, 0)
   else T {pat = Tuple ps,
           ty = Type.tuple (Vector.map (ps, ty))}

fun layout (p, isDelimited) =
   let
      open Layout
      fun delimit t = if isDelimited then t else paren t
   in
      case node p of
         Con {arg, con, targs} =>
            delimit (Pretty.conApp {arg = Option.map (arg, layoutF),
                                    con = Con.layout con,
                                    targs = Vector.map (targs, Type.layout)})
       | Const {const = c, ...} => Const.layout c
       | Layered (x, p) => delimit (seq [Var.layout x, str " as ", layoutT p])
       | Or ps => paren (mayAlign (separateLeft (Vector.toListMap (ps, layoutT), "| ")))
       | Tuple ps => tuple (Vector.toListMap (ps, layoutT))
       | Var x => Var.layout x
       | Wild => str "_"
   end
and layoutF p = layout (p, false)
and layoutT p = layout (p, true)

val layout = layoutT

fun make (p, t) =
   case p of
      Tuple ps =>
         if 1 = Vector.length ps
            then Vector.sub (ps, 0)
         else T {pat = p, ty = t}
    | _ => T {pat = p, ty = t}

fun flatten p =
   let
      val ty = ty p
      val make = fn p => make (p, ty)
   in
      case node p of
         Con {arg, con, targs} =>
            (case arg of
                NONE => Vector.new1 p
              | SOME arg => Vector.map (flatten arg, fn arg =>
                                        make (Con {arg = SOME arg, con = con, targs = targs})))
       | Const _ => Vector.new1 p
       | Layered (x, p) => Vector.map (flatten p, fn p => make (Layered (x, p)))
       | Or ps => Vector.concatV (Vector.map (ps, flatten))
       | Tuple ps => let
                        val fpss =
                           Vector.foldr
                           (Vector.map (ps, flatten), [[]], fn (fps, fpss) =>
                            List.concat (Vector.toListMap (fps, fn fp =>
                                                           List.map (fpss, fn fps => fp :: fps))))
                     in
                        Vector.fromListMap (fpss, fn fps => make (Tuple (Vector.fromList fps)))
                     end
       | Var _ => Vector.new1 p
       | Wild => Vector.new1 p
   end

val flatten =
   Trace.trace ("NestedPat.flatten", layout, Vector.layout layout)
   flatten

fun isRefutable p =
   case node p of
      Wild => false
    | Var _ => false
    | Const _ => true
    | Con _ => true
    | Or ps => Vector.exists (ps, isRefutable)
    | Tuple ps => Vector.exists (ps, isRefutable)
    | Layered (_, p) => isRefutable p

fun isVar p =
   case node p of
      Var _ => true
    | _ => false

fun removeOthersReplace (p, {new, old}) =
   let
      fun loop (T {pat, ty}) =
         let
            val pat =
               case pat of
                  Con {arg, con, targs} =>
                     Con {arg = Option.map (arg, loop),
                          con = con,
                          targs = targs}
                | Const _ => pat
                | Layered (x, p) =>
                     let
                        val p = loop p
                     in
                        if Var.equals (x, old)
                           then Layered (new, p)
                        else node p
                     end
                | Or ps => Or (Vector.map (ps, loop))
                | Tuple ps => Tuple (Vector.map (ps, loop))
                | Var x =>
                     if Var.equals (x, old)
                        then Var new
                     else Wild
                | Wild => Wild
         in
            T {pat = pat, ty = ty}
         end
   in
      loop p
   end

val removeOthersReplace =
   Trace.trace ("NestedPat.removeOthersReplace", fn (p, _) => layout p, layout)
   removeOthersReplace

local
   val bogus = Var.newNoname ()
in
   fun removeVars (p: t): t =
      removeOthersReplace (p, {new = bogus, old = bogus})
end

fun replaceTypes (p: t, f: Type.t -> Type.t): t =
   let
      fun loop (T {pat, ty}) =
         let
            val pat =
               case pat of
                  Con {arg, con, targs} =>
                     Con {arg = Option.map (arg, loop),
                          con = con,
                          targs = Vector.map (targs, f)}
                | Const _ => pat
                | Layered (x, p) => Layered (x, loop p)
                | Or ps => Or (Vector.map (ps, loop))
                | Tuple ps => Tuple (Vector.map (ps, loop))
                | Var _ => pat
                | Wild => pat
         in
            T {pat = pat, ty = f ty}
         end
   in
      loop p
   end

fun varsAndTypes (p: t): (Var.t * Type.t) list =
   let
      fun loop (p: t, accum: (Var.t * Type.t) list) =
         case node p of
            Wild => accum
          | Const _ => accum
          | Var x => (x, ty p) :: accum
          | Or ps => loop (Vector.sub (ps, 0), accum)
          | Tuple ps => Vector.fold (ps, accum, loop)
          | Con {arg, ...} => (case arg of
                                NONE => accum
                              | SOME p => loop (p, accum))
          | Layered (x, p) => loop (p, (x, ty p) :: accum)
   in loop (p, [])
   end

end
