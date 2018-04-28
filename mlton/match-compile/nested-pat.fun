(* Copyright (C) 2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
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
  | Record of t SortedRecord.t
  | Var of Var.t
  | Vector of t vector
  | Wild

local
   fun make f (T r) = f r
in
   val node = make #pat
   val ty = make #ty
end

fun tuple ps =
   T {pat = Record (SortedRecord.tuple ps),
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
       | Record rps =>
            SortedRecord.layout
            {extra = "",
             layoutElt = layoutT,
             layoutTuple = fn ps => tuple (Vector.toListMap (ps, layoutT)),
             record = rps,
             separator = " = "}
       | Var x => Var.layout x
       | Vector ps => vector (Vector.map (ps, layoutT))
       | Wild => str "_"
   end
and layoutF p = layout (p, false)
and layoutT p = layout (p, true)

val layout = layoutT

fun make (p, t) =
   T {pat = p, ty = t}

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
       | Record rps =>
            let
               val (fs, ps) = SortedRecord.unzip rps
               val record = fn ps =>
                  Record (SortedRecord.zip (fs, ps))
            in
               flattens (ps, make o record)
            end
       | Var _ => Vector.new1 p
       | Vector ps => flattens (ps, make o Vector)
       | Wild => Vector.new1 p
   end
and flattens (ps, make) =
   let
      val fpss =
         Vector.foldr
         (Vector.map (ps, flatten), [[]], fn (fps, fpss) =>
          List.concat (Vector.toListMap (fps, fn fp =>
                                         List.map (fpss, fn fps => fp :: fps))))
   in
      Vector.fromListMap (fpss, fn fps => make (Vector.fromList fps))
   end

val flatten =
   Trace.trace ("NestedPat.flatten", layout, Vector.layout layout)
   flatten

fun isRefutable p =
   case node p of
      Con _ => true
    | Const _ => true
    | Layered (_, p) => isRefutable p
    | Or ps => Vector.exists (ps, isRefutable)
    | Record rps => SortedRecord.exists (rps, isRefutable)
    | Var _ => false
    | Vector _ => true
    | Wild => false

fun isVarOrWild p =
   case node p of
      Var _ => true
    | Wild => true
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
                | Record rps => Record (SortedRecord.map (rps, loop))
                | Var x =>
                     if Var.equals (x, old)
                        then Var new
                     else Wild
                | Vector ps => Vector (Vector.map (ps, loop))
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
                | Record rps => Record (SortedRecord.map (rps, loop))
                | Var _ => pat
                | Vector ps => Vector (Vector.map (ps, loop))
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
            Con {arg, ...} => (case arg of
                                  NONE => accum
                                | SOME p => loop (p, accum))
          | Const _ => accum
          | Layered (x, p) => loop (p, (x, ty p) :: accum)
          | Or ps => loop (Vector.first ps, accum)
          | Record rps => SortedRecord.fold (rps, accum, loop)
          | Var x => (x, ty p) :: accum
          | Vector ps => Vector.fold (ps, accum, loop)
          | Wild => accum
   in loop (p, [])
   end

end
