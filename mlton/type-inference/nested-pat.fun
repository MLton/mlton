(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor NestedPat (S: NESTED_PAT_STRUCTS): NESTED_PAT = 
struct

open S

datatype t = T of {pat: node, ty: Type.t}
and node =
   Wild
  | Var of Var.t
  | Const of Const.t
  | Con of {con: Con.t,
	    targs: Type.t vector,
	    arg: t option}
  | Tuple of t vector
  | Layered of Var.t * t

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

local
   structure Pat = Ast.Pat
in
   fun toAst p =
      case node p of
	 Wild => Pat.wild
       | Var x => Pat.var (Var.toAst x)
       | Const c => Const.toAstPat c
       | Con {con, arg, ...} =>
	    let val con = Con.toAst con
	    in case arg of
	       NONE => Pat.con con
	     | SOME p => Pat.app (con, toAst p)
	    end
       | Tuple ps => Pat.tuple (Vector.map (ps, toAst))
       | Layered (x, p) => Pat.layered {fixop = Ast.Fixop.None,
					var = Var.toAst x,
					constraint = NONE,
					pat = toAst p}
end

val layout = Ast.Pat.layout o toAst

fun new (p, t) =
   case p of
      Tuple ps =>
	 if 1 = Vector.length ps
	    then Vector.sub (ps, 0)
	 else T {pat = p, ty = t}
    | _ => T {pat = p, ty = t}

fun wild t = new (Wild, t)
   
fun isRefutable p =
   case node p of
      Wild => false
    | Var _ => false
    | Const _ => true
    | Con _ => true
    | Tuple ps => Vector.exists (ps, isRefutable)
    | Layered (_, p) => isRefutable p

fun isVar p =
   case node p of
      Var _ => true
    | _ => false

val unit =
   T {pat = Tuple (Vector.new0 ()),
      ty = Type.tuple (Vector.new0 ())}
   
fun varsAndTypes (p: t): (Var.t * Type.t) list =
   let
      fun loop (p: t, accum: (Var.t * Type.t) list) =
	 case node p of
	    Wild => accum
	  | Const _ => accum
	  | Var x => (x, ty p) :: accum
	  | Tuple ps => Vector.fold (ps, accum, loop)
	  | Con {arg, ...} => (case arg of
				NONE => accum
			      | SOME p => loop (p, accum))
	  | Layered (x, p) => loop (p, (x, ty p) :: accum)
   in loop (p, [])
   end

fun vars (p: t): Var.t list =
   List.revMap (varsAndTypes p, #1)

end
