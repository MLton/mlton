(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor TypeStr (S: TYPE_STR_STRUCTS): TYPE_STR = 
struct

open S

structure Cons =
   struct
      datatype t = T of {con: Con.t,
			 name: Name.t,
			 scheme: Scheme.t} vector

      val empty = T (Vector.new0 ())

      fun layout (T v) =
	 Vector.layout (fn {con, name, scheme} =>
			Layout.tuple [Name.layout name,
				      Con.layout con,
				      Layout.str ": ",
				      Scheme.layout scheme])
	 v
   end

datatype node =
   Datatype of {cons: Cons.t,
		tycon: Tycon.t}
 | Scheme of Scheme.t
 | Tycon of Tycon.t

datatype t = T of {kind: Kind.t,
		   node: node}

local
   fun make f (T r) = f r
in
   val kind = make #kind
   val node = make #node
end

fun layout t =
   let
      open Layout
   in
      case node t of
	 Datatype {tycon, cons} =>
	    seq [str "Datatype ",
		 record [("tycon", Tycon.layout tycon),
			 ("cons", Cons.layout cons)]]
       | Scheme s => Scheme.layout s
       | Tycon t => seq [str "Tycon ", Tycon.layout t]
   end

fun bogus (k: Kind.t): t =
   T {kind = k,
      node = Scheme (Scheme.bogus ())}

fun abs t =
   case node t of
      Datatype {tycon, ...} => T {kind = kind t,
				  node = Tycon tycon}
    | _ => t

fun apply (t: t, tys: Type.t vector): Type.t =
   case node t of
      Datatype {tycon, ...} => Type.con (tycon, tys)
    | Scheme s => Scheme.apply (s, tys)
    | Tycon t => Type.con (t, tys)

fun cons t =
   case node t of
      Datatype {cons, ...} => cons
    | _ => Cons.empty

fun data (tycon, kind, cons) =
   T {kind = kind,
      node = Datatype {tycon = tycon, cons = cons}}
   
fun def (s: Scheme.t, k: Kind.t) =
   let
      val (tyvars, ty) = Scheme.dest s
   in
      T {kind = k,
	 node = (case Type.deEta (ty, tyvars) of
		    NONE => Scheme s
		  | SOME c => Tycon c)}
   end

fun isTycon s =
   case node s of
      Datatype _ => false
    | Scheme _ => false
    | Tycon _ => true

fun toTyconOpt s =
   case node s of
      Datatype {tycon, ...} => SOME tycon
    | Scheme _ => NONE
    | Tycon c => SOME c

fun tycon (c, kind) = T {kind = kind,
			 node = Tycon c}

end
