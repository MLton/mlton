(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor GenericScheme (S: GENERIC_SCHEME_STRUCTS): GENERIC_SCHEME =
struct

open S

type ty = Type.t
type tyvar = Tyvar.t
   
datatype t = T of {tyvars: Tyvar.t vector,
		   ty: Type.t}

local
   fun make f (T r) = f r
in
   val ty = make #ty
   val tyvars = make #tyvars
end

fun fromType t = T {tyvars = Vector.new0 (), ty = t}

val equals = fn _ => Error.unimplemented "GenericScheme.equals"

fun layout (T {tyvars, ty}) =
   let open Layout
      val ty = Type.layout ty
   in
      if 0 = Vector.length tyvars
	 then ty
      else
	 align [seq [str "Forall ",
		     Vector.layout Tyvar.layout tyvars,
		     str "."],
		ty]
   end

fun apply (T {tyvars, ty}, args) =
   if Vector.isEmpty tyvars andalso Vector.isEmpty args
      then ty (* Must special case this, since don't want to substitute
	       * in monotypes.
	       *)
   else Type.substitute (ty, Vector.zip (tyvars, args))

fun makeGen (numTyvars, equality, makeType): t =
   let
      val tyvars =
	 Vector.tabulate (numTyvars, fn _ =>
			  Tyvar.newNoname {equality = equality})
      val tys = Vector.map (tyvars, Type.var)
   in T {tyvars = tyvars,
	 ty = makeType (fn i => Vector.sub (tys, i))}
   end

val make0 = fromType

fun make1 makeType = makeGen (1, false, fn sub => makeType (sub 0))
fun make2 makeType = makeGen (2, false, fn sub => makeType (sub 0, sub 1))
fun makeEqual1 makeType = makeGen (1, true, fn sub => makeType (sub 0))

end
