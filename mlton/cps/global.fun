(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Global (S: GLOBAL_STRUCTS): GLOBAL = 
struct

open S
open PrimExp

fun equalss (cs, cs') = Vector.equals (cs, cs', Var.equals)

val primExpEquals =
   fn (Const c, Const c') => Const.equals (c, c')
    | (ConApp {con = c, args}, ConApp {con = c', args = args'}) =>
	 Con.equals (c, c') andalso equalss (args, args')
    | (PrimApp {prim = p, targs = t, ...},
       PrimApp {prim = p', targs = t', ...}) =>
	 let
	    datatype z = datatype Prim.Name.t
	    val n = Prim.name p
	    val n' = Prim.name p'
	 in
	    case (n, n') of
	       (Array_array0, Array_array0) => Vector.equals (t, t', Type.equals)
	     | _ => false
	 end
    | (Tuple xs, Tuple xs') => equalss (xs, xs')
    | _ => false

fun make () =
   let
      type bind = {var: Var.t, ty: Type.t, exp: PrimExp.t}
      val binds: bind list ref = ref []
      fun all () = List.rev (!binds) before binds := []
      val set: (word * bind) HashSet.t = HashSet.new {hash = #1}
      fun new (ty: Type.t, exp: PrimExp.t): Var.t =
	 let
	    val hash = hash exp
	 in
	    #var
	    (#2
	     (HashSet.lookupOrInsert
	      (set, hash,
	       fn (_, {exp = exp', ...}) => primExpEquals (exp, exp'),
	       fn () => 
	       let
		  val x = Var.newString "global"
		  val bind = {var = x, ty = ty, exp = exp}
	       in List.push (binds, bind)
		  ; (hash, bind)
	       end)))
	 end
   in {new = new, all = all}
   end
	    
end


