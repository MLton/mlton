(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Type (S: TYPE_STRUCTS): TYPE =
struct

open S

structure Type =
   struct
      datatype t =
	 Var of var
       | Con of con
       | Record of record
      withtype var = Tyvar.t
      and con = Tycon.t * t vector
      and record = t Record.t
      datatype t' = datatype t

      val var = Var
	 
      fun con (c, ts) =
	 if Tycon.equals (c, Tycon.tuple)
	    then if 1 = Vector.length ts
		    then Vector.sub (ts, 0)
		 else Record (Record.tuple ts)
	 else Con (c, ts)
	    
      val record = Record

      val deconOpt =
	 fn Con (c, ts) => SOME (c, ts)
	  | Record r => (case Record.detupleOpt r of
			    NONE => NONE
			  | SOME ts => SOME (Tycon.tuple, ts))
	  | _ => NONE

      fun toAst t =
	 case t of
	    Var a => Ast.Type.var a
	  | Con (c, ts) => Ast.Type.con (Tycon.toAst c, Vector.map (ts, toAst))
	  | Record r => Ast.Type.record (Record.map (r, toAst))

      val layout = Ast.Type.layout o toAst
   end

structure Ops = TypeOps (structure Tycon = Tycon
			 open Type)
open Ops Type

structure Tyvars = UnorderedSet (Tyvar)
   
fun tyvars t =
   case t of
      Var a => Tyvars.singleton a
    | Con (_, ts) =>
	 Vector.fold (ts, Tyvars.empty, fn (t, ac) =>
		      Tyvars.union (ac, tyvars t))
    | Record r =>
	 Record.fold (r, Tyvars.empty, fn (t, ac) =>
		      Tyvars.union (ac, tyvars t))
	 
val tyvars = Tyvars.toList o tyvars

fun optionToAst z = Option.map (z, toAst)

fun substitute (t, sub) =
   let
      fun loop t =
	 case t of
	    Var a =>
	       Vector.loop
	       (sub,
		fn (a', t) => if Tyvar.equals (a, a') then SOME t else NONE,
		fn () => Error.bug "substitute")
	  | Con (c, ts) => Con (c, Vector.map (ts, loop))
	  | Record r => Record (Record.map (r, loop))
   in loop t
   end

fun hom {ty, var, con} =
   let
      val rec loop =
	 fn Var a => var a
	  | Con (c, ts) => con (c, Vector.map (ts, loop))
	  | Record r =>
	       (case Record.detupleOpt r of
		   SOME ts => con (Tycon.tuple, Vector.map (ts, loop))
		 | NONE => Error.bug "Type.hom")
   in loop ty
   end

end
