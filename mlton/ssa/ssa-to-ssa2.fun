(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor SsaToSsa2 (S: SSA_TO_SSA2_STRUCTS): SSA_TO_SSA2 = 
struct

open S

structure S = Ssa
structure S2 = Ssa2

fun convert (S.Program.T {datatypes, functions, globals, main}) =
   let
      val {destroy, hom = convertType: S.Type.t -> S2.Type.t, ...} =
	 S.Type.makeMonoHom {con = fn (_, c, ts) => S2.Type.con (c, ts)}
      fun convertTypes ts = Vector.map (ts, convertType)
      val datatypes =
	 Vector.map
	 (datatypes, fn S.Datatype.T {cons, tycon} =>
	  S2.Datatype.T {cons = Vector.map (cons, fn {args, con} =>
					    {args = convertTypes args,
					     con = con}),
			 tycon = tycon})
      fun convertPrim p = S.Prim.map (p, convertType)
      fun convertExp (e: S.Exp.t): S2.Exp.t =
	 case e of
	    S.Exp.ConApp r => S2.Exp.ConApp r
	  | S.Exp.Const c => S2.Exp.Const c
	  | S.Exp.PrimApp {args, prim, targs} =>
	       S2.Exp.PrimApp {args = args,
			       prim = convertPrim prim,
			       targs = convertTypes targs}
	  | S.Exp.Profile e => S2.Exp.Profile e
	  | S.Exp.Select r => S2.Exp.Select r
	  | S.Exp.Tuple v => S2.Exp.Tuple v
	  | S.Exp.Var x => S2.Exp.Var x
      fun convertStatement (S.Statement.T {exp, ty, var}) =
	 S2.Statement.T {exp = convertExp exp,
			 ty = convertType ty,
			 var = var}
      fun convertHandler (h: S.Handler.t): S2.Handler.t =
	 case h of
	    S.Handler.Caller => S2.Handler.Caller
	  | S.Handler.Dead => S2.Handler.Dead
	  | S.Handler.Handle l => S2.Handler.Handle l
      fun convertReturn (r: S.Return.t): S2.Return.t =
	 case r of
	    S.Return.Dead => S2.Return.Dead
	  | S.Return.NonTail {cont, handler} =>
	       S2.Return.NonTail {cont = cont,
				  handler = convertHandler handler}
	  | S.Return.Tail => S2.Return.Tail
      fun convertCases (cs: S.Cases.t): S2.Cases.t =
	 case cs of
	    S.Cases.Con v => S2.Cases.Con v
	  | S.Cases.Word v => S2.Cases.Word v
      fun convertTransfer (t: S.Transfer.t): S2.Transfer.t =
	 case t of
	    S.Transfer.Arith {args, overflow, prim, success, ty} =>
	       S2.Transfer.Arith {args = args,
				  overflow = overflow,
				  prim = convertPrim prim,
				  success = success,
				  ty = convertType ty}
	  | S.Transfer.Bug => S2.Transfer.Bug
	  | S.Transfer.Call {args, func, return} =>
	       S2.Transfer.Call {args = args,
				 func = func,
				 return = convertReturn return}
	  | S.Transfer.Case {cases, default, test} =>
	       S2.Transfer.Case {cases = convertCases cases,
				 default = default,
				 test = test}
	  | S.Transfer.Goto r => S2.Transfer.Goto r
	  | S.Transfer.Raise v => S2.Transfer.Raise v
	  | S.Transfer.Return v => S2.Transfer.Return v
	  | S.Transfer.Runtime {args, prim, return} =>
	       S2.Transfer.Runtime {args = args,
				    prim = convertPrim prim,
				    return = return}
      fun convertFormals xts = Vector.map (xts, fn (x, t) => (x, convertType t))
      fun convertBlock (S.Block.T {args, label, statements, transfer}) =
	 S2.Block.T {args = convertFormals args,
		     label = label,
		     statements = Vector.map (statements, convertStatement),
		     transfer = convertTransfer transfer}
      val functions =
	 List.map
	 (functions, fn f =>
	  let
	     val {args, blocks, name, raises, returns, start} =
		S.Function.dest f
	     fun rr tvo = Option.map (tvo, convertTypes)
	  in
	     S2.Function.new {args = convertFormals args,
			      blocks = Vector.map (blocks, convertBlock),
			      name = name,
			      raises = rr raises,
			      returns = rr returns,
			      start = start}
	  end)
      val globals = Vector.map (globals, convertStatement)
   in
      S2.Program.T {datatypes = datatypes,
		    functions = functions,
		    globals = globals,
		    main = main}
   end

end
