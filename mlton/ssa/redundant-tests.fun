(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor RedundantTests (S: REDUNDANT_TESTS_STRUCTS): REDUNDANT_TESTS = 
struct

open S

type int = Int.t
type word = Word.t

structure Rel =
   struct
      datatype t = EQ | LT | LE | NE

      val equals: t * t -> bool = op =

      val toString =
	 fn EQ => "="
	  | LT => "<"
	  | LE => "<="
	  | NE => "<>"

      val layout = Layout.str o toString
   end

structure Oper =
   struct
      datatype t =
	 Const of Const.t
       | Var of Var.t

      val layout =
	 fn Const c => Const.layout c
	  | Var x => Var.layout x

      val equals =
	 fn (Const c, Const c') => Const.equals (c, c')
	  | (Var x, Var x') => Var.equals (x, x')
	  | _ => false
   end

structure Fact =
   struct
      datatype t = T of {rel: Rel.t,
			 lhs: Oper.t,
			 rhs: Oper.t}

      fun layout (T {rel, lhs, rhs}) =
	 let open Layout
	 in seq [Oper.layout lhs, str " ", Rel.layout rel,
		 str " ", Oper.layout rhs]
	 end

      fun equals (T {rel, lhs = l, rhs = r},
		  T {rel = rel', lhs = l', rhs = r'}) =
	 Rel.equals (rel, rel')
	 andalso Oper.equals (l, l')
	 andalso Oper.equals (r, r')

      fun negate (T {rel, lhs, rhs}): t =
	 let
	    datatype z = datatype Rel.t
	    val rel =
	       case rel of
		  EQ => NE
		| LT => LE
		| LE => LT
		| NE => EQ
	 in
	    T {rel = rel, lhs = rhs, rhs = lhs}
	 end

      datatype result = False | True | Unknown
      fun determine (facts: t list, f: t): result =
	 if List.contains (facts, f, equals)
	    then True
	 else if List.contains (facts, negate f, equals)
		 then False
	      else Unknown
   end

open Exp Transfer

fun simplify (Program.T {globals, datatypes, functions, main}) =
   let
      datatype varInfo =
	 Const of Const.t
       | Fact of Fact.t
       | None
       | Or of Fact.t * Fact.t
      val {get = varInfo: Var.t -> varInfo, set = setVarInfo, ...} =
	 Property.getSetOnce (Var.plist, Property.initConst None)
      datatype z = datatype Fact.result
      datatype z = datatype Rel.t
      fun makeVarInfo {args, prim, targs = _}: varInfo =
	 let
	    fun arg i =
	       let
		  val x = Vector.sub (args, i)
	       in
		  case varInfo x of
		     Const c => Oper.Const c
		   | _ => Oper.Var x
	       end
	    fun z (r, a, b) =
	       Fact (Fact.T {rel = r,
			     lhs = arg a,
			     rhs = arg b})
	    fun doit rel = z (rel, 0, 1)
	    fun doit' rel = z (rel, 1, 0)
	    datatype z = datatype Prim.Name.t
	 in
	    case Prim.name prim of
	       Int_equal _ => doit EQ
	     | Int_ge _ => doit' LE
	     | Int_gt _ => doit' LT
	     | Int_le _ => doit LE
	     | Int_lt _ => doit LT
	     | MLton_eq => doit EQ
	     | Word_equal _ => doit EQ
	     | Word_ge _ => doit' LE
	     | Word_gt _ => doit' LT
	     | Word_le _ => doit LE
	     | Word_lt _ => doit LT
	     | _ => None
	 end
      fun setConst (x, c) = setVarInfo (x, Const c)
      val _ =
	 Vector.foreach
	 (globals, fn Statement.T {var, exp, ...} =>
	  case exp of
	     Exp.Const c => Option.app (var, fn x => setConst (x, c))
	   | _ => ())
      local
	 fun make c =
	    let
	       val x = Var.newNoname ()
	    in
	       (x,
		Statement.T {var = SOME x, 
			     ty = Type.bool,
			     exp = ConApp {con = c, args = Vector.new0 ()}})
	    end
      in
	 val (trueVar, t) = make Con.truee
	 val (falseVar, f) = make Con.falsee
      end
      local
	 fun make s =
	    let
	       val one = Var.newNoname ()
	       val oneS = 
		  Statement.T {exp = Exp.Const (Const.int (IntX.one s)),
			       ty = Type.int s,
			       var = SOME one}
	    in
	       (one,oneS)
	    end
	 datatype z = datatype IntX.IntSize.t
	 val (one8, oneS8) = make I8
	 val (one16, oneS16) = make I16
	 val (one32, oneS32) = make I32
	 val (one64, oneS64) = make I64
      in
	 fun one s =
	   case s of
	     I8 => one8
	   | I16 => one16
	   | I32 => one32
	   | I64 => one64
	 val oneSs = Vector.new4 (oneS8, oneS16, oneS32, oneS64)
      end
      val globals = Vector.concat [Vector.new2 (t, f), oneSs, globals]
      val shrink = shrinkFunction globals
      val numSimplified = ref 0
      fun simplifyFunction f =
	  let
	     val {args, blocks, name, raises, returns, start} = Function.dest f
	     val _ =
		Control.diagnostic
		(fn () => 
		 let open Layout
		 in seq [str "processing ", Func.layout name]
		 end)
	     val {get = labelInfo: Label.t -> {ancestor: Label.t option ref,
					       facts: Fact.t list ref,
					       inDeg: int ref},
		  ...} =
		Property.get
		(Label.plist, Property.initFun (fn _ => {ancestor = ref NONE,
							 facts = ref [],
							 inDeg = ref 0}))
	     (* Set up inDeg. *)
	     val _ =
		Vector.foreach
		(blocks, fn Block.T {transfer, ...} =>
		 Transfer.foreachLabel
		 (transfer, Int.inc o #inDeg o labelInfo))
	     (* Perform analysis, set up facts, and set up ancestor. *)
	     fun loop (Tree.T (Block.T {label, statements, transfer, ...},
			       children),
		       ancestor') =
	        let
		   val _ = 
		      Vector.foreach
		      (statements, fn Statement.T {var, exp, ...} =>
		       case exp of
			  Exp.Const c =>
			     Option.app (var, fn x => setConst (x, c))
			| Exp.PrimApp pa =>
			     Option.app (var, fn x =>
					 setVarInfo (x, makeVarInfo pa))
			| _ => ())
		   val _ = 
		      case transfer of
			 Case {test, cases, default, ...} =>
			    let
			       fun add (l, f) =
				  let
				     val {facts, inDeg, ...} = labelInfo l
				  in
				     if !inDeg = 1
				        then List.push (facts, f)
				     else ()
				  end		
			       fun falseTrue () =
				  case cases of
				     Cases.Con v =>
				        let
					   fun ca i = Vector.sub (v, i)
					in
					   case (Vector.length v, default) of
					      (1, SOME l') =>
						 let
						    val (c, l) = ca 0
						 in
						    if Con.equals (c, Con.truee)
						       then (l', l)
						    else (l, l')
						 end
					    | (2, _) =>
						 let
						    val (c, l) = ca 0
						    val (_, l') = ca 1
						 in
						    if Con.equals (c, Con.truee)
						       then (l', l)
						    else (l, l')
						 end
					    | _ => Error.bug "redundant expected two branches"
					end
				   | _ => Error.bug "redundant expected con"
			    in
			       case varInfo test of
				  Fact f =>
				     let
				        val (l, l') = falseTrue ()
				     in
				       add (l, Fact.negate f)
				       ; add (l', f)
				     end
				| Or (f, f') =>
				     let
				        val (l, _) = falseTrue ()
				     in
				        add (l, Fact.negate f) 
					; add (l, Fact.negate f')
				     end
				| _ => ()
			    end
		       | _ => ()

		   val {ancestor, facts, ...} = labelInfo label
		   val _ = ancestor := ancestor'
		   val ancestor' = if List.isEmpty (!facts)
				      then ancestor'
				   else SOME label
		in
		   Vector.foreach 
		   (children, fn tree => loop (tree, ancestor'))
		end
	     val _ = loop (Function.dominatorTree f, NONE)
	     (* Diagnostic. *)
	     val _ = 
		Control.diagnostics
		(fn display =>
		 Vector.foreach
		 (blocks, fn Block.T {label, ...} =>
		  let open Layout
		  in display (seq [Label.layout label,
				   str " ",
				   List.layout Fact.layout
				   (! (#facts (labelInfo label)))])
		  end))
             (* Transformation. *)
	     fun isFact (l: Label.t, p: Fact.t -> bool): bool =
		let
		   fun loop (l: Label.t) =
		      let
			 val {ancestor, facts, ...} = labelInfo l
		      in
			 List.exists (!facts, p)
			 orelse (case !ancestor of
				    NONE => false
				  | SOME l => loop l)
		      end
		in
		   loop l
		end
	     fun determine (l: Label.t, f: Fact.t) =
	        let
		   fun loop {ancestor, facts, ...} =
		      case Fact.determine (!facts, f) of
		         Unknown =>
			    (case !ancestor of
			        NONE => Unknown
			      | SOME l => loop (labelInfo l))
		     | r => r
		in
		   loop (labelInfo l)
		end
	     val blocks =
	        Vector.map
		(blocks, fn Block.T {label, args, statements, transfer} =>
		 let
		    val statements =
		       Vector.map
		       (statements, fn statement as Statement.T {ty, var, ...} =>
			let
			   fun doit x =
			      (Int.inc numSimplified
			       ; Control.diagnostic
			         (fn () =>
				  let open Layout
				  in seq [Option.layout Var.layout var,
					  str " -> ",
					  Var.layout x]
				  end)
			       ; Statement.T {var = var, 
					      ty = ty,
					      exp = Var x})
			   fun falsee () = doit falseVar
			   fun truee () = doit trueVar
			in
			   case var of
			      NONE => statement
			    | SOME var =>
				 (case varInfo var of
				     Or (f, f') =>
				        (case determine (label, f) of
					    False =>
					       (case determine (label, f') of
						   False => falsee ()
						 | True => truee ()
						 | Unknown => statement)
					  | True => truee ()
					  | Unknown => statement)
				   | Fact f => 
				        (case determine (label, f) of
					    False => falsee ()
					  | True => truee () 
					  | Unknown => statement)
				   | _ => statement)
			end)
		    val noChange = (statements, transfer)
		    fun arith (args: Var.t vector,
			       prim: Prim.t,
			       success: Label.t)
		       : Statement.t vector * Transfer.t =
		       let
			  fun simplify (prim: Prim.t, x: Var.t, s: IntSize.t) =
			     let
				val res = Var.newNoname ()
			     in
				(Vector.concat
				 [statements,
				  Vector.new1
				  (Statement.T
				   {exp = PrimApp {args = Vector.new2 (x, one s),
						   prim = prim,
						   targs = Vector.new0 ()},
				    ty = Type.int s,
				    var = SOME res})],
				 Goto {args = Vector.new1 res,
				       dst = success})
			     end
			  fun add1 (x: Var.t, s: IntSize.t) =
			     if isFact (label, fn Fact.T {lhs, rel, rhs} =>
					case (lhs, rel, rhs) of
					   (Oper.Var x', Rel.LT, _) =>
					      Var.equals (x, x')
					 | (Oper.Var x', Rel.LE, Oper.Const c) =>
					      Var.equals (x, x')
					      andalso
					      (case c of
						  Const.Int i =>
						     IntX.<
						     (i, IntX.max (IntX.size i))
						| _ => Error.bug "strange fact")
					 | _ => false)
				then simplify (Prim.intAdd s, x, s)
			     else noChange
			  fun sub1 (x: Var.t, s: IntSize.t) =
			     if isFact (label, fn Fact.T {lhs, rel, rhs} =>
					case (lhs, rel, rhs) of
					   (_, Rel.LT, Oper.Var x') =>
					      Var.equals (x, x')
					 | (Oper.Const c, Rel.LE, Oper.Var x') =>
					      Var.equals (x, x')
					      andalso
					      (case c of
						  Const.Int i =>
						     IntX.>
						     (i, IntX.min (IntX.size i))
						| _ => Error.bug "strange fact")
					 | _ => false)
				then simplify (Prim.intSub s, x, s)
			     else noChange
			  fun add (c: Const.t, x: Var.t, s: IntSize.t) =
			     case c of
				Const.Int i =>
				   if IntX.isOne i
				      then add1 (x, s)
				   else if IntX.isNegOne i
					   then sub1 (x, s)
					else noChange
			      | _ => Error.bug "add of strange const"
			  datatype z = datatype Prim.Name.t
		       in
			  case Prim.name prim of
			     Int_addCheck s =>
				let
				   val x1 = Vector.sub (args, 0)
				   val x2 = Vector.sub (args, 1)
				in
				   case varInfo x1 of
				      Const c => add (c, x2, s)
				    | _ => (case varInfo x2 of
					       Const c => add (c, x1, s)
					     | _ => noChange)
				end
			   | Int_subCheck s =>
				let
				   val x1 = Vector.sub (args, 0)
				   val x2 = Vector.sub (args, 1)
				in
				   case varInfo x2 of
				      Const c =>
					 (case c of
					     Const.Int i =>
						if IntX.isNegOne i
						   then add1 (x1, s)
						else if IntX.isOne i
							then sub1 (x1, s)
						     else noChange
					   | _ =>
						Error.bug "sub of strage const")
				    | _ => noChange
				end
			   | _ => noChange
		       end
		    val (statements, transfer) =
		       if !Control.eliminateOverflow
			  then
			     case transfer of
				Arith {args, prim, success, ...} =>
				   arith (args, prim, success)
			      | _ => noChange
		       else noChange
		 in
		   Block.T {label = label,
			    args = args,
			    statements = statements,
			    transfer = transfer}
		 end)
	  in
	     shrink (Function.new {args = args,
				   blocks = blocks,
				   name = name,
				   raises = raises,
				   returns = returns,
				   start = start})
	  end
      val _ =
	 Control.diagnostic
	 (fn () =>
	  let open Layout
	  in seq [str "numSimplified = ", Int.layout (!numSimplified)]
	  end)
      val functions = List.revMap (functions, simplifyFunction)
      val program = 
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
      val _ = Program.clearTop program
   in
      program
   end
end
