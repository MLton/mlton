(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor LocalRef (S: LOCAL_REF_STRUCTS): LOCAL_REF = 
struct

open S
open Exp Transfer

type int = Int.t
type word = Word.t

structure FuncLattice = FlatLattice(structure Point = Func)

structure GlobalInfo =
  struct
    datatype t = T of {isGlobalRef: bool,
		       funcUses: FuncLattice.t}

    fun layout (T {isGlobalRef, funcUses, ...})
      = let open Layout
	in record [("isGlobalRef", Bool.layout isGlobalRef),
		   ("funcUses", FuncLattice.layout funcUses)]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val isGlobalRef = make #isGlobalRef
      val funcUses = make #funcUses
    end

    fun new isGlobalRef = T {isGlobalRef = isGlobalRef, 
			     funcUses = FuncLattice.new ()}
  end

structure Local =
  struct
    structure L = TwoPointLattice (val bottom = "local"
				   val top = "non local")
    open L
    val isLocal = isBottom
    val nonLocal = makeTop
  end

structure VarInfo =
  struct
    datatype t = T of {reff: (Label.t * Type.t) option,
		       assigns: Label.t list ref,
		       derefs: Label.t list ref,
		       locall: Local.t, 
		       threadCopyCurrent: {assign: bool ref,
					   deref: bool ref}}

    fun layout (T {reff, assigns, derefs, locall, 
		   threadCopyCurrent as {assign, deref, ...}, ...})
      = let open Layout
	in record [("reff", Option.layout (tuple2 (Label.layout, Type.layout)) reff),
		   ("assigns", List.layout Label.layout (!assigns)),
		   ("derefs", List.layout Label.layout (!derefs)),
		   ("locall", Local.layout locall),
		   ("threadCopyCurrent", record [("assign", Bool.layout (!assign)),
						 ("deref", Bool.layout (!deref))])]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val reff = make #reff
      val (assigns, assigns') = make' #assigns
      val (derefs, derefs') = make' #derefs
      val locall = make #locall
      val threadCopyCurrent = make #threadCopyCurrent
    end
    val isLocal = Local.isLocal o locall
    val nonLocal = Local.nonLocal o locall
    local
      fun make f = f o threadCopyCurrent
      fun make' f = (make f, ! o (make f))
    in
      val (threadCopyCurrentAssign,threadCopyCurrentAssign') = make' #assign
      val (threadCopyCurrentDeref,threadCopyCurrentDeref') = make' #deref
    end

    fun new reff: t = T {reff = reff, 
			 assigns = ref [],
			 derefs = ref [],
			 locall = let
				    val locall = Local.new ()
				    val _ = if isSome reff
					      then ()
					      else Local.nonLocal locall
				  in
				    locall
				  end,
			 threadCopyCurrent = {assign = ref false,
					      deref = ref false}}
  end

structure LabelInfo =
  struct
    datatype t = T of {reffs: Var.t list ref,
		       assigns: Var.t list ref,
		       derefs: Var.t list ref,
		       preds: Label.t list ref,
		       visited: bool ref}

    fun layout (T {reffs, assigns, derefs, ...})
      = let open Layout
	in record [("reffs", List.layout Var.layout (!reffs)),
		   ("assigns", List.layout Var.layout (!assigns)),
		   ("derefs", List.layout Var.layout (!derefs))]
	end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val (reffs, reffs') = make' #reffs
      val (assigns, assigns') = make' #assigns
      val (derefs, derefs') = make' #derefs
      val (preds, preds') = make' #preds
      val (visited, visited') = make' #visited
    end

    fun new (): t = T {reffs = ref [],
		       assigns = ref [],
		       derefs = ref [],
		       preds = ref [],
		       visited = ref false}
  end

structure Multi = Multi (S)

fun eliminate (program: Program.t): Program.t
  = let
      val program as Program.T {datatypes, globals, functions, main}
	= eliminateDeadBlocks program

      exception NoLocalRefs

      (* Compute multi *)
      val multi = Control.trace (Control.Detail, "multi") Multi.multi
      val {usesThreadsOrConts: bool,
	   funcIsMultiUsed: Func.t -> bool, 
	   labelDoesThreadCopyCurrent: Label.t -> bool, ...} 
	= multi program

      (* Initialize globalInfo *)
      val {get = globalInfo: Var.t -> GlobalInfo.t,
	   set = setGlobalInfo, ...}
	= Property.getSetOnce
	  (Var.plist, Property.initFun (fn _ => GlobalInfo.new false))

      val _ = Vector.foreach
	      (globals, fn Statement.T {var, exp, ...} =>
	       Option.app (var, fn var => 
			   case exp
			     of PrimApp {prim, ...}
			      => if Prim.name prim = Prim.Name.Ref_ref
				   then setGlobalInfo(var, GlobalInfo.new true)
				   else ()
			      | _ => ()))

      (* Compute funcUses *)
      fun addFunc f x
	= let 
	    val gi = globalInfo x
	  in 
	    if GlobalInfo.isGlobalRef gi
	      then ignore (FuncLattice.lowerBound (GlobalInfo.funcUses gi, f))
	      else ()
	  end
      val dummy = Func.newNoname ()
      val _ = Vector.foreach
	      (globals, fn Statement.T {var, exp, ...} =>
	       let
		 fun default () = Exp.foreachVar (exp, addFunc dummy)
	       in
		 case exp
		   of PrimApp {prim, args, ...}
		    => if Prim.name prim = Prim.Name.Ref_ref
			 then ignore
			      (FuncLattice.<=
			       (GlobalInfo.funcUses 
				(globalInfo (valOf var)),
				GlobalInfo.funcUses 
				(globalInfo (Vector.sub (args, 0)))))
			 else default ()
		    | _ => default ()
	       end)
      val _ = List.foreach
	      (functions, fn f =>
	       let
		 val {name, blocks, ...} = Function.dest f
	       in
		 Vector.foreach
		 (blocks, fn Block.T {statements, transfer, ...} =>
		  (Vector.foreach
		   (statements, fn Statement.T {exp, ...} =>
		    Exp.foreachVar (exp, addFunc name)) ;
		   Transfer.foreachVar (transfer, addFunc name)))
	       end)

      (* Diagnostics *)
      val _ = Control.diagnostics
	      (fn display =>
	       let
		 open Layout
	       in
		 display (str "\n\nGlobals:") ;
		 Vector.foreach
		 (globals, fn Statement.T {var, ...} =>
		  Option.app 
		  (var, fn x =>
		   if GlobalInfo.isGlobalRef (globalInfo x)
		     then display (seq [Var.layout x,
					str ": ",
					GlobalInfo.layout (globalInfo x)])
		     else ()))
	       end)

      (* Localize global refs *)
      val (functions,globals)
	= List.fold
	  (functions, ([],Vector.toList globals), fn (f, (functions, globals)) =>
	   if funcIsMultiUsed (Function.name f)
	     then (f::functions,globals)
	     else let
		    val {args, blocks, name, raises, returns, start} =
		       Function.dest f

		    val (globals, locals)
		      = List.fold
		        (globals, ([],[]), fn (s as Statement.T {var, ...},
					       (globals, locals)) =>
			 if case var
			      of NONE => false
			       | SOME x 
			       => let
				    val gi = globalInfo x
				  in 
				    GlobalInfo.isGlobalRef gi
				    andalso
				    FuncLattice.isPointEq
				    (GlobalInfo.funcUses gi, name)
				  end
			   then (globals,s::locals)
			   else (s::globals,locals))

		    val locals = Vector.fromListRev locals
		    val f = if Vector.length locals = 0
			      then f
			      else let
				     val localsLabel = Label.newNoname ()
				     val localsBlock
				       = Block.T
				         {label = localsLabel,
					  args = Vector.new0 (),
					  statements = locals,
					  transfer = Goto {dst = start,
							   args = Vector.new0 ()}}
				     val blocks = Vector.concat 
				                  [Vector.new1 localsBlock,
						   blocks]
				   in
				     Function.new {args = args,
						   blocks = blocks,
						   name = name,
						   raises = raises,
						   returns = returns,
						   start = localsLabel}
				   end
		  in
		    (f::functions, List.rev globals)
		  end)
      val globals = Vector.fromList globals

      (* restore and shrink *)
      val restore = restoreFunction globals
      val shrink = shrinkFunction globals

      (* varInfo *)
      val {get = varInfo: Var.t -> VarInfo.t,
	   set = setVarInfo, ...} 
	= Property.getSetOnce
	  (Var.plist, Property.initFun (fn _ => VarInfo.new NONE))
      fun nonLocal x = VarInfo.nonLocal (varInfo x)
      fun isLocal x = VarInfo.isLocal (varInfo x)

      (* labelInfo *)
      val {get = labelInfo: Label.t -> LabelInfo.t, 
	   set = setLabelInfo, ...}
	= Property.getSetOnce
	  (Label.plist, Property.initRaise ("localRef.labelInfo", Label.layout))

      val functions
	= List.revMap
	  (functions, fn f =>
	   let
	     val {args, blocks, name, raises, returns, start} = Function.dest f

	     (* Find all localizable refs. *)
	     val refs = ref []
	     fun visitStatement label
				(s: Statement.t as Statement.T {var, ty, exp})
	       = let
		   val li = labelInfo label
		   fun setReff ()
		     = Option.app
		       (var, fn var =>
			let
			  val vi = VarInfo.new (SOME (label, Type.deref ty))
			  val _ = setVarInfo (var, vi)
			in
			  List.push (refs, var) ;
			  List.push (LabelInfo.reffs li, var)
			end)
		   fun setAssign var
		     = (List.push (VarInfo.assigns (varInfo var), label) ;
			List.push (LabelInfo.assigns li, var))
		   fun setDeref var
		     = (List.push (VarInfo.derefs (varInfo var), label) ;
			List.push (LabelInfo.derefs li, var))
		   fun default () = Exp.foreachVar (exp, nonLocal)
		   datatype z = datatype Prim.Name.t
		 in
		   case exp
		     of PrimApp {prim, args, ...}
		      => let
			   fun arg n = Vector.sub (args, n)
			 in
			   case Prim.name prim
			     of Ref_ref => (setReff (); default ())
			      | Ref_assign => (setAssign (arg 0); 
					       nonLocal (arg 1))
			      | Ref_deref => setDeref (arg 0)
			      | _ => default ()
			 end
		      | _ => default ()
		 end
	     fun visitBlock (Block.T {label, args, statements, transfer, ...})
	       = let
		   val li = LabelInfo.new ()
		   val _ = setLabelInfo (label, li)
		   val _ = Vector.foreach (statements, visitStatement label)
		   val _ = Transfer.foreachVar (transfer, nonLocal)
		 in
		   if usesThreadsOrConts
		     then fn () => Transfer.foreachLabel
		                   (transfer, fn l =>
				    List.push (LabelInfo.preds (labelInfo l), label))
		     else fn () => ()
		 end
	     val _ = Function.dfs (f, visitBlock)
	     val refs = List.keepAll (!refs, isLocal)

	     (* Thread criteria *)
	     val refs
	       = if usesThreadsOrConts
		   then (List.foreach
			 (refs, fn x =>
			  let
			    val vi = varInfo x
			    val def = #1 (valOf (VarInfo.reff vi))
			    val assigns = VarInfo.assigns' vi
			    val derefs = VarInfo.derefs' vi

			    fun doit (threadCopyCurrent, uses)
			      = let
				  val visited = ref []
				  fun doit' l
				    = let
					val li = labelInfo l
				      in
					if LabelInfo.visited' li
					  then ()
					  else (List.push (visited, l);
						LabelInfo.visited li := true;
						if labelDoesThreadCopyCurrent l
						  then threadCopyCurrent := true
						  else ();
						if Label.equals (def, l)
						  then ()
						  else List.foreach
						       (LabelInfo.preds' li, doit'))
				      end
				in
				  List.foreach 
				  (uses, fn l =>
				   List.foreach
				   (LabelInfo.preds' (labelInfo l), doit')) ;
				  List.foreach
				  (!visited, fn l =>
				   LabelInfo.visited (labelInfo l) := false)
				end
			    val _ = doit (VarInfo.threadCopyCurrentAssign vi,
					  !(VarInfo.assigns vi))
			    val _ = doit (VarInfo.threadCopyCurrentDeref vi,
					  !(VarInfo.derefs vi))
			  in
			    if VarInfo.threadCopyCurrentAssign' vi
			       andalso
			       VarInfo.threadCopyCurrentDeref' vi
			      then VarInfo.nonLocal vi
			      else ()
			  end);
			 List.keepAll (refs, isLocal))
		   else refs

	     (* Escape early when there are no localizable refs *)
	     val _ = if List.length refs = 0
		       then (Function.clear f;
			     Control.diagnostics
			     (fn display =>
			      let
				open Layout
			      in
				display (seq [str "\n",
					      Func.layout name,
					      str " NoLocalRefs"])
			      end);
			     raise NoLocalRefs)
		       else ()

	     (* Diagnostics *)
	     val _ = Control.diagnostics
	             (fn display =>
		      let
			open Layout
		      in
			display (seq [str "\n",
				      Func.layout name,
				      str " LocalRefs: ",
				      List.layout 
				      (fn x =>
				       seq [Var.layout x,
					    str ": ",
					    VarInfo.layout (varInfo x)])
				      refs])
		      end)

	     (* Rewrite. *)
	     fun rewriteStatement (s: Statement.t as Statement.T {var, ty, exp})
	       = let
		   datatype z = datatype Prim.Name.t
		 in
		   case exp
		     of PrimApp {prim, args, ...}
		      => let
			   fun arg n = Vector.sub (args, n)

			   fun rewriteReffAssign rvar var
			     = let
				 val vi = varInfo rvar
			       in
				 if VarInfo.isLocal vi
				   then Statement.T
				        {var = SOME rvar,
					 ty = #2 (valOf (VarInfo.reff vi)),
					 exp = Var var}
				   else s
			       end
			   fun rewriteReff ()
			     = case var 
				 of NONE => s
				  | SOME var => rewriteReffAssign var (arg 0)
			   fun rewriteAssign () = rewriteReffAssign (arg 0) (arg 1)
			   fun rewriteDeref rvar
			     = let
				 val vi = varInfo rvar
			       in
				 if VarInfo.isLocal vi
				   then let
					in
					  Statement.T
					  {var = var,
					   ty = #2 (valOf (VarInfo.reff vi)),
					   exp = Var rvar}
					end
				   else s
			       end
			   val rewriteDeref
			     = fn () => rewriteDeref (arg 0)
			 in
			   case Prim.name prim
			     of Ref_ref => rewriteReff ()
		              | Ref_assign => rewriteAssign ()
			      | Ref_deref => rewriteDeref ()
			      | _ => s
			 end
		      | _ => s
		 end
	     fun rewriteBlock (Block.T {label, args, statements, transfer})
	       = let
		   val li = labelInfo label
		   (* Don't need to rewrite the statements
		    * if this block doesn't mention localizable refs.
		    *)
		   val statements
		     = if List.exists (LabelInfo.reffs' li, isLocal)
		          orelse
			  List.exists (LabelInfo.assigns' li, isLocal)
			  orelse
			  List.exists (LabelInfo.derefs' li, isLocal)
			 then Vector.map (statements, rewriteStatement)
			 else statements
		 in
		   Block.T {label = label,
			    args = args,
			    statements = statements,
			    transfer = transfer}
		 end
	     val blocks = Vector.map (blocks, rewriteBlock)
	     val f = Function.new {args = args,
				   blocks = blocks,
				   name = name,
				   raises = raises,
				   returns = returns,
				   start = start}
	     val f = restore f
	     val f = shrink f
	   in
	     f
	   end
	   handle NoLocalRefs => f)
      val program = Program.T {datatypes = datatypes,
			       globals = globals,
			       functions = functions,
			       main = main}
      val _ = Program.clearTop program
    in
      program
    end
end
