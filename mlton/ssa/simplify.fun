(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Simplify (S: SIMPLIFY_STRUCTS): SIMPLIFY = 
struct

open S

structure CommonBlock = CommonBlock (S)
structure CommonSubexp = CommonSubexp (S)
structure ConstantPropagation = ConstantPropagation (S)
structure Contify = Contify (S)
structure Flatten = Flatten (S)
structure Inline = Inline (S)
structure IntroduceLoops = IntroduceLoops (S)
structure LocalFlatten = LocalFlatten (S)
structure LocalRef = LocalRef (S)
structure LoopInvariant = LoopInvariant (S)
structure PolyEqual = PolyEqual (S)
structure Redundant = Redundant (S)
structure RedundantTests = RedundantTests (S)
structure RemoveUnused = RemoveUnused (S)
structure SimplifyTypes = SimplifyTypes (S)
structure Useless = Useless (S)

fun leafInline p =
   Ref.fluidLet
   (Control.inline, Control.Leaf {size = SOME 20 (* arbitrary *)}, fn () =>
    Inline.inline p)

fun traces s p = (Trace.Immediate.on s; p)

val passes =
   [
    ("removeUnused1", RemoveUnused.remove),
    ("leafInline", leafInline),
    (* contify should be run before constant propagation because of the once
     * pass that only looks at main -- hence want as much in main as possible.
     *)
    ("contify1", Contify.contify),
    ("localFlatten1", LocalFlatten.flatten),
    (* constantPropagation cannot be omitted. It implements Array_array0. *)
    ("constantPropagation", ConstantPropagation.simplify),
    (*
     * useless should run after constantPropagation because constantPropagation
     * makes slots of tuples that are constant useless.
     *)
    ("useless", Useless.useless),
    ("removeUnused2", RemoveUnused.remove),
    ("simplifyTypes", SimplifyTypes.simplify),
    (* polyEqual cannot be omitted.  It implements MLton_equal.
     * polyEqual should run
     *   - after types are simplified so that many equals are turned into eqs
     *   - before inlining so that equality functions can be inlined
     *)
    ("polyEqual", PolyEqual.polyEqual),
    ("contify2", Contify.contify),
    ("inline", Inline.inline),
    ("localFlatten2", LocalFlatten.flatten),
    ("removeUnused3", RemoveUnused.remove),
    ("contify3", Contify.contify),
    ("introduceLoops", IntroduceLoops.introduceLoops),
    ("loopInvariant", LoopInvariant.loopInvariant),
    ("localRef", LocalRef.eliminate),
    (* flatten cannot be omitted.  It ensures(?) that no spurious
     * allocations occur between allocation of an intInf return 
     * and the call to the primitive.
     *)
    ("flatten", Flatten.flatten),
    ("localFlatten3", LocalFlatten.flatten),
    ("commonSubexp", CommonSubexp.eliminate),
    ("commonBlock", CommonBlock.eliminate),
    ("redundantTests", RedundantTests.simplify),
    ("redundant", Redundant.redundant),
    ("removeUnused4", RemoveUnused.remove)
    ]
   
fun stats p =
   Control.message (Control.Detail, fn () => Program.layoutStats p)

fun simplify p =
   let
      fun simplify' n p =
	 let
	    val n' = Int.toString n
	 in
	    if n = !Control.loopPasses
	       then p
	    else simplify' 
	         (n + 1)
		 (List.fold
	          (passes, p, fn ((name, pass), p) =>
		   if List.contains (!Control.dropPasses, name, String.equals)
		      then p
		   else
		     let
(*
		       val _ = List.push(Control.keepPasses, name)
		       val _ = List.push(Control.keepDiagnostics, name)
*)
		       val _ =
			  let
			     open Control
			  in maybeSaveToFile
			     ({name = name, suffix = n' ^ ".pre.ssa"},
			      Control.No, p, Control.Layouts Program.layouts)
			  end
		       val p =
			  Control.passTypeCheck
			  {name = name,
			   suffix = n' ^ ".post.ssa",
			   style = Control.No,
			   thunk = fn () => pass p,
			   display = Control.Layouts Program.layouts,
			   typeCheck = typeCheck}
		       val _ = stats p
		     in
		       p
		     end))
	 end
   in
     stats p
     ; simplify' 0 p
   end

(*
fun simplify p =
   (stats p
    ; (List.fold
       (passes, p, fn ((name, pass), p) =>
      if List.contains (!Control.dropPasses, name, String.equals)
         then p
      else
         let
(*
	    val _ = List.push(Control.keepPasses, name)
	    val _ = List.push(Control.keepDiagnostics, name)
*)
            val _ =
	       let
		  open Control
	       in maybeSaveToFile
		  ({name = name, suffix = "pre.ssa"},
		   Control.No, p, Control.Layouts Program.layouts)
	       end
            val p =
               Control.passTypeCheck
               {name = name,
                suffix = "post.ssa",
                style = Control.No,
                thunk = fn () => pass p,
                display = Control.Layouts Program.layouts,
                typeCheck = typeCheck}
            val _ = stats p
         in
            p
         end)))
*)

val typeCheck = S.typeCheck

val simplify = fn p => let
			 (* Always want to type check the initial and final SSA 
			  * programs, even if type checking is turned off, just
			  * to catch bugs.
			  *)
			 val _ = typeCheck p
			 val p' = simplify p
			 val _ = typeCheck p'
		       in
			 p'
		       end

end
