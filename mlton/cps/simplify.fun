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
structure LoopInvariant = LoopInvariant (S)
structure PolyEqual = PolyEqual (S)
structure RaiseToJump = RaiseToJump (S)
structure Redundant = Redundant (S)
structure RedundantTests = RedundantTests (S)
structure RemoveUnused = RemoveUnused (S)
structure SimplifyTypes = SimplifyTypes (S)
structure UnusedArgs = UnusedArgs (S)
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
    ("raiseToJump1", RaiseToJump.raiseToJump),
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
    ("unusedArgs1", UnusedArgs.unusedArgs),
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
    ("raiseToJump2", RaiseToJump.raiseToJump),
    ("contify3", Contify.contify),
    ("unusedArgs2", UnusedArgs.unusedArgs),
    ("introduceLoops", IntroduceLoops.introduceLoops),
    ("loopInvariant", LoopInvariant.loopInvariant),
    ("flatten", Flatten.flatten),
    ("localFlatten3", LocalFlatten.flatten),
    ("commonSubexp", CommonSubexp.eliminate),
    ("commonBlock", CommonBlock.eliminate),
    ("redundantTests", RedundantTests.simplify),
    ("redundant", Redundant.redundant),
    ("unusedArgs3", UnusedArgs.unusedArgs),
     (* removeUnused cannot be omitted.
      * The final shrink pass ensures that constant operands are
      * not used in dead switch branches in a type-unsafe way.
      * This ensures that constants are not used where pointers
      * are expected in the Operand.offsets generated in the
      * backend.
      *)
    ("removeUnused4", RemoveUnused.remove)
    ]

fun stats p =
   Control.message (Control.Detail, fn () => Program.layoutStats p)

fun simplify p =
   (stats p
    ; (List.fold
       (passes, p, fn ((name, pass), p) =>
	if List.contains (!Control.dropPasses, name, String.equals)
	   then p
	else
	   let
	      val p =
		 Control.passTypeCheck
		 {name = name,
		  suffix = "cps",
		  style = Control.No,
		  thunk = fn () => pass p,
		  display = Control.Layouts Program.layouts,
		  typeCheck = typeCheck}
	      val _ = stats p
	   in
	      p
	   end)))

val typeCheck = S.typeCheck

val simplify = fn p => let
			 (* Always want to type check the initial and final CPS 
			  * programs, even if type checking is turned off, just 
			  * to catch bugs.
			  *)
(*			 val _ = typeCheck p *)
			 val p' = simplify p
			 val _ = typeCheck p'
		       in
			 p'
		       end

end
