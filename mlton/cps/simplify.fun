(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Simplify (S: SIMPLIFY_STRUCTS): SIMPLIFY = 
struct

open S

fun $ (f, g) x = g (f x)
infixr $

fun stats p =
   (Control.message (Control.Detail, fn () => Program.layoutStats p)
    ; p)

val typeCheck =
   stats
   $ (fn p => (if !Control.typeCheck
		  then Control.trace (Control.Pass, "typeCheck") typeCheck p
	       else ()
               ; p))

fun trace (name, pass) =
   Control.trace (Control.Pass, name)
   (Trace.trace ("Simplify." ^ name, Program.layout, Program.layout) pass)

structure CommonSubexp = CommonSubexp (S)
val commonSubexp =
   trace ("commonSubexp", CommonSubexp.eliminate)
   
structure ConstantPropagation = ConstantPropagation (S)
val constantPropagation =
   trace ("constantPropagation", ConstantPropagation.simplify)

structure Contify = Contify (S)
val contify = trace ("contify", Contify.contify)

structure Flatten = Flatten (S)
val flatten = trace ("flatten", Flatten.flatten)
(* val flatten =
 *    fn p =>
 *    Int.fold (0, !Control.flattenRounds, p, fn (_, p) => typeCheck (flatten p))
 *)

structure Inline = Inline (S)
val inline = trace ("inline", Inline.inline)

structure IntroduceLoops = IntroduceLoops (S)
val introduceLoops = trace ("introduce-loops", IntroduceLoops.introduceLoops)

(*structure LoopCount = LoopCount (S) *)

structure LocalFlatten = LocalFlatten (S)
val localFlatten = trace ("local-flatten", LocalFlatten.flatten)
   
structure LoopInvariant = LoopInvariant (S)
val loopInvariant = trace ("loop-invariant", LoopInvariant.loopInvariant)
   
structure PolyEqual = PolyEqual (S)
val polyEqual = trace ("poly-equal", PolyEqual.polyEqual)

structure RaiseToJump = RaiseToJump (S)
val raiseToJump = trace ("raise-to-jump", RaiseToJump.raiseToJump)

structure Redundant = Redundant (S)
val redundant = trace ("redundant", Redundant.redundant)

structure RedundantTests = RedundantTests (S)
val redundantTests = trace ("redundant tests", RedundantTests.simplify)

structure RemoveUnused = RemoveUnused (S)
val removeUnused = trace ("removeUnused", RemoveUnused.remove)
   
structure SimplifyTypes = SimplifyTypes (S)
val simplifyTypes = trace ("simplifyTypes", SimplifyTypes.simplify)

structure UnusedArgs = UnusedArgs (S)
val unusedArgs = trace ("unusedArgs", UnusedArgs.unusedArgs)

structure Useless = Useless (S)
val useless = trace ("useless", Useless.useless)

fun leafInline p =
   Ref.fluidLet
   (Control.inline, Control.Leaf {size = SOME 20 (* arbitrary *)}, fn () =>
    inline p)
val leafInline = trace ("leaf-inline", leafInline)
			   
fun save suffix p =
   (Ref.fluidLet
    (Control.aux, true, 
     fn () => Control.displays (suffix, fn layout =>
				Program.layouts (p, layout)))
    ; p)

(*
 * useless should run after constant propagation because const prop makes
 *  slots of tuples that are constant useless
 *
 * contify should be run before constant propagation because of the once pass
 *  that only looks at main -- hence want as much in main as possible.
 *
 * poly equal should run
 *   - after types are simplified so that many equals are turned into eqs
 *   - before inlining so that equality functions can be inlined
 *)

fun traces s p = (Trace.Immediate.on s; p)

val removeUnused = shrink o removeUnused

val passes =
   [
    removeUnused,
    leafInline,
    raiseToJump,
    contify,
    localFlatten,
    constantPropagation,  (* constantPropagation cannot be omitted.
			   * It implements Array_array0.
			   *)
    useless,
    removeUnused,
    unusedArgs,
    simplifyTypes,
    polyEqual, (* polyEqual cannot be omitted.  It implements MLton_equal. *)
    contify,
    inline,
    localFlatten,
    removeUnused,
    raiseToJump,
    contify,
    unusedArgs,
    introduceLoops,
    loopInvariant,
    flatten,
    localFlatten,
    commonSubexp,
    redundantTests,
    redundant,
    unusedArgs,
    removeUnused  (* removeUnused cannot be omitted.
		   * The final shrink pass ensures that constant operands are
		   * not used in dead switch branches in a type-unsafe way.
		   * This ensures that constants are not used where pointers
		   * are expected in the Operand.offsets generated in the
		   * backend.
		   *)
    ]
   
fun simplify p =
   List.fold (passes, stats p, fn (pass, p) => typeCheck (pass p))

(*
fun display (pass, p, s, i)
  = let open Control
    in if !keepCps
	 then (Ref.fluidLet
	       (aux, true, fn () =>
		displays (s ^ "." ^ pass ^ ".cps" ^ (Int.toString i), fn disp =>
			  (outputHeader (No, disp)
			   ; Program.layouts (p, disp)))))
	 else ()
    end
in
fun simplify p =
   List.fold
   (passes,
    stats p,
    fn (pass, p)
     => let
	  val _ = display (pass, p, "pre", !n)
	  val p' = pass p
	  val _ = display (pass, p', "post", !n)
	  val _ = n := !n + 1
	in
	  typeCheck p'
	end)
end
*)
   
val typeCheck = S.typeCheck

val simplify = fn p => let val p' = simplify p
                       in (typeCheck p' 
			   ; p')
		       end 

end
