(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Simplify2 (S: SIMPLIFY2_STRUCTS): SIMPLIFY2 = 
struct

open S

(* structure CommonArg = CommonArg (S) *)
(* structure CommonBlock = CommonBlock (S) *)
(* structure CommonSubexp = CommonSubexp (S) *)
(* structure ConstantPropagation = ConstantPropagation (S) *)
(* structure Contify = Contify (S) *)
structure DeepFlatten = DeepFlatten (S)
(* structure Flatten = Flatten (S) *)
(* structure Inline = Inline (S) *)
(* structure IntroduceLoops = IntroduceLoops (S) *)
(* structure KnownCase = KnownCase (S) *)
(* structure LocalFlatten = LocalFlatten (S) *)
(* structure LocalRef = LocalRef (S) *)
(* structure LoopInvariant = LoopInvariant (S) *)
(* structure PolyEqual = PolyEqual (S) *)
(* structure Redundant = Redundant (S) *)
(* structure RedundantTests = RedundantTests (S) *)
structure RefFlatten = RefFlatten (S)
structure RemoveUnused2 = RemoveUnused2 (S)
(* structure SimplifyTypes = SimplifyTypes (S) *)
(* structure Useless = Useless (S) *)
structure Zone = Zone (S)

(*
fun inlineNonRecursive (product, small) p =
   Ref.fluidLet
   (Control.inline,
    Control.NonRecursive {product = product, small = small},
    fn () => Inline.inline p)
fun inlineLeaf size p =
   Ref.fluidLet
   (Control.inline,
    Control.Leaf {size = SOME size},
    fn () => Inline.inline p)
fun inlineLeafNoLoop size p =
   Ref.fluidLet
   (Control.inline,
    Control.LeafNoLoop {size = SOME size},
    fn () => Inline.inline p)
*)

type pass = {name: string,
	     doit: Program.t -> Program.t}

val ssa2PassesDefault = 
   {name = "deepFlatten", doit = DeepFlatten.flatten} ::
   {name = "refFlatten", doit = RefFlatten.flatten} ::
   {name = "removeUnused5", doit = RemoveUnused2.remove} ::
   {name = "zone", doit = Zone.zone} ::
   nil

val ssa2PassesMinimal =
   nil

val ssa2Passes : pass list ref = ref ssa2PassesDefault

local
   type passGen = string -> pass option

   fun mkSimplePassGen (name, doit): passGen =
      let val count = Counter.new 1
      in fn s => if s = name
		    then SOME {name = concat [name, "#",
					      Int.toString (Counter.next count)],
			       doit = doit}
		    else NONE
      end


   val passGens = 
      List.map([("deepFlatten", DeepFlatten.flatten),
		("dropProfile", S.dropProfile),
		("refFlatten", RefFlatten.flatten),
		("removeUnused", RemoveUnused2.remove), 
		("zone", Zone.zone),
		("eliminateDeadBlocks",S.eliminateDeadBlocks),
		("reverseFunctions",S.reverseFunctions),
		("shrink", S.shrink)],
	       mkSimplePassGen)

   fun ssa2PassesSetCustom s =
      Exn.withEscape
      (fn esc =>
       (let val ss = String.split (s, #":")
	in 
	   ssa2Passes := 
	   List.map(ss, fn s =>
		    case (List.peekMap (passGens, fn gen => gen s)) of
		       NONE => esc (Result.No s)
		     | SOME pass => pass)
	   ; Control.ssa2Passes := ss
	   ; Result.Yes ()
	end))

   datatype t = datatype Control.optimizationPasses
   fun ssa2PassesSet opt =
      case opt of
	 OptPassesDefault => (ssa2Passes := ssa2PassesDefault
			      ; Control.ssa2Passes := ["default"]
			      ; Result.Yes ())
       | OptPassesMinimal => (ssa2Passes := ssa2PassesMinimal
			      ; Control.ssa2Passes := ["minimal"]
			      ; Result.Yes ())
       | OptPassesCustom s => ssa2PassesSetCustom s
in
   val _ = Control.ssa2PassesSet := ssa2PassesSet
   val _ = List.push (Control.optimizationPassesSet, ("ssa2", ssa2PassesSet))
end

fun stats p = Control.message (Control.Detail, fn () => Program.layoutStats p)

fun simplify p =
   let
      fun simplify' n p =
	 let
	    val mkSuffix = if n = 0
			     then fn s => s
			     else let val n' = Int.toString n
				  in fn s => concat [n',".",s]
				  end
	 in
	    if n = !Control.loopPasses
	       then p
	    else simplify' 
	         (n + 1)
		 (List.fold
	          (!ssa2Passes, p, fn ({name, doit}, p) =>
		   if List.exists (!Control.dropPasses, fn re =>
				   Regexp.Compiled.matchesAll (re, name))
		      then p
		   else
		     let
		       val _ =
			  let open Control
			  in maybeSaveToFile
			     ({name = name, suffix = mkSuffix "pre.ssa2"},
			      Control.No, p, Control.Layouts Program.layouts)
			  end
		       val p =
			  Control.passTypeCheck
			  {name = name,
			   suffix = mkSuffix "post.ssa2",
			   style = Control.No,
			   thunk = fn () => doit p,
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

val simplify = fn p => let
			 (* Always want to type check the initial and final SSA 
			  * programs, even if type checking is turned off, just
			  * to catch bugs.
			  *)
			 val _ = typeCheck p
			 val p = simplify p
			 val p =
			    if !Control.profile <> Control.ProfileNone
			       andalso !Control.profileIL = Control.ProfileSSA2
			       then Program.profile p
			    else p
			 val _ = typeCheck p
		       in
			 p
		       end
end
