(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor ElaborateControls (S: ELABORATE_CONTROLS_STRUCTS): ELABORATE_CONTROLS = 
struct
   open S

   val allowConstant : bool ref = ref false
   val allowExport : bool ref = ref true
   val allowImport : bool ref = ref true
   val allowOverload : bool ref = ref false
   val allowPrim : bool ref = ref false
   val allowRebindEquals : bool ref = ref false
   val deadCode : bool ref = ref false
   val forceUsed : int ref = ref 0
   val lookupConstant : (string * ConstType.t -> CoreML.Const.t) ref = 
      ref (fn _ => Error.bug "lookupConstant not set")
   val sequenceUnit : bool ref = ref false
   val warnMatch : bool ref = ref false
   val warnUnused : bool ref = ref false

   local
      fun make' (r : 'a ref, def: unit -> 'a): unit -> unit =
	 let 
	    val old = !r
	 in 
	    r := def ()
	    ; fn () => r := old
	 end
      fun make (r : 'a ref, def: 'a): unit -> unit =
	 make' (r, fn () => def)
   in
      fun withDefault f =
	 let
	    val restore =
	       (make (allowConstant, false)) o
	       (make (allowExport, true)) o
	       (make (allowImport, true)) o
	       (make (allowOverload, false)) o
	       (make (allowPrim, true)) o
	       (make (allowRebindEquals, false)) o
	       (make (deadCode, false)) o
	       (make (forceUsed, 0)) o
	       (make' (sequenceUnit, fn () => 
		       !Control.sequenceUnitAnn 
		       andalso !Control.sequenceUnitDef)) o
	       (make' (warnMatch, fn () => 
		       !Control.warnMatchAnn 
		       andalso !Control.warnMatchDef)) o
	       (make' (warnUnused, fn () => 
		       !Control.warnUnusedAnn 
		       andalso !Control.warnUnusedDef)) o
	       (fn () => ())
	 in
	    DynamicWind.wind (f, restore)
	 end 
   end

   fun withAnns (anns, f) =
      let
	 val restore =
	    List.fold
	    (anns, fn () => (), fn (ann, restore) =>
	     let
		fun warn () =
		   if !Control.warnAnn
		      then let open Layout
			   in
			      Control.warning
			      (Ast.Ann.region ann,
			       seq [str "unrecognized annotation: ",
				    Ast.Ann.layout ann],
			       empty)
			   end
		      else ()

		fun setCtrl'' (enabled, r, f) =
		   if enabled
		      then let 
			      val old = !r
			      val new = f old
			   in
			      r := new
			      ; (fn () => r := old) o restore
			   end
		      else restore
		fun setCtrl' (r, f) = setCtrl'' (true, r, f)
		fun setCtrl (r, v) =
		   setCtrl' (r, fn _ => v)

		fun setBool'' (enabled, r, b) =
		   case Bool.fromString b of
		      NONE => (warn (); restore)
		    | SOME b => setCtrl'' (enabled, r, fn _ => b)
		fun setBool (r, b) = setBool'' (true, r, b)
		fun incInt r =
		   setCtrl' (r, fn i => i + 1)
	     in
		case Ast.Ann.node ann of
		   Ast.Ann.Ann ["allowConstant", b] =>
		      setBool (allowConstant, b)
		 | Ast.Ann.Ann ["allowExport", b] =>
		      setBool (allowExport, b)
		 | Ast.Ann.Ann ["allowImport", b] =>
		      setBool (allowImport, b)
		 | Ast.Ann.Ann ["allowOverload", b] =>
		      setBool (allowOverload, b)
		 | Ast.Ann.Ann ["allowPrim", b] =>
		      setBool (allowPrim, b)
		 | Ast.Ann.Ann ["allowRebindEquals", b] =>
		      setBool (allowRebindEquals, b)
		 | Ast.Ann.Ann ["deadCode", b] =>
		      setBool'' (!Control.deadCodeAnn, deadCode, b)
		 | Ast.Ann.Ann ["forceUsed"] =>
		      incInt forceUsed
		 | Ast.Ann.Ann ["sequenceUnit", b] =>
		      setBool'' (!Control.sequenceUnitAnn, sequenceUnit, b)
		 | Ast.Ann.Ann ["warnMatch", b] =>
		      setBool'' (!Control.warnMatchAnn, warnMatch, b)
		 | Ast.Ann.Ann ["warnUnused", b] =>
		      setBool'' (!Control.warnUnusedAnn, warnUnused, b)
		 | _ => (warn (); restore)
	     end)
		   
      in
	 DynamicWind.wind (f, restore)
      end
      
end
