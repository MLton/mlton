(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature ELABORATE_ENV_STRUCTS =
   sig
      structure Ast: AST
      structure CoreML: CORE_ML
      structure Decs: DECS
      sharing Ast = CoreML.Ast = Decs.Ast
      sharing CoreML = Decs.CoreML
   end

signature ELABORATE_ENV =
   sig
      include ELABORATE_ENV_STRUCTS

      (* The value of a vid.  This is used to distinguish between vids whose
       * status cannot be determined at parse time.
       *)
      structure Vid:
	 sig
	    datatype t =
	       Var of CoreML.Var.t
	     | ConAsVar of CoreML.Con.t (* a constructor, but it has status
					 * of a variable.
					 *)
	     | Con of CoreML.Con.t
	     | Exn of CoreML.Con.t
	     | Prim of CoreML.Prim.t

	    val deVar: t -> CoreML.Var.t option
	    val deCon: t -> CoreML.Con.t option
	    val layout: t -> Layout.t
	 end
      structure TypeStr:
	 sig
	    type t

	    val abs: t -> t
	    val apply: t * CoreML.Type.t vector -> CoreML.Type.t
	    val cons: t -> {name: Ast.Con.t,
			    con: CoreML.Con.t} vector
	    val data: CoreML.Tycon.t * {name: Ast.Con.t,
					con: CoreML.Con.t} vector -> t
	    val def: CoreML.Scheme.t -> t
	    val tycon: CoreML.Tycon.t -> t
	 end
      structure Interface:
	 sig
	    type t
	 end
      structure InterfaceMaker:
	 sig
	    type t

	    val addVar: t * Ast.Var.t -> unit
	    val addExcon: t * Ast.Con.t -> unit
	    val addTycon: t * Ast.Tycon.t * Ast.Con.t vector -> unit
	    val addStrid: t * Ast.Strid.t * Interface.t -> unit
	    val includeInterface: t * Interface.t -> unit
	    val lookupLongtycon: t * Ast.Longtycon.t -> Ast.Con.t vector
	    val makeInterface: t * (unit -> 'a) -> 'a * Interface.t
	 end
      structure Structure:
	 sig
	    type t
	       
	    (* cut keeps only those bindings in the structure that also appear
	     * in the interface.  It proceeds recursively on substructures.
	     *)
	    val cut: {str: t,
		      interface: Interface.t,
		      opaque: bool,
		      region: Region.t} -> t
	 end
      structure FunctorClosure:
	 sig
	    type t

	    val apply: t * Structure.t * Region.t -> Decs.t * Structure.t
	 end

      type t

      val addEquals: t -> unit
      (* Remove unnecessary entries. *)
      val clean: t -> unit
      val empty: unit -> t
      val extendCon: t * Ast.Con.t * CoreML.Con.t -> unit
      val extendExn: t * Ast.Con.t * CoreML.Con.t -> unit
      val extendFctid: t * Ast.Fctid.t * FunctorClosure.t -> unit
      val extendFix: t * Ast.Vid.t * Ast.Fixity.t -> unit
      val extendSigid: t * Ast.Sigid.t * Interface.t -> unit
      val extendStrid: t * Ast.Strid.t * Structure.t -> unit
      val extendTycon: t * Ast.Tycon.t * TypeStr.t -> unit
      val extendVar: t * Ast.Var.t * CoreML.Var.t -> unit
      val functorClosure:
	 t * Interface.t * (Structure.t -> Decs.t * Structure.t)
	 -> FunctorClosure.t
      val layout: t -> Layout.t
      val layoutPretty: t -> Layout.t
      val localCore: t * (unit -> 'a) * (unit -> 'b) -> 'a * 'b
      val localModule: t * (unit -> 'a) * (unit -> 'b) -> 'a * 'b
      val localTop: t * (unit -> 'a) * (unit -> 'b) -> 'a * 'b
      val lookupFctid: t * Ast.Fctid.t -> FunctorClosure.t
      val lookupLongcon: t * Ast.Longcon.t -> CoreML.Con.t
      val lookupLongstrid: t * Ast.Longstrid.t -> Structure.t
      val lookupLongtycon: t * Ast.Longtycon.t -> TypeStr.t
      val lookupLongvar: t * Ast.Longvar.t -> CoreML.Var.t
      val lookupLongvid: t * Ast.Longvid.t -> Vid.t
      val lookupSigid: t * Ast.Sigid.t -> Interface.t
      val makeInterfaceMaker: t -> InterfaceMaker.t
      val makeStructure: t * (unit -> 'a) -> 'a * Structure.t
      (* openStructure (E, S) opens S in the environment E. *) 
      val openStructure: t * Structure.t -> unit
      val peekFix: t * Ast.Vid.t -> Ast.Fixity.t option
      val peekLongcon: t * Ast.Longcon.t -> CoreML.Con.t option
      val peekLongtycon: t * Ast.Longtycon.t -> TypeStr.t option
      (* scope f evaluates f () in a new scope so that extensions that occur
       * during f () are forgotten afterwards.
       *)
      val scope: t * (unit -> 'a) -> 'a
      val sizeMessage: t -> Layout.t
   end

