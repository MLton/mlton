(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature ELABORATE_ENV_STRUCTS =
   sig
      structure Ast: AST
      structure CoreML: CORE_ML
      structure TypeEnv: TYPE_ENV
      sharing Ast.Record = CoreML.Record
      sharing Ast.SortedRecord = CoreML.SortedRecord
      sharing CoreML.Atoms = TypeEnv.Atoms
      sharing CoreML.Type = TypeEnv.Type
   end

signature ELABORATE_ENV =
   sig
      include ELABORATE_ENV_STRUCTS

      structure Decs: DECS
      sharing CoreML = Decs.CoreML

      structure Type:
	 sig
	    type t
	 end
      sharing type Type.t = TypeEnv.Type.t
      structure Scheme:
	 sig
	    type t
	 end
      sharing type Scheme.t = TypeEnv.Scheme.t
      (* The value of a vid.  This is used to distinguish between vids whose
       * status cannot be determined at parse time.
       *)
      structure Vid:
	 sig
	    datatype t =
	       Con of CoreML.Con.t
	     | ConAsVar of CoreML.Con.t
	     | Exn of CoreML.Con.t
	     | Overload of (CoreML.Var.t * TypeEnv.Type.t) vector
	     | Var of CoreML.Var.t

	    val layout: t -> Layout.t
	 end
      structure TypeStr:
	 sig
	    structure Kind: TYCON_KIND
	    type t

	    val abs: t -> t
	    val apply: t * TypeEnv.Type.t vector -> TypeEnv.Type.t
	    val cons: t -> {con: CoreML.Con.t,
			    name: Ast.Con.t,
			    scheme: Scheme.t} vector
	    val data:
	       CoreML.Tycon.t * Kind.t
	       * {con: CoreML.Con.t,
		  name: Ast.Con.t,
		  scheme: Scheme.t} vector -> t
	    val def: Scheme.t * Kind.t -> t
	    val kind: t -> Kind.t
	    val tycon: CoreML.Tycon.t * Kind.t -> t
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
	    (* ffi represents MLtonFFI, which is built by the basis library
	     * and is set in compile.sml after processing the basis.
	     *)
	    val ffi: t option ref
	 end
      structure FunctorClosure:
	 sig
	    type t

	    val apply:
	       t * Structure.t * string list * Region.t -> Decs.t * Structure.t
	 end

      type t

      (* Remove unnecessary entries. *)
      val clean: t -> unit
      val empty: unit -> t
      val extendCon: t * Ast.Con.t * CoreML.Con.t * Scheme.t -> unit
      val extendExn: t * Ast.Con.t * CoreML.Con.t * Scheme.t -> unit
      val extendFctid: t * Ast.Fctid.t * FunctorClosure.t -> unit
      val extendFix: t * Ast.Vid.t * Ast.Fixity.t -> unit
      val extendSigid: t * Ast.Sigid.t * Interface.t -> unit
      val extendStrid: t * Ast.Strid.t * Structure.t -> unit
      val extendTycon: t * Ast.Tycon.t * TypeStr.t -> unit
      val extendVar: t * Ast.Var.t * CoreML.Var.t * Scheme.t -> unit
      val extendOverload:
	 t * Ast.Var.t * (CoreML.Var.t * TypeEnv.Type.t) vector * Scheme.t
	 -> unit
      val functorClosure:
	 t * Interface.t * (Structure.t * string list -> Decs.t * Structure.t)
	 -> FunctorClosure.t
      val layout: t -> Layout.t
      val layoutPretty: t -> Layout.t
      val layoutUsed: t -> Layout.t
      val localCore: t * (unit -> 'a) * ('a -> 'b) -> 'b
      val localModule: t * (unit -> 'a) * ('a -> 'b) -> 'b
      val localTop: t * (unit -> 'a) -> ('a * ((unit -> 'b) -> 'b))
      val lookupFctid: t * Ast.Fctid.t -> FunctorClosure.t
      val lookupLongcon: t * Ast.Longcon.t -> CoreML.Con.t * Scheme.t
      val lookupLongstrid: t * Ast.Longstrid.t -> Structure.t
      val lookupLongtycon: t * Ast.Longtycon.t -> TypeStr.t
      val lookupLongvar: t * Ast.Longvar.t -> CoreML.Var.t * Scheme.t
      val lookupLongvid: t * Ast.Longvid.t -> Vid.t * Scheme.t
      val lookupSigid: t * Ast.Sigid.t -> Interface.t
      val makeInterfaceMaker: t -> InterfaceMaker.t
      val makeStructure: t * (unit -> 'a) -> 'a * Structure.t
      (* openStructure (E, S) opens S in the environment E. *) 
      val openStructure: t * Structure.t -> unit
      val peekFix: t * Ast.Vid.t -> Ast.Fixity.t option
      val peekLongcon: t * Ast.Longcon.t -> (CoreML.Con.t * Scheme.t) option
      val peekLongtycon: t * Ast.Longtycon.t -> TypeStr.t option
      (* scope f evaluates f () in a new scope so that extensions that occur
       * during f () are forgotten afterwards.
       * scope works for infixes, types, values, and structures
       *)
      val scope: t * (unit -> 'a) -> 'a
      (* like scope, but works for signatures and functors as well *)
      val scopeAll: t * (unit -> 'a) -> 'a
      val sizeMessage: t -> Layout.t
   end

