(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature CONTROL_FLAGS =
   sig
      (* set all flags to their default values *)
      val defaults: unit -> unit
	 
      val all : unit -> {name: string, 
			 value: string} list

      (*------------------------------------*)
      (*            Begin Flags             *)
      (*------------------------------------*)

      datatype align = Align4 | Align8
      val align: align ref

      val atMLtons: string vector ref

      val basisLibs: string list
      val basisLibrary: string ref

      (* build identifies the machine on which this MLton was built. *)
      val build: string

      datatype chunk =
	 OneChunk
       | ChunkPerFunc
       | Coalesce of {limit: int}

      val chunk: chunk ref

      datatype codegen =
	 Bytecode
       | CCodegen
       | Native

      val codegen: codegen ref

      val contifyIntoMain: bool ref

      (* Generate an executable with debugging info. *)
      val debug: bool ref

      (* List of pass names to keep diagnostic info on. *)
      val diagPasses: Regexp.Compiled.t list ref

      (* List of optimization passes to skip. *)
      val dropPasses: Regexp.Compiled.t list ref

      structure Elaborate:
	 sig
	    type ('args, 'st) t

	    val allowConstant: (bool,bool) t
	    val allowExport: (bool,bool) t
	    val allowImport: (bool,bool) t
	    val allowOverload: (bool,bool) t
	    val allowPrim: (bool,bool) t
	    val allowRebindEquals: (bool,bool) t
	    val allowSymbol: (bool,bool) t
	    val deadCode: (bool,bool) t
	    val forceUsed: (unit,bool) t
	    val ffiStr: (string,string option) t
	    (* in (e1; e2), require e1: unit. *)
	    val sequenceUnit: (bool,bool) t
	    val warnMatch: (bool,bool) t
	    val warnUnused: (bool,bool) t

	    val current: ('args, 'st) t -> 'st
	    val default: ('args, 'st) t -> 'st
	    val enabled: ('args, 'st) t -> bool
	    val expert: ('args, 'st) t -> bool
	    val name: ('args, 'st) t -> string

	    structure Id :
	       sig
		  type t
	       end
	    val equalsId: ('args, 'st) t * Id.t -> bool
	    val parseId: string -> Id.t option

	    structure Args :
	       sig
		  type t
		  val processAnn: t -> (unit -> unit)
	       end
	    val parseIdAndArgs: string -> (Id.t * Args.t) option

	    val processDefault: string -> bool
	    val processEnabled: string * bool -> bool

	    val withDef: (unit -> 'a) -> 'a
	    val snapshot: unit -> (unit -> 'a) -> 'a
	 end

      (* stop after elaboration.  So, no need for the elaborator to generate
       * valid CoreML.
       *)
      val elaborateOnly: bool ref

      val exportHeader: File.t option ref
	 
      val exnHistory: bool ref

      (* *)
      datatype gcCheck =
	 Limit
       | First
       | Every
      val gcCheck: gcCheck ref

      (* Indentation used in laying out ILs. *)
      val indentation: int ref
	 
      datatype inline =
	 NonRecursive of {product: int,
			  small: int}
       | Leaf of {size: int option}
       | LeafNoLoop of {size: int option}
      val inline: inline ref
      val setInlineSize: int -> unit

      val inlineIntoMain: bool ref

      (* The input file on the command line, minus path and extension *)
      val inputFile: File.t ref

      (* Keep dot files for whatever SSA files are produced. *)
      val keepDot: bool ref

      (* Save the Machine to a file. *)
      val keepMachine: bool ref
	 
      (* List of pass names to save the result of. *)
      val keepPasses: Regexp.Compiled.t list ref

      (* Save the RSSA to a file. *)
      val keepRSSA: bool ref
	 
      (* Save the SSA to a file. *)
      val keepSSA: bool ref
      (* Save the SSA2 to a file. *)
      val keepSSA2: bool ref

      (* For the codegen -- do labels for gcc and assembler need an extra leading
       * underscore.
       *)
      val labelsHaveExtra_: bool ref

      (* lib/mlton directory *)
      val libDir: Dir.t ref

      (* lib/mlton/target directory *)
      val libTargetDir: Dir.t ref

      (* Number of times to loop through optimization passes. *)
      val loopPasses: int ref
	 
      (* Should the mutator mark cards? *)
      val markCards: bool ref

      val maxFunctionSize: int ref

      structure Native:
	 sig
	    (* whether or not to use comments in native codegen *)
	    val commented: int ref

	    (* whether or not to track liveness of stack slots *)
	    val liveStack: bool ref 

	    (* level of optimization to use in native codegen *)
	    val optimize: int ref

	    (* whether or not to use move hoisting in native codegen *)
	    val moveHoist: bool ref
	       
	    (* whether or not to use copy propagation in native codegen *)
	    val copyProp: bool ref

	    (* Don't use copy propagation on blocks larger than this. *)
	    val copyPropCutoff: int ref
	       
	    (* live transfer cutoff distance *)
	    val cutoff: int ref 

	    (* whether or not to use live transfer in native codegen *)
	    val liveTransfer: int ref 

	    (* whether or not to shuffle registers around C-calls *)
	    val shuffle: bool ref

	    (* whether or not to use strict IEEE floating-point in native codegen *)
	    val IEEEFP: bool ref

	    (* whether or not to split assembly file in native codegen *)
	    val split: int option ref
	 end

      datatype optimizationPasses =
	 OptPassesCustom of string
       | OptPassesDefault
       | OptPassesMinimal
      val optimizationPassesSet: 
	 (string * (optimizationPasses -> unit Result.t)) list ref

      (* Only duplicate big functions when
       * (size - small) * (number of occurrences - 1) <= product
       *)
      val polyvariance:
	 {
	  rounds: int,
	  small: int,
	  product: int
	 } option ref

      (* List of pass names to keep profiling info on. *)
      val profPasses: Regexp.Compiled.t list ref

      (* Insert profiling information. *)
      datatype profile =
	 ProfileNone
       | ProfileAlloc
       | ProfileCallStack
       | ProfileCount
       | ProfileDrop
       | ProfileLabel
       | ProfileTime
      val profile: profile ref

      val profileBranch: bool ref

      val profileC: Regexp.Compiled.t list ref

      datatype profileIL = ProfileSSA | ProfileSSA2 | ProfileSource
      val profileIL: profileIL ref

      val profileInclExcl: (Regexp.Compiled.t * bool) list ref

      val profileRaise: bool ref

      val profileStack: bool ref

      (* Show the basis library. *)
      val showBasis: File.t option ref
	 
      (* Show def-use information. *)
      val showDefUse: File.t option ref
	 
      (* Should types be printed in ILs. *)
      val showTypes: bool ref

      (* SSA Passes *)
      val ssaPassesSet: (optimizationPasses -> unit Result.t) ref
      val ssaPasses: string list ref
      val ssa2PassesSet: (optimizationPasses -> unit Result.t) ref
      val ssa2Passes: string list ref

      (* SXML Passes *)
      val sxmlPassesSet: (optimizationPasses -> unit Result.t) ref
      val sxmlPasses: string list ref

      datatype target =
	 Cross of string
       | Self
      val target: target ref

      datatype arch = datatype MLton.Platform.Arch.t
      val targetArch: arch ref

      val setTargetBigEndian: bool -> unit
      val targetIsBigEndian: unit -> bool

      datatype os = datatype MLton.Platform.OS.t
      val targetOS: os ref

      (* Type check ILs. *)
      val typeCheck: bool ref

      datatype verbosity = 
	 Silent
       | Top
       | Pass
       | Detail
      val verbosity: verbosity ref

      (* version number *)
      val version: string

      val warnAnn: bool ref

      (* XML Passes *)
      val xmlPassesSet: (optimizationPasses -> unit Result.t) ref
      val xmlPasses: string list ref

      val zoneCutDepth: int ref

      (*------------------------------------*)
      (*             End Flags              *)
      (*------------------------------------*)
end
