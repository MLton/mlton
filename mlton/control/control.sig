(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature CONTROL =
   sig
      val instrumentSxml: bool ref
	 
      (* set all flags to their default values *)
      val defaults: unit -> unit

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

      val cardSizeLog2: int ref

      datatype chunk =
	 OneChunk
       | ChunkPerFunc
       | Coalesce of {limit: int}

      val chunk: chunk ref

      val deadCode: bool ref
	 
      (* Generate an executable with debugging info. *)
      val debug: bool ref

      val defines: string list ref

      (* whether the arithmetic primitives detect overflow *)
      val detectOverflow: bool ref

      (* List of optimization passes to skip. *)
      val dropPasses: Regexp.Compiled.t list ref

      (* stop after elaboration.  So, no need for the elaborator to generate
       * valid CoreML.
       *)
      val elaborateOnly: bool ref

      (* whether optimization passes should eliminate useless overflow tests *)
      val eliminateOverflow: bool ref

      val exportHeader: File.t option ref
	 
      val exnHistory: bool ref

      (* *)
      datatype gcCheck =
	 Limit
       | First
       | Every
      val gcCheck: gcCheck ref

      datatype handlers = Flow | Simple
      val handlers: handlers ref

      (* Indentation used in laying out ILs. *)
      val indentation: int ref
	 
      datatype inline =
	 NonRecursive of {product: int,
			  small: int}
       | Leaf of {size: int option}
       | LeafNoLoop of {size: int option}
      val inline: inline ref
      val layoutInline: inline -> Layout.t
      val setInlineSize: int -> unit

      (* The input file on the command line, minus path and extension *)
      val inputFile: File.t ref

      (* call count instrumentation *)
      val instrument: bool ref

      val keepDefUse: bool ref

      (* List of pass names to keep diagnostic info on. *)
      val keepDiagnostics: Regexp.Compiled.t list ref

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

      (* lib/mlton directory *)
      val libDir: Dir.t ref

      (* lib/mlton/target directory *)
      val libTargetDir: Dir.t ref

      datatype limitCheck =
	 (* per block *)
	 PerBlock
         (* decycle using extended basic blocks 
	  *)
       | ExtBasicBlocks
	 (* decycle using loop headers
	  *  - use full CFG
	  *  - use loop exits of non-allocating loops
	  *)
       | LoopHeaders of {fullCFG: bool,
			 loopExits: bool}

      val limitCheck: limitCheck ref

      (* Whether or not dynamic counts of limit checks are computed. *)
      val limitCheckCounts: bool ref

      (* Number of times to loop through optimization passes. *)
      val loopPasses: int ref
	 
      (* Should the mutator mark cards? *)
      val markCards: bool ref

      (* May the executable use @MLton load-world -- *)
      val mayLoadWorld: bool ref

      structure Native:
	 sig
	    (* whether or not to use native codegen *)
	    val native: bool ref

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
	       
	    (* live transfer cutoff distance *)
	    val cutoff: int ref 

	    (* whether or not to use live transfer in native codegen *)
	    val liveTransfer: int ref 

	    (* size of future list for register allocation *)
	    val future: int ref

	    (* whether or not to shuffle registers around C-calls *)
	    val shuffle: bool ref

	    (* whether or not to use strict IEEE floating-point in native codegen *)
	    val IEEEFP: bool ref

	    (* whether or not to split assembly file in native codegen *)
	    val split: int option ref
	 end

      (* Whether or not to use the new non-tail call return convention.
       *)
      val newReturn: bool ref

      (* Only duplicate big functions when
       * (size - small) * (number of occurrences - 1) <= product
       *)
      val polyvariance:
	 {
	  rounds: int,
	  small: int,
	  product: int
	 } option ref

      (* Insert profiling information. *)
      datatype profile = ProfileNone | ProfileAlloc | ProfileTime
      val profile: profile ref

      val profileBasis: bool ref

      datatype profileIL = ProfileSSA | ProfileSource
      val profileIL: profileIL ref

      val profileStack: bool ref

      datatype representation = Normal | Packed 
      val representation: representation ref

      val reserveEsp: bool option ref

      (* Array bounds checking. *)
      val safe: bool ref

      (* in (e1; e2), require e1: unit. *)
      val sequenceUnit: bool ref

      (* Show the basis library. *)
      val showBasis: File.t option ref
	 
      (* Show the basis library used. *)
      val showBasisUsed: File.t option ref

      (* Show def-use information. *)
      val showDefUse: File.t option ref
	 
      (* Should types be printed in ILs. *)
      val showTypes: bool ref

      (* SSA Passes *)
      val ssaPassesSet: (string -> string list Result.t) ref
      val ssaPasses: string list ref

      (* Force continuation formals to stack. *)
      val stackCont: bool ref 

      (* Generate a statically linked executable. *)
      val static: bool ref

      (* SXML Passes *)
      val sxmlPassesSet: (string -> string list Result.t) ref
      val sxmlPasses: string list ref

      datatype target =
	 Cross of string
       | Self
      val target: target ref

      datatype arch = datatype MLton.Platform.Arch.t
      val targetArch: arch ref

      val targetIsBigEndian: unit -> bool

      datatype os = datatype MLton.Platform.OS.t
      val targetOS: os ref

      val textIOBufSize: int ref
	 
      (* Type check ILs. *)
      val typeCheck: bool ref

      datatype typeError = Concise | Full
      val typeError: typeError ref
	 
      (* Should the basis library be prefixed onto the program. *)
      val useBasisLibrary: bool ref

      datatype verbosity =
	 Silent
       | Top
       | Pass
       | Detail
      val verbosity: verbosity ref

      (* version number *)
      val version: string

      val warnNonExhaustive: bool ref

      val warnRedundant: bool ref

      val warnUnused: bool ref

      (* XML Passes *)
      val xmlPassesSet: (string -> string list Result.t) ref
      val xmlPasses: string list ref

      (*------------------------------------*)
      (*             End Flags              *)
      (*------------------------------------*)

      (* Tracing and other informative messages.
       * Some take a verbosity argument that specifies the verbosity level at
       * which messages should be printed. 
       *)
      val message: verbosity * (unit -> Layout.t) -> unit
      val messageStr: verbosity * string -> unit
      val sizeMessage: string * 'a -> Layout.t
      val trace: verbosity * string -> ('a -> 'b) -> 'a -> 'b
      type traceAccum
      val traceAccum: verbosity * string -> (traceAccum * (unit -> unit))
      val traceAdd: traceAccum * string -> ('a -> 'b) -> 'a -> 'b
      val traceBatch: verbosity * string -> ('a -> 'b) -> 
                      (('a -> 'b) * (unit -> unit))
      val indent: unit -> unit
      val unindent: unit -> unit
      val getDepth: unit -> int

      (*------------------------------------*)
      (*          Error Reporting           *)
      (*------------------------------------*)
      val checkForErrors: string -> unit
      val error: Region.t * Layout.t * Layout.t -> unit
      val errorStr: Region.t * string -> unit
      (* abort compilation once this many errors reached *)
      val errorThreshhold: int ref
      val numErrors: int ref
      val warning: Region.t * Layout.t * Layout.t -> unit
	 
      (*------------------------------------*)
      (*          Compiler Passes           *)
      (*------------------------------------*)
      datatype style = No | Assembly | C | Dot | ML

      datatype 'a display =
	 NoDisplay
       | Layout of 'a -> Layout.t
       | Layouts of 'a * (Layout.t -> unit) -> unit

      val diagnostic: (unit -> Layout.t) -> unit
      val diagnostics: ((Layout.t -> unit) -> unit) -> unit
      val maybeSaveToFile:
	 {name: string, suffix: string} * style * 'a * 'a display -> unit
      val saveToFile:
	 {suffix: string} * style * 'a * 'a display -> unit
      val outputHeader: style * (Layout.t -> unit) -> unit
      val outputHeader': style * Out.t -> unit

      val pass: {name: string,
		 suffix: string,
		 style: style,
		 thunk: unit -> 'a,
		 display: 'a display} -> 'a
	 
      val passTypeCheck: {name: string,
			  suffix: string,
			  style: style,
			  thunk: unit -> 'a,
			  display: 'a display,
			  typeCheck: 'a -> unit} -> 'a
	 
      val passSimplify: {name: string,
			 suffix: string,
			 style: style,
			 thunk: unit -> 'a,
			 display: 'a display,
			 simplify: 'a -> 'a,
			 typeCheck: 'a -> unit} -> 'a
   end
