(* Copyright (C) 2009-2012,2014-2017,2019-2021,2025 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CONTROL_FLAGS =
   sig
      (* set all flags to their default values *)
      val defaults: unit -> unit

      val all : unit -> {name: string, 
                         value: string} list

      val layout': {pre: string, suf: string} -> Layout.t
      val layout: unit -> Layout.t

      structure StrMap:
         sig
            type t
            val load: File.t -> t
            val lookup: t * string -> string
            val lookupIntInf: t * string -> IntInf.t
            val peek: t * string -> string option
         end

      (*------------------------------------*)
      (*            Begin Flags             *)
      (*------------------------------------*)

      datatype align = Align4 | Align8
      val align: align ref

      val atMLtons: string vector ref

      val bounceRssaLimit: int option ref
      val bounceRssaLiveCutoff: int option ref
      val bounceRssaLoopCutoff: int option ref
      val bounceRssaUsageCutoff: int option ref

      val buildConsts: StrMap.t Promise.t

      val chunkBatch: int ref

      structure Chunkify:
         sig
            datatype t = Coalesce of {limit: int}
                       | Func
                       | One
                       | Simple of {mainFns: bool,
                                    sccC: bool,
                                    sccR: bool,
                                    singC: bool,
                                    singR: bool}
            val toString: t -> string
            val fromString: string -> t option
         end
      val chunkify: Chunkify.t ref

      val chunkJumpTable: bool ref
      val chunkMayRToSelfOpt: bool ref
      val chunkMustRToOtherOpt: bool ref
      val chunkMustRToSelfOpt: bool ref
      val chunkMustRToSingOpt: bool ref
      val chunkTailCall: bool ref

      val closureConvertGlobalize: bool ref
      val closureConvertShrink: bool ref

      structure Codegen:
         sig
            datatype t =
               AMD64Codegen
             | CCodegen
             | LLVMCodegen
             | X86Codegen
            val all: t list
            val toString: t -> string
         end

      datatype codegen = datatype Codegen.t

      val codegen: Codegen.t ref

      (* whether or not to use comments in codegen *)
      val codegenComments: int ref

      (* whether or not to fuse `op` and `opCheckP` primitives in codegen *)
      val codegenFuseOpAndChk: bool ref

      val commandLineConsts: StrMap.t
      val setCommandLineConst: {name: string, value: string} -> unit

      val constPropAbsValLayoutDepth: int ref

      val contifyIntoMain: bool ref

      (* Generate an executable with debugging info. *)
      val debug: bool ref

      val defaultChar: string ref
      val defaultWideChar: string ref
      val defaultInt: string ref
      val defaultReal: string ref
      val defaultWord: string ref

      (* List of pass names to keep diagnostic info on. *)
      val diagPasses: Regexp.Compiled.t list ref

      (* List of optimization passes to disable/enable. *)
      val executePasses: (Regexp.Compiled.t * bool) list ref

      structure Elaborate:
         sig
            structure DiagEIW :
               sig
                  datatype t =
                     Error
                   | Ignore
                   | Warn
               end
            structure DiagDI :
               sig
                  datatype t =
                     Default
                   | Ignore
               end
            structure ResolveScope :
               sig
                  datatype t =
                     Dec
                   | Strdec
                   | Topdec
                   | Program
               end

            type ('args, 'st) t

            val document: {expert: bool} -> Layout.t

            val allowConstant: (bool,bool) t
            val allowFFI: (bool,bool) t
            val allowOverload: (bool,bool) t
            val allowPrim: (bool,bool) t
            val allowRedefineSpecialIds: (bool,bool) t
            val allowSpecifySpecialIds: (bool,bool) t
            val deadCode: (bool,bool) t
            val forceUsed: (unit,bool) t
            val ffiStr: (string,string option) t
            val nonexhaustiveBind: (DiagEIW.t,DiagEIW.t) t
            val nonexhaustiveExnBind: (DiagDI.t,DiagDI.t) t
            val redundantBind: (DiagEIW.t,DiagEIW.t) t
            val nonexhaustiveMatch: (DiagEIW.t,DiagEIW.t) t
            val nonexhaustiveExnMatch: (DiagDI.t,DiagDI.t) t
            val redundantMatch: (DiagEIW.t,DiagEIW.t) t
            val nonexhaustiveRaise: (DiagEIW.t,DiagEIW.t) t
            val nonexhaustiveExnRaise: (DiagDI.t,DiagDI.t) t
            val redundantRaise: (DiagEIW.t,DiagEIW.t) t
            val resolveScope: (ResolveScope.t,ResolveScope.t) t
            val sequenceNonUnit: (DiagEIW.t,DiagEIW.t) t
            val valrecConstr: (DiagEIW.t,DiagEIW.t) t
            val warnUnused: (bool,bool) t

            (* Successor ML *)
            val allowDoDecls: (bool,bool) t
            val allowExtendedNumConsts: (bool,bool) t
            val allowExtendedTextConsts: (bool,bool) t
            val allowLineComments: (bool,bool) t
            val allowOptBar: (bool,bool) t
            val allowOptSemicolon: (bool,bool) t
            val allowOrPats: (bool,bool) t
            val allowRecordPunExps: (bool,bool) t
            val allowSigWithtype: (bool,bool) t
            val allowVectorExps: (bool,bool) t
            val allowVectorPats: (bool,bool) t

            val current: ('args, 'st) t -> 'st
            val default: ('args, 'st) t -> 'st
            val enabled: ('args, 'st) t -> bool
            val expert: ('args, 'st) t -> bool
            val name: ('args, 'st) t -> string

            datatype 'a parseResult =
               Bad | Good of 'a | Other | Proxy of 'a list * {deprecated: bool}

            structure Id :
               sig
                  type t
                  val name: t -> string
               end
            val equalsId: ('args, 'st) t * Id.t -> bool
            val parseId: string -> Id.t parseResult

            structure Args :
               sig
                  type t
                  val processAnn: t -> (unit -> unit)
               end
            val parseIdAndArgs: string -> (Id.t * Args.t) parseResult

            val processDefault: string -> Id.t parseResult
            val processEnabled: string * bool -> Id.t parseResult

            val withDef: (unit -> 'a) -> 'a
            val snapshot: unit -> (unit -> 'a) -> 'a
         end

      val emitMain: bool ref

      val exportHeader: File.t option ref

      val exnHistory: bool ref

      val forceHandlesSignals: bool ref

      structure Format:
         sig
            datatype t =
               Archive
             | Executable
             | LibArchive
             | Library
            val all: t list
            val toString: t -> string
         end
      
      datatype format = datatype Format.t

      val format: Format.t ref

      (* *)
      datatype gcCheck =
         Limit
       | First
       | Every
      val gcCheck: gcCheck ref

      val globalizeArrays: bool ref

      val globalizeRefs: bool ref

      val globalizeSmallIntInf: bool ref

      val globalizeSmallType: int ref

      (* Indentation used in laying out ILs. *)
      val indentation: int ref

      val inlineIntoMain: bool ref

      val inlineLeafA: {loops: bool, repeat: bool, size: int option} ref
      val inlineLeafB: {loops: bool, repeat: bool, size: int option} ref

      val inlineNonRec: {small: int, product: int} ref

      (* The input file on the command line, minus path and extension. *)
      val inputFile: File.t ref

      (* Whether or not the elaborator keeps def-use information. *)
      val keepDefUse: bool ref
         
      (* Keep dot files for whatever SSA files are produced. *)
      val keepDot: bool ref

      (* List of pass names to save the input/output. *)
      val keepPasses: Regexp.Compiled.t list ref

      (* Save the AST to a file. *)
      val keepAST: bool ref
      (* Save the final CoreML to a file. *)
      val keepCoreML: bool ref
      (* Save the final Machine to a file. *)
      val keepMachine: bool ref
      (* Save the final RSSA to a file. *)
      val keepRSSA: bool ref
      (* Save the final SSA to a file. *)
      val keepSSA: bool ref
      (* Save the final SSA2 to a file. *)
      val keepSSA2: bool ref
      (* Save the final SXML to a file. *)
      val keepSXML: bool ref
      (* Save the final XML to a file. *)
      val keepXML: bool ref

      (* For the codegen -- do labels for gcc and assembler need an extra leading
       * underscore.
       *)
      val labelsHaveExtra_: bool ref

      (* lib/mlton directory *)
      val libDir: Dir.t ref

      (* lib/mlton/target directory *)
      val libTargetDir: Dir.t ref
      
      (* name of the output library *)
      val libname : string ref

      val limitCheckExpect: bool option ref

      structure LLVMAliasAnalysisMetaData:
         sig
            datatype t =
               None
             | Scope
             | TBAA of {gcstate: {offset: bool} option,
                        global: {cty: bool, index: bool} option,
                        heap: {cty: bool, kind: bool, offset: bool, tycon: bool} option,
                        other: bool,
                        stack: {offset: bool} option}
            val toString: t -> string
            val fromString: string -> t option
         end
      val llvmAAMD: LLVMAliasAnalysisMetaData.t ref

      val llvmCC10: bool ref

      (* Limit the code growth loop unrolling/unswitching will allow. *)
      val loopUnrollLimit: int ref
      val loopUnswitchLimit: int ref

      (* Should the mutator mark cards? *)
      val markCards: bool ref

      val maxFunctionSize: int ref

      val mlbPathVars: {var: string,
                        path: string} list ref
      val mlbPathMap: unit -> {var: string,
                               path: string} list

      structure Native:
         sig
            (* whether to eliminate redundant AL ops in native codegen *)
            val elimALRedundant: bool ref

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

            (* whether or not to use position-independent code in native codegen *)
            val pic: bool ref
         end

      val numExports: int ref

      val optFuel: int option ref

      val optFuelAvailAndUse: unit -> bool

      (* Control IL-specific optimization passes *)
      structure OptimizationPasses:
         sig
            val register: {il: string,
                           set: string -> unit Result.t} -> unit
            val set: {il: string, passes: string} -> unit Result.t
            val setAll: string -> unit Result.t
         end

      (* Only duplicate big functions when
       * (size - small) * (number of occurrences - 1) <= product
       *)
      val polyvariance:
         {
          hofo: bool,
          rounds: int,
          small: int,
          product: int
         } option ref

      structure PositionIndependentStyle:
         sig
            datatype t =
               NPI
             | PIC
             | PIE

            val ccOpts: t option -> string list
            val fromString: string -> t option
            val linkOpts: t option -> string list
            val llvm_llcOpts: t option * {targetDefault: t} -> string list
            val toString: t -> string
            val toSuffix: t option -> string
         end

      val positionIndependentStyle: PositionIndependentStyle.t option ref

      val preferAbsPaths: bool ref

      (* List of pass names to keep profiling info on. *)
      val profPasses: Regexp.Compiled.t list ref

      (* Insert profiling information. *)
      datatype profile =
         ProfileNone
       | ProfileAlloc
       | ProfileCallStack
       | ProfileCount
       | ProfileDrop
       | ProfileTime
      val profile: profile ref

      val profileBlock: bool ref

      val profileBranch: bool ref

      val profileC: Regexp.Compiled.t list ref

      datatype profileIL = ProfileSource | ProfileSSA | ProfileSSA2
      val profileIL: profileIL ref

      val profileInclExcl: (Regexp.Compiled.t * bool) list ref

      val profileRaise: bool ref

      val profileStack: bool ref

      val profileTailCallOpt: bool ref

      val profileVal: bool ref

      (* Show the basis library. *)
      val showBasis: File.t option ref

      val showBasisCompact: bool ref
      val showBasisDef: bool ref
      val showBasisFlat: bool ref

      (* Show def-use information. *)
      val showDefUse: File.t option ref

      (* Should types be printed in ILs. *)
      val showTypes: bool ref

      structure SignalCheck:
         sig
            datatype t =
               Always
             | IfHandlesSignals
         end
      val signalCheck: SignalCheck.t ref
      val signalCheckAtLimitCheck: bool ref
      val signalCheckExpect: bool option ref

      datatype splitTypesBool =
         Never
       | Smart (* split only when smaller than two, default *)
       | Always
      val splitTypesBool: splitTypesBool ref

      val stackCheckExpect: bool option ref

      (* List of pass names to stop at. *)
      val stopPasses: Regexp.Compiled.t list ref

      datatype target =
         Cross of string
       | Self
      val target: target ref

      structure Target:
         sig
            datatype arch = datatype MLton.Platform.Arch.t
            val arch: arch ref

            val bigEndian: unit -> bool

            datatype os = datatype MLton.Platform.OS.t
            val os: os ref

            val consts: StrMap.t Promise.t

            structure Size:
               sig
                  val cint: unit -> Bits.t
                  val cpointer: unit -> Bits.t
                  val cptrdiff: unit -> Bits.t
                  val csize: unit -> Bits.t
                  val header: unit -> Bits.t
                  val mplimb: unit -> Bits.t
                  val normalMetaData: unit -> Bits.t
                  val objptr: unit -> Bits.t
                  val seqIndex: unit -> Bits.t
                  val sequenceMetaData: unit -> Bits.t
               end
         end

      structure Verbosity:
         sig
            datatype t =
               Silent
             | Top
             | Pass
             | Detail
            val < : t * t -> bool
            val <= : t * t -> bool
         end
      datatype verbosity = datatype Verbosity.t
      val verbosity: verbosity ref

      val warnAnn: bool ref

      val warnDeprecated: bool ref

      val zoneCutDepth: int ref

      (*------------------------------------*)
      (*             End Flags              *)
      (*------------------------------------*)
end
