(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature X86_MLTON_STRUCTS =
  sig
    structure x86MLtonBasic : X86_MLTON_BASIC
    structure x86Liveness : X86_LIVENESS
    sharing x86MLtonBasic.x86 = x86Liveness.x86
  end

signature X86_MLTON =
  sig
    include X86_MLTON_STRUCTS
    include X86_MLTON_BASIC
    sharing x86 = x86MLtonBasic.x86
    sharing x86 = x86Liveness.x86
    sharing x86.Label = MachineOutput.Label

    val wordAlign : int -> int
    (* bug, runtime and primitive Assembly sequences. *)
    val bug : {liveInfo: x86Liveness.LiveInfo.t} -> x86.Block.t' AppendList.t
    val invokeRuntime : {prim: x86.Prim.t, 
			 args : (x86.Operand.t * x86.Size.t) list, 
			 info : {frameSize: int, 
				 live: x86.Operand.t list,
				 return: x86.Label.t},
			 addData : x86.Assembly.t list -> unit,
			 frameLayouts : x86.Label.t
                                        -> {size: int, frameLayoutsIndex: int} option,
			 liveInfo : x86Liveness.LiveInfo.t}
                        -> x86.Block.t' AppendList.t

    structure PrimInfo :
      sig
	datatype t
	  = None
	  | Runtime of {frameSize: int, 
			live: x86.Operand.t list,
			return: x86.Label.t}
	  | Normal of x86.Operand.t list
      end

    val applyPrim : {prim : MachineOutput.Prim.t,
		     args : (x86.Operand.t * x86.Size.t) vector,
		     dst : (x86.Operand.t * x86.Size.t) option,
		     pinfo : PrimInfo.t,	
		     addData : x86.Assembly.t list -> unit,
		     frameLayouts : x86.Label.t
                                    -> {size: int, frameLayoutsIndex: int} option,
		     liveInfo : x86Liveness.LiveInfo.t}
                    -> x86.Block.t' AppendList.t
    val arith : {prim : MachineOutput.Prim.t,
		 args : (x86.Operand.t * x86.Size.t) vector,
		 dst : (x86.Operand.t * x86.Size.t),
		 overflow : x86.Label.t,
		 success : x86.Label.t,
		 addData : x86.Assembly.t list -> unit,
		 frameLayouts : x86.Label.t
		                -> {size: int, frameLayoutsIndex: int} option,
		 liveInfo : x86Liveness.LiveInfo.t}
                -> x86.Block.t' AppendList.t
  end
