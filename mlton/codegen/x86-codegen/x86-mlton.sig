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
    sharing x86.Label = Machine.Label

    val wordAlign : int -> int

    type transInfo = {addData : x86.Assembly.t list -> unit,
		      frameLayouts: x86.Label.t ->
		                    {size: int,
				     frameLayoutsIndex: int} option,
		      live: x86.Label.t -> x86.Operand.t list,
		      liveInfo: x86Liveness.LiveInfo.t}

    (* bug, runtime and primitive Assembly sequences. *)
    val creturn : {prim : Machine.Prim.t,
		   label : x86.Label.t, 
		   dst : (x86.Operand.t * x86.Size.t) option,
		   transInfo : transInfo} 
                  -> x86.Block.t' AppendList.t
    val runtimereturn : {prim : Machine.Prim.t,
			 label : x86.Label.t, 
			 transInfo : transInfo}
                        -> x86.Block.t' AppendList.t
    val prim : {prim : Machine.Prim.t,
		args : (x86.Operand.t * x86.Size.t) vector,
		dst : (x86.Operand.t * x86.Size.t) option,
		transInfo : transInfo}
               -> x86.Block.t' AppendList.t
    val arith : {prim : Machine.Prim.t,
		 args : (x86.Operand.t * x86.Size.t) vector,
		 dst : (x86.Operand.t * x86.Size.t),
		 overflow : x86.Label.t,
		 success : x86.Label.t,
		 transInfo : transInfo}
                -> x86.Block.t' AppendList.t
    val bug : {transInfo: transInfo} -> x86.Block.t' AppendList.t
    val ccall : {prim : Machine.Prim.t,
		 args : (x86.Operand.t * x86.Size.t) vector,
		 return : x86.Label.t,
		 dstsize : x86.Size.t option,
		 transInfo : transInfo}
                -> x86.Block.t' AppendList.t
    val runtimecall : {prim : Machine.Prim.t,
		       args : (x86.Operand.t * x86.Size.t) vector,
		       return : x86.Label.t,
		       transInfo : transInfo}
                      -> x86.Block.t' AppendList.t
  end
