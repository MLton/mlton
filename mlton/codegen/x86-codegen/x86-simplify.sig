(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature X86_SIMPLIFY_STRUCTS =
  sig
    structure x86 : X86
  end

signature X86_SIMPLIFY =
  sig
    include X86_SIMPLIFY_STRUCTS

    val simplify : {chunk : x86.Chunk.t,
		    optimize : int,
		    block_pre : x86.Label.t -> x86.Assembly.t list option,
		    block_begin : x86.Assembly.t list,
		    block_end : x86.Assembly.t list,
		    block_fall : x86.Assembly.t list,
		    transferRegs : x86.Register.t list,
		    liveInfo : {get : x86.Label.t -> x86.MemLoc.t list,
				set : x86.Label.t * x86.MemLoc.t list
				      -> unit}}
                   -> x86.Assembly.t list list

    val simplify_totals : unit -> unit
  end







