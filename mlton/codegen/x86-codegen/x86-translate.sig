(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature X86_TRANSLATE_STRUCTS =
  sig
    structure x86: X86_PSEUDO
    structure x86MLton : X86_MLTON
    sharing x86 = x86MLton.x86
  end

signature X86_TRANSLATE =
  sig
    include X86_TRANSLATE_STRUCTS

    val translateChunk : {chunk: x86MLton.MachineOutput.Chunk.t} ->
                         {chunk: x86.Chunk.t,
			  liveInfo: {get: x86.Label.t 
				          -> x86.MemLoc.t list,
				     set: x86.Label.t * 
				          x86.MemLoc.t list
					  -> unit}}

    val translateProgram : {program: x86MLton.MachineOutput.Program.t} ->
                           {chunks: x86.Chunk.t list,
			    liveInfo: {get: x86.Label.t 
				            -> x86.MemLoc.t list,
				       set: x86.Label.t * 
				            x86.MemLoc.t list
				            -> unit}}

    val translateChunk_totals : unit -> unit
    val translateProgram_totals : unit -> unit
  end
