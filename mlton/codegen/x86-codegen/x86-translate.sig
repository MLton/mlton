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
    structure x86Liveness : X86_LIVENESS
    sharing x86 = x86Liveness.x86
    sharing x86MLton.x86Liveness = x86Liveness 
  end

signature X86_TRANSLATE =
  sig
    include X86_TRANSLATE_STRUCTS

    val translateChunk : {chunk: x86MLton.MachineOutput.Chunk.t,
			  frameLayouts: x86MLton.MachineOutput.Label.t ->
			                {size: int, frameLayoutsIndex: int} option,
			  liveInfo: x86Liveness.LiveInfo.t} ->
                         {chunk: x86.Chunk.t}

    val translateProgram : {program: x86MLton.MachineOutput.Program.t,
			    frameLayouts: x86MLton.MachineOutput.Label.t ->
			                  {size: int, frameLayoutsIndex: int} option,
			    liveInfo: x86Liveness.LiveInfo.t} ->
                           {chunks: x86.Chunk.t list}

    val translateChunk_totals : unit -> unit
    val translateProgram_totals : unit -> unit
  end
