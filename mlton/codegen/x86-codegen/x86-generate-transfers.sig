
type int = Int.t
type word = Word.t

signature X86_GENERATE_TRANSFERS_STRUCTS =
  sig
    structure x86 : X86
    structure x86MLton : X86_MLTON
    sharing x86 = x86MLton.x86
    structure x86Liveness : X86_LIVENESS
    sharing x86 = x86Liveness.x86
    sharing x86.MemLocSet = x86Liveness.LiveSet
    structure x86JumpInfo : X86_JUMP_INFO
    sharing x86 = x86JumpInfo.x86
    structure x86LoopInfo : X86_LOOP_INFO
    sharing x86 = x86LoopInfo.x86
    structure x86EntryTransfer : X86_ENTRY_TRANSFER
    sharing x86 = x86EntryTransfer.x86
  end

signature X86_GENERATE_TRANSFERS =
  sig
    include X86_GENERATE_TRANSFERS_STRUCTS

    val generateTransfers : {chunk : x86.Chunk.t,
			     optimize : int,
			     liveInfo : x86Liveness.LiveInfo.t,
			     jumpInfo : x86JumpInfo.t} -> x86.Assembly.t list list
    val generateTransfers_totals : unit -> unit
  end