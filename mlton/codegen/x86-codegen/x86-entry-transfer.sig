type int = Int.t
type word = Word.t

signature X86_ENTRY_TRANSFER_STRUCTS =
  sig
    structure x86 : X86
  end

signature X86_ENTRY_TRANSFER =
  sig
    include X86_ENTRY_TRANSFER_STRUCTS

    val verifyEntryTransfer : {chunk: x86.Chunk.t} -> bool
    val verifyEntryTransfer_msg : unit -> unit
  end
