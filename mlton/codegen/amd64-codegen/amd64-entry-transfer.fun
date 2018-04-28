(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64EntryTransfer(S: AMD64_ENTRY_TRANSFER_STRUCTS) : AMD64_ENTRY_TRANSFER =
struct
  open S
  open amd64

  val tracer = amd64.tracer

  fun verifyEntryTransfer {chunk = Chunk.T {blocks, ...}}
    = let
        val {get : Label.t -> Block.t option, 
             set, destroy}
          = Property.destGetSetOnce(Label.plist,
                                    Property.initConst NONE)

        val _
          = List.foreach
            (blocks,
             fn block as Block.T {entry,...}
              => set(Entry.label entry, SOME block))

        fun isJump l = case get l
                         of SOME (Block.T {entry = Entry.Jump _, ...}) => true
                          | _ => false
        fun isFunc l = case get l
                         of SOME (Block.T {entry = Entry.Func _, ...}) => true
                          | NONE => true
                          | _ => false
        fun isCont l = case get l
                         of SOME (Block.T {entry = Entry.Cont _, ...}) => true
                          | _ => false
        fun isHandler l = case get l
                            of SOME (Block.T {entry = Entry.Handler _, ...}) => true
                             | _ => false
        fun isCReturn l f = case get l
                              of SOME (Block.T {entry = Entry.CReturn {func, ...}, ...})
                               => CFunction.equals (f, func)
                               | _ => false
        val b = List.forall
                (blocks,
                 fn Block.T {transfer, ...}
                  => (case transfer
                        of Transfer.Goto {target, ...}
                         => isJump target
                         | Transfer.Iff {truee, falsee, ...}
                         => isJump truee andalso isJump falsee
                         | Transfer.Switch {cases, default, ...}
                         => isJump default andalso
                            Transfer.Cases.forall(cases, isJump o #2)
                         | Transfer.Tail {target, ...}
                         => isFunc target
                         | Transfer.NonTail {target, return, handler, ...}
                         => isFunc target andalso
                            isCont return andalso
                            (case handler
                               of SOME handler => isHandler handler
                                | NONE => true)
                         | Transfer.Return {...} => true
                         | Transfer.Raise {...} => true
                         | Transfer.CCall {return, func, ...} 
                         => (case return
                               of NONE => true
                                | SOME l => isCReturn l func)))
        val _ = destroy ()
        val _ = if b then ()
                  else List.foreach(blocks, Block.printBlock)
      in
        b
      end

  val (verifyEntryTransfer, verifyEntryTransfer_msg)
    = tracer
      "verifyEntryTransfer"
      verifyEntryTransfer

end
