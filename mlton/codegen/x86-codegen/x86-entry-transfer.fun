(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor x86EntryTransfer(S: X86_ENTRY_TRANSFER_STRUCTS) : X86_ENTRY_TRANSFER =
struct
  open S
  open x86

  val tracer = x86.tracer

  fun verifyEntryTransfer {chunk as Chunk.T {blocks, ...}}
    = let
	val info as {get : Label.t -> Block.t option,
		     set,
		     destroy}
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
	fun isCReturn l = case get l
			    of SOME (Block.T {entry = Entry.CReturn _, ...}) => true
			     | _ => false
      in
	List.forall
	(blocks,
	 fn block as Block.T {entry, transfer, ...}
	  => (case transfer
		of Transfer.Goto {target, ...}
		 => isJump target
		 | Transfer.Iff {truee, falsee, ...}
		 => isJump truee andalso isJump falsee
		 | Transfer.Switch {cases, default, ...}
	         => isJump default andalso
	            Transfer.Cases.forall(cases, isJump)
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
	         | Transfer.CCall {return, ...} =>
		      (case return of
			  NONE => true
			| SOME l => isCReturn l)))
	before destroy ()
      end

  val (verifyEntryTranfer, verifyEntryTransfer_msg)
    = tracer
      "verifyEntryTransfer"
      verifyEntryTransfer

end
