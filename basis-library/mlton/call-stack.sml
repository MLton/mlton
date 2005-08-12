(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonCallStack =
   struct
      open Primitive.CallStack

      val gcState = Primitive.GCState.gcState
      structure Pointer = MLtonPointer
	 
      val current: unit -> t =
	 fn () =>
	 if not keep
	    then T (Array.array (0, 0))
	 else
	    let
	       val a = Array.array (numStackFrames gcState, ~1)
	       val () = callStack (gcState, a)
	    in
	       T a
	    end

      val toStrings: t -> string list =
	 fn T a =>
	 if not keep
	    then []
	 else
	    let
	       val skip = Array.length a - 2
	    in
	       Array.foldri
	       (fn (i, frameIndex, ac) =>
		if i >= skip
		   then ac
		else
		   let
		      val p = frameIndexSourceSeq (gcState, frameIndex)
		      val max = Pointer.getInt32 (p, 0)
		      fun loop (j, ac) =
			 if j > max
			    then ac
			 else loop (j + 1,
				    C.CS.toString (sourceName
						   (gcState, Pointer.getInt32 (p, j)))
				    :: ac)
		   in
		      loop (1, ac)
		   end)
	       [] a
	    end
   end
