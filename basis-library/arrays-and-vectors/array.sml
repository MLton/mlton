(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Array: ARRAY_EXTRA =
   struct
      structure A = Sequence (type 'a sequence = 'a array
			      type 'a elt = 'a
			      val fromArray = fn a => a
			      val isMutable = true
			      open Primitive.Array)
      open A
      open Primitive.Int

      local open Primitive.Array
      in val unsafeSub = sub
	 val unsafeUpdate = update
      end

      type 'a array = 'a array
      type 'a vector = 'a vector

      val array = new

      (* can't use o because of value restriction *)
      val extract = fn arg => Primitive.Vector.fromArray (extract arg)

      fun modifyi f (slice as (a, _, _)) =
	 appi (fn (i, x) => unsafeUpdate (a, i, f (i, x))) slice

      fun modify f a = modifyi (f o #2) (wholeSlice a)

      local
	 fun make (checkSlice, sub) {src, si, len, dst, di} =
	    let
	       val sm = checkSlice (src, si, len)
	       val diff = si -? di
	    in modifyi
	       (fn (i, _) => sub (src, i +? diff))
	       (dst, di, SOME (sm -? si))
	    end
      in
	 fun copy (arg as {src, si, len, dst, di}) =
	    if src = dst andalso si < di
	       then
		  (* Must go right-to-left *)
		  let
		     val sm = checkSlice (src, si, len)
		     val dm = checkSlice (dst, di, SOME (sm -? si))
		     fun loop i =
			if i < si then ()
			else (unsafeUpdate (dst, di +? i, unsafeSub (src, i))
			      ; loop (i -? 1))
		  in loop (sm -? 1)
		  end
	    else make (checkSlice, unsafeSub) arg
	       
	 fun copyVec arg =
	    make (Vector.checkSlice, Primitive.Vector.sub) arg
      end
   end

structure ArrayGlobal: ARRAY_GLOBAL = Array
open ArrayGlobal
