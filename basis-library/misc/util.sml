(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Util =
   struct
      fun makeCompare (op <) =
	 {compare = (fn (i, j) =>
		     if i < j then LESS
		     else if j < i then GREATER
			  else EQUAL),
	  min = fn (x, y) => if x < y then x else y,
	  max = fn (x, y) => if x < y then y else x}

      fun makeOrder compare =
	 {< = fn (x, y) => (case compare (x, y) of
			      LESS => true
			    | _ => false),
	  <= = fn (x, y) => (case compare (x, y) of
			       GREATER => false
			     | _ => true),
	  > = fn (x, y) => (case compare (x, y) of
			      GREATER => true
			    | _ => false),
	  >= = fn (x, y) => (case compare (x, y) of
			       LESS => false
			     | _ => true)}

      open Primitive.Int
	 
      fun naturalFoldStartStop (start, stop, b, f) =
	 if start > stop
	    then raise Subscript
	 else 
	    let
	       fun loop (i, b) =
		  if i >= stop then b
		  else loop (i + 1, f (i, b))
	    in loop (start, b)
	    end

      fun naturalFold (n, b, f) = naturalFoldStartStop (0, n, b, f)

      fun naturalFoldDown (n, b, f) =
	 if n < 0
	    then raise Subscript
	 else 
	    let
	       fun loop (i, b) =
		  if i < 0 then b
		  else loop (i - 1, f (i, b))
	    in loop (n - 1, b)
	    end

      fun naturalForeachStartStop (start, stop, f) =
	 naturalFoldStartStop (start, stop, (), fn (i, ()) => f i)

      fun naturalForeach (n, f) = naturalForeachStartStop (0, n, f)
   end
