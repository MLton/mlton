(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Sequence (S: sig
			type 'a sequence 
			type 'a elt
			(* fromArray should be constant time. *)
			val fromArray: 'a elt array -> 'a sequence 
			val isMutable: bool
			val length: 'a sequence -> int
			val sub: 'a sequence * int -> 'a elt
		     end
		  ): SEQUENCE =
   struct
      open S

      structure Array = Primitive.Array

      open Primitive.Int

      val maxLen = Array.maxLen

      fun array n =
	 if not isMutable andalso n = 0
	    then Array.array0Const ()
	 else Array.array n

      fun sub (s, i) =
	 if Primitive.safe andalso Primitive.Int.geu (i, length s)
	    then raise Subscript
	 else S.sub (s, i)

      fun update (a, i, x) =
	 if Primitive.safe andalso Primitive.Int.geu (i, Array.length a)
	    then raise Subscript
	 else Array.update (a, i, x)

      fun unfoldi (n, b, f) =
	 let
	    val a = array n
	    fun loop (i, b)  =
	       if i = n
		  then ()
	       else
		  let
		     val (x, b') = f (i, b)
		     val _ = Array.update (a, i, x)
		  in
		     loop (i + 1, b')
		  end
	    val _ = loop (0, b)
	 in
	    fromArray a
	 end

      (* Tabulate depends on the fact that the runtime system fills in the array
       * with reasonable bogus values.
       *)
      fun tabulate (n, f) =
	 if !Primitive.usesCallcc
	    then
	       (* This code is careful to use a list to accumulate the 
		* components of the array in case f uses callcc.
		*)
	       let
		  fun loop (i, l) =
		     if i >= n
			then l
		     else loop (i + 1, f i :: l)
		  val l = loop (0, [])
		  val a = array n
		  fun loop (l, i) =
		     case l of
			[] => ()
		      | x :: l =>
			   let val i = i -? 1
			   in Array.update (a, i, x)
			      ; loop (l, i)
			   end
	       in loop (l, n)
		  ; fromArray a
	       end
	 else
	    unfoldi (n, (), fn (i, ()) => (f i, ()))

      fun new (n, x) = tabulate (n, fn _ => x)

      fun fromListOfLength (l, n) =
	 let 
	    val a = array n
	    val rec loop =
	       fn ([],     _) => ()
		| (x :: l, i) => (Array.update (a, i, x)
				  ; loop (l, i +? 1))
	 in loop (l, 0)
	    ; fromArray a
	 end

      fun fromList l = fromListOfLength (l, List.length l)

      type 'a slice = 'a sequence * int * int option
	 
      fun 'a wholeSlice (a: 'a sequence): 'a slice = (a, 0, NONE)

      fun checkSliceMax (start: int, num: int option, max: int): int =
	 case num of
	    NONE => if Primitive.safe andalso (start < 0 orelse start > max)
		       then raise Subscript
		    else max
	  | SOME num =>
	       if Primitive.safe
		  andalso (start < 0 orelse num < 0 orelse start > max -? num)
		  then raise Subscript
	       else start +? num

      fun checkSlice (s, i, opt) = checkSliceMax (i, opt, length s)

      fun foldli f b (slice as (s, min, _)) =
	 let
	    val max = checkSlice slice
	    fun loop (i, b) =
	       if i >= max then b
	       else loop (i + 1, f (i, S.sub (s, i), b))
	 in loop (min, b)
	 end

      fun appi f slice = foldli (fn (i, x, ()) => f (i, x)) () slice

      fun app f s = appi (f o #2) (wholeSlice s)

      fun foldri f b (slice as (s, min, _)) =
	 let
	    val max = checkSlice slice
	    fun loop (i, b) =
	       if i < min
		  then b
	       else loop (i -? 1, f (i, S.sub (s, i), b))
	 in loop (max -? 1, b)
	 end

      local
	 fun make foldi f b s = foldi (fn (_, x, b) => f (x, b)) b (wholeSlice s)
      in
	 fun foldl f = make foldli f
	 fun foldr f = make foldri f
      end

      fun mapi f (slice as (s, min, _)) =
	 let val max = checkSlice slice
	 in tabulate (max -? min, fn i => let val j = i +? min
					  in f (j, S.sub (s, j))
					  end)
	 end

      fun map f s = mapi (f o #2) (wholeSlice s)

      val extract =
	 fn (s, 0, NONE) => s
	  | slice => mapi #2 slice

      fun copy s = map (fn x => x) s

      fun 'a concat (vs: 'a sequence list): 'a sequence =
	 case vs of
	    [] => fromArray (array 0)
	  | [v] => if isMutable then copy v else v
	  | v :: vs' => 
	       let
		  val n = List.foldl (fn (v, s) => s + length v) 0 vs
	       in
		  unfoldi (n, (0, v, vs'),
			   fn (_, ac) =>
			   let
			      fun loop (i, v, vs) =
				 if i < length v
				    then (sub (v, i), (i + 1, v, vs))
				 else
				    case vs of
				       [] => raise Fail "concat bug"
				     | v :: vs => loop (0, v, vs)
			   in loop ac
			   end)
	       end
 
      fun prefixToList (s, n) =
	 let
	    fun loop (i, l) =
	       if i < 0
		  then l
	       else loop (i - 1, S.sub (s, i) :: l)
	 in loop (n - 1, [])
	 end

      fun toList a = prefixToList (a, length a)
	 
      fun find (s, p) =
	 let
	    val max = length s
	    fun loop i =
	       if i >= max
		  then NONE
	       else let
		       val x = S.sub (s, i)
		    in if p x
			  then SOME x
		       else loop (i + 1)
		    end
	 in loop 0
	 end
   end
