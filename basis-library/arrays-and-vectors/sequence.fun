(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Sequence (S: sig
			type 'a sequence 
			type 'a elt
			(* fromArray should be constant time. *)
			val fromArray: 'a elt Array.array -> 'a sequence 
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

      structure Slice =
	 struct
	    type 'a sequence = 'a sequence
	    type 'a elt = 'a elt
	    type 'a slice = {seq: 'a sequence, start: int, len: int}

	    fun base (sl: 'a slice as {seq, start, len}) = (seq, start, len)
	    fun length (sl: 'a slice) = #len sl
	    fun sub (sl: 'a slice as {seq, start, len}, i) =
	       if Primitive.safe andalso Primitive.Int.geu (i, len)
		  then raise Subscript
	       else S.sub (seq, start +? i)
	    fun unsafeSub (sl: 'a slice as {seq, start, ...}, i) =
	       S.sub (seq, start +? i)
	    fun update' update (sl: 'a slice as {seq, start, len}, i, x) =
	       if Primitive.safe andalso Primitive.Int.geu (i, len)
		  then raise Subscript
	       else update (seq, start +? i, x)
	    fun unsafeUpdate' update (sl: 'a slice as {seq, start, ...}, i, x) =
	       update (seq, start +? i, x)
	    fun full (seq: 'a sequence) : 'a slice = 
	       {seq = seq, start = 0, len = S.length seq}
	    fun subslice (sl: 'a slice as {seq, start, len}, 
			  start': int, 
			  len': int option) =
	       case len' of
		  NONE => if Primitive.safe andalso
		             (start' < 0 orelse start' > len)
			     then raise Subscript
			  else {seq = seq,
				start = start +? start',
				len = len -? start'}
		| SOME len' => if Primitive.safe andalso
			          (start' < 0 orelse start' > len orelse
				   len' < 0 orelse len' > len -? start')
				  then raise Subscript
			       else {seq = seq,
				     start = start +? start',
				     len = len'}
	    fun slice (seq: 'a sequence, start: int, len: int option) = 
	       subslice (full seq, start, len)
	    fun isEmpty sl = length sl = 0
	    fun getItem (sl: 'a slice as {seq, start, len}) =
	       if isEmpty sl
		  then NONE
	       else SOME (S.sub (seq, start), 
			  {seq = seq, 
			   start = start +? 1, 
			   len = len -? 1})
	    fun foldli f b (sl: 'a slice as {seq, start, len}) =
	       let
		  val min = start
		  val max = start +? len
		  fun loop (i, b) =
		     if i >= max then b
		     else loop (i +? 1, f (i -? min, S.sub (seq, i), b))
	       in loop (min, b)
	       end
	    fun foldri f b (sl: 'a slice as {seq, start, len}) =
	       let
		  val min = start
		  val max = start +? len
		  fun loop (i, b) =
		     if i <= min then b
		     else loop (i -? 1, f (i -? min, S.sub (seq, i), b))
	       in loop (max, b)
	       end
	    local
	       fun make foldi f b sl = foldi (fn (_, x, b) => f (x, b)) b sl
	    in
	       fun foldl f = make foldli f
	       fun foldr f = make foldri f
	    end
	    fun appi f sl = foldli (fn (i, x, ()) => f (i, x)) () sl
	    fun app f sl = appi (f o #2) sl
	    fun mapi' tabulate f (sl: 'a slice as {seq, start, len}) =
	       tabulate (len, fn i => let val j = start +? i
				      in f (j, S.sub (seq, j))
				      end)
	    fun map' tabulate f sl = mapi' tabulate (f o #2) sl
	    fun mapi f sl = mapi' tabulate f sl
	    fun map f sl = mapi (f o #2) sl
	    fun findi p (sl: 'a slice as {seq, start, len}) = 
	       let
		  val min = start
		  val max = start +? len
		  fun loop i =
		     if i >= max
		        then NONE
		     else let val z = (i -? min, S.sub (seq, i))
			  in if p z
			        then SOME z
			     else loop (i +? 1)
			  end
	       in loop min
	       end
	    fun find p sl = Option.map #2 (findi (p o #2) sl)
	    fun existsi p sl = Option.isSome (findi p sl)
	    fun exists p sl = existsi (p o #2) sl
	    fun alli p sl = not (existsi (not o p) sl)
	    fun all p sl = alli (p o #2) sl
	    fun collate cmp (sl1 as {seq = seq1, start = start1, len = len1},
			     sl2 as {seq = seq2, start = start2, len = len2}) =
	       let
		  val min1 = start1
		  val min2 = start2
		  val max1 = start1 +? len1
		  val max2 = start2 +? len2
		  fun loop (i, j) =
		     case (i >= max1, j >= max2) of
		        (true, true) => EQUAL
		      | (true, false) => LESS
		      | (false, true) => GREATER
		      | (false, false) => 
			   (case cmp (S.sub (seq1, i), S.sub (seq2, j)) of
			      EQUAL => loop (i +? 1, j +? 1)
			    | ans => ans)
	       in loop (min1, min2)
	       end
	    fun toList (sl: 'a slice) = foldr (fn (a,l) => a::l) [] sl
	    fun copy sl = map (fn x => x) sl
	    fun concat (sls: 'a slice list): 'a sequence =
	       case sls of
		  [] => fromArray (array 0)
		| [sl as {seq, start, len}] => 
		     if isMutable orelse (start <> 0 orelse len <> S.length seq)
		        then copy sl
		     else seq
		| sl::sls' =>
		     let
		        val n = List.foldl (fn (sl, s) => s + length sl) 0 sls
			        handle Overflow => raise Size
		     in
		        unfoldi (n, (0, sl, sls'),
				 fn (_, ac) =>
				 let
				    fun loop (i, sl, sls) =
				       if i < length sl
					  then (unsafeSub (sl, i), (i +? 1, sl, sls))
				       else case sls of
					       [] => raise Fail "concat bug"
					     | sl :: sls => loop (0, sl, sls)
				 in loop ac
				 end)
		     end
	 end

      local
	fun make f seq = f (Slice.full seq)
	fun make2 f (seq1, seq2) = f (Slice.full seq1, Slice.full seq2)
      in
	fun sub (seq, i) = Slice.sub (Slice.full seq, i)
	fun unsafeSub (seq, i) = Slice.unsafeSub (Slice.full seq, i)
	fun update' update (seq, i, x) = 
	   Slice.update' update (Slice.full seq, i, x)
	fun unsafeUpdate' update (seq, i, x) = 
	   Slice.unsafeUpdate' update (Slice.full seq, i, x)
	fun foldli f b = make (Slice.foldli f b)
	fun foldri f b = make (Slice.foldri f b)
	fun foldl f b = make (Slice.foldl f b)
	fun foldr f b = make (Slice.foldr f b)
	fun appi f = make (Slice.appi f)
	fun app f = make (Slice.app f)
	fun mapi' tabulate f = make (Slice.mapi' tabulate f)
	fun map' tabulate f = make (Slice.map' tabulate f)
	fun mapi f = make (Slice.mapi f)
	fun map f = make (Slice.map f)
	fun copy seq = make (Slice.copy) seq
	fun findi p = make (Slice.findi p)
	fun find p = make (Slice.find p)
	fun existsi p = make (Slice.existsi p)
	fun exists p = make (Slice.exists p)
	fun alli p = make (Slice.alli p)
	fun all p = make (Slice.all p)
	fun collate cmp = make2 (Slice.collate cmp)
	fun toList seq = make (Slice.toList) seq
	fun concat seqs = Slice.concat (List.map Slice.full seqs)
      end
    
      (* Depreciated *)
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
      (* Depreciated *)
      fun checkSlice (s, i, opt) = checkSliceMax (i, opt, length s)
      (* Depreciatd *)
      fun extract args = Slice.copy (Slice.slice args)
   end
