(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Substring: SUBSTRING =
   struct
      open Int

      open Substring0

(*
      datatype t = T of {str: string,
			 start: int,
			 size: int}
      type substring = t

      fun base (T {str, start, size}) = (str, start, size)
	 
      val string = String.substring o base

      fun extract (slice as (str, start, _)): t =
	 let val max = String0.checkSlice slice
	 in T {str = str,
	       start = start,
	       size = max -? start}
	 end
      
      fun substring (s, i, j) = extract (s, i, SOME j)

      fun all s = substring (s, 0, String.size s)

      fun isEmpty (T {size, ...}) = size = 0

      fun getc (T {str, start, size}) =
	 if size = 0
	    then NONE
	 else SOME (String.sub (str, start),
		    T {str = str,
		       start = start +? 1,
		       size = size -? 1})

      fun first ss =
	 case getc ss of
	    NONE => NONE
	  | SOME (c, _) => SOME c

      fun triml k =
	 if Primitive.safe andalso k < 0
	    then raise Subscript
	 else
	    (fn T {str, start, size} =>
	     if k > size
		then T {str = str, start = start +? size, size = 0}
	     else T {str = str, start = start +? k, size = size -? k})

      fun trimr k =
	 if Primitive.safe andalso k < 0
	    then raise Subscript
	 else
	    (fn T {str, start, size} =>
	     T {str = str,
	       start = start,
	       size = if k > size then 0 else size -? k})

      fun slice (T {str, start, size}, i, opt) =
	 case opt of
	    SOME m =>
	       if Primitive.safe andalso 0 <= i
		  andalso 0 <= m
		  andalso i <= size -? m
		  then T {str = str, start = start +? i, size = m}
	       else raise Subscript
	  | NONE =>
	       if Primitive.safe andalso 0 <= i andalso i <= size
		  then T {str = str, start = start +? i, size = size -? i}
	       else raise Subscript
		
      fun sub (T {str, start, size}, i) =
	 if Primitive.safe andalso Int.geu (i, size)
	    then raise Subscript
	 else String.sub (str, start +? i)

      fun size (T {size, ...}) = size

      fun concat substrings =
	 let
	    val size = List.foldl (fn (ss, n) => n +? size ss) 0 substrings
	    val dst = Primitive.Array.array size
	 in
	    List.foldl (fn (T {str, start, size}, n) =>
			let
			   fun loop i =
			      if i >= size then ()
			      else (Array.update (dst, n +? i,
						  String.sub (str, start +? i))
				    ; loop (i + 1))
			in loop 0;
			   n +? size
			end)
	    0 substrings
	    ; String.fromArray dst
	 end

      val explode = String.explode o string

      fun explode (T {str, start, size}) =
	 let
	    fun loop (i, l) =
	       if i < start
		  then l
	       else loop (i -? 1, String.sub (str, i) :: l)
	 in
	    loop (start +? size -? 1, [])
	 end

      fun isPrefix str' (T {str, start, size}) =
	 let
	    val size' = String.size str'
	    fun loop (i, i') =
	       i' >= size'
	       orelse (String.sub (str, i) = String.sub (str', i')
		       andalso loop (i +? 1, i' + 1))
	 in
	    size' <= size andalso loop (start, 0)
	 end

      fun collate comp (T {str, start, size},
			T {str=str', start=start', size=size'}) =
	 let
	    val max = start +? size
	    val max' = start' +? size'
	    fun loop (i, i') =
	       if i >= max
		  then if i' = max'
			  then EQUAL
		       else LESS
	       else if i' >= max'
		       then GREATER
		    else (case comp (String.sub (str, i),
				     String.sub (str', i')) of
			     EQUAL => loop (i + 1, i' + 1)
			   | r => r)
	 in loop (start, start')
	 end

      val compare = collate Char.compare

      fun split (T {str, start, size}, i) =
	 (T {str = str, start = start, size = i -? start},
	  T {str = str, start = i, size = size -? (i -? start)})

      fun splitl f (ss as T {str, start, size}) =
	 let
	    val stop = start +? size
	    fun loop i =
	       if i >= stop
		  then i
	       else if f (String.sub (str, i))
		       then loop (i + 1)
		    else i
	 in split (ss, loop start)
	 end

      fun splitr f (ss as T {str, start, size}) =
	 let
	    fun loop i =
	       if i < start
		  then start
	       else if f (String.sub (str, i))
		       then loop (i -? 1)
		    else i +? 1
	 in split (ss, loop (start +? size -? 1))
	 end
   
      fun splitAt (T {str, start, size}, i) =
	 if Primitive.safe andalso Int.gtu (i, size)
	    then raise Subscript
	 else (T {str = str, start = start, size = i},
	       T {str = str, start = start +? i, size = size -? i})

      fun takel p s = #1 (splitl p s)
      fun dropl p s = #2 (splitl p s)
      fun taker p s = #2 (splitr p s)
      fun dropr p s = #1 (splitr p s)
	     
      fun position s' (ss as T {str=s, start, size}) =
	 let
	    val size' = String.size s'
	    val max = start +? size -? size' +? 1
	    (* loop returns the index of the front of suffix. *)
	    fun loop i =
	       if i >= max
		  then start +? size
	       else let
		       fun loop' j =
			  if j >= size'
			     then i
			  else if String.sub (s, i +? j) = String.sub (s', j)
				  then loop' (j + 1)
			       else loop (i + 1)
		    in loop' 0
		    end
	 in split (ss, loop start)
	 end

      fun span (T {str = s, start = i, size = n},
	       T {str = s', start = i', size = n'}) =
	 if s = s' andalso i' +? n' >= i
	    then T {str = s, start = i, size = i' +? n' -? i}
	 else raise Span

      type cs = int
	 
      fun reader (T {str, start, size}): (char, cs) Reader.reader =
	 fn i => if i >= size
		    then NONE
		 else SOME (String.sub (str, start +? i), i + 1)
		    
      fun 'a scanSubstring
	 (f: (char, cs) Reader.reader -> ('a, int) Reader.reader)
	 (ss: substring): 'a option =
	 case f (reader ss) 0 of
	    NONE => NONE
	  | SOME (a, _) => SOME a

      local
	 fun make finish p (T {str, start, size}) =
	    let
	       val max = start +? size
	       fun loop (i, start, sss) =
		  if i >= max
		     then rev (finish (str, start, i, sss))
		  else
		     if p (String.sub (str, i))
			then loop (i + 1, i + 1, finish (str, start, i, sss))
		     else loop (i + 1, start, sss)
	    in loop (start, start, []) 
	    end
      in
	 val tokens = make (fn (str, start, stop, sss) =>
			   if start = stop
			      then sss
			   else
			      T {str = str, start = start, size = stop -? start}
			      :: sss)
	 val fields = make (fn (str, start, stop, sss) =>
			   T {str = str, start = start, size = stop -? start}
			   :: sss)
      end

      local
	 fun make naturalFold f b (T {str, size, start}) =
	    naturalFold (size, b, fn (i, b) => 
			 f (String.sub (str, start +? i), b))
      in
	 fun foldl f = make Util.naturalFold f
	 fun foldr f = make Util.naturalFoldDown f
      end

      fun app f ss = foldl (f o #1) () ss

      fun translate f ss =
	 String.concat (rev (foldl (fn (c, l) => f c :: l) [] ss))
*)
   end

structure SubstringGlobal: SUBSTRING_GLOBAL = Substring
open SubstringGlobal
