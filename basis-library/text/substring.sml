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

      val size = length
      val extract = slice
      fun substring (s, i, j) = extract (s, i, SOME j)
      val all = full
      val string = copy
      val getc = getItem
      fun first ss = Option.map #1 (getItem ss)

      fun triml k =
	 if Primitive.safe andalso k < 0
	    then raise Subscript
	 else
	    (fn ss =>
	     let 
	        val (s, start, len) = base ss
	     in
	        if k > len
		   then unsafeSubstring (s, start +? len, 0)
		else unsafeSubstring (s, start +? k, len -? k)
	     end)
      fun trimr k =
	 if Primitive.safe andalso k < 0
	    then raise Subscript
	 else
	    (fn ss =>
	     let 
	        val (s, start, len) = base ss
	     in
	        unsafeSubstring (s, start, if k > len then 0 else len -? k)
	     end)

      val slice = subslice

      fun concatWith sep sss =
	 (case sss of
	     [] => ""
	   | [s] => string s
	   | ss::sss => 
	        List.foldl (fn (ss,res) => String0.concat [res, sep, string ss]) 
		           (string ss) sss)

      fun explode s = foldr (op ::) [] s

      val isPrefix = fn s => fn s' => isPrefix (op =) s s'
      val isSubstring = fn s => fn s' => isSubsequence (op =) s s'
      val isSuffix = fn s => fn s' => isSuffix (op =) s s'

      fun compare (ss1,ss2) = collate Char.compare (ss1,ss2)

      fun split (ss, i) =
	 let val (s, start, len) = base ss
	 in 
	    (unsafeSubstring (s, start, i -? start),
	     unsafeSubstring (s, i, len -? (i -? start)))
	 end
      fun splitl f ss =
	 let
	    val (s, start, len) = base ss
	    val stop = start +? len
	    fun loop i =
	       if i >= stop
		  then i
	       else if f (String0.unsafeSub (s, i))
		       then loop (i +? 1)
		    else i
	 in split (ss, loop start)
	 end
      fun splitr f ss =
	 let
	    val (s, start, len) = base ss
	    fun loop i =
	       if i < start
		  then start
	       else if f (String0.unsafeSub (s, i))
		       then loop (i -? 1)
		    else i +? 1
	 in split (ss, loop (start +? len -? 1))
	 end
      fun splitAt (ss, i) =
	 let val (s, start, len) = base ss
	 in
	    if Primitive.safe andalso Int.gtu (i, len)
	       then raise Subscript
	    else (unsafeSubstring (s, start, i),
		  unsafeSubstring (s, start +? i, len -? i))
	 end

      fun dropl p s = #2 (splitl p s)
      fun dropr p s = #1 (splitr p s)
      fun takel p s = #1 (splitl p s)
      fun taker p s = #2 (splitr p s)

      fun position s' ss =
	 let
	    val (s, start, size) = base ss
            val size' = String0.size s'
	    val max = start +? size -? size' +? 1
	    (* loop returns the index of the front of suffix. *)
	    fun loop i =
	       if i >= max
		  then start +? size
	       else let
		       fun loop' j =
			  if j >= size'
			     then i
			  else if String0.unsafeSub (s, i +? j) = 
			          String0.unsafeSub (s', j)
				  then loop' (j +? 1)
			       else loop (i +? 1)
		    in loop' 0
		    end
	 in split (ss, loop start)
	 end

      fun span (ss, ss') =
	 let
	    val (s, i, n) = base ss
	    val (s', i', n') = base ss'
	 in
	    if Primitive.safe andalso (s <> s' orelse i' + n' < i)
	       then raise Span
	    else substring(s, i, (i'+n')-i)
	 end

      fun translate f ss =
	 String0.concat (List.rev (foldl (fn (c, l) => f c :: l) [] ss))

      local
	 fun make finish p ss =
	    let
	       val (str, start, size) = base ss
	       val max = start +? size
	       fun loop (i, start, sss) =
		  if i >= max
		     then List.rev (finish (str, start, i, sss))
		  else
		     if p (String0.unsafeSub (str, i))
			then loop (i +? 1, i +? 1, finish (str, start, i, sss))
		     else loop (i +? 1, start, sss)
	    in loop (start, start, []) 
	    end
      in
	 fun tokens p ss =
	    make (fn (str, start, stop, sss) =>
		  if start = stop
		     then sss
		  else
		     (unsafeSubstring (str, start, stop -? start))
		     :: sss)
	         p ss
	 fun fields p ss = 
	    make (fn (str, start, stop, sss) =>
		  (unsafeSubstring (str, start, stop -? start))
		  :: sss)
	         p ss
      end

(*
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
*)
   end

structure SubstringGlobal: SUBSTRING_GLOBAL = Substring
open SubstringGlobal
