(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure C: C =
   struct
      open Int
	 
      fun makeLength (sub, term) p =
	 let
	    fun loop i =
	       if term (sub (p, i))
		  then i
	       else loop (i +? 1)
	 in loop 0
	 end

      fun toArrayOfLength (s: 'a,
			   sub: 'a * int -> 'b,
			   n: int) : 'b array =
	 let
	    val a = Primitive.Array.array n
	    fun loop i =
	       if i >= n
		  then ()
	       else (Array.update (a, i, sub (s, i))
		     ; loop (i + 1))
	 in loop 0;
	    a
	 end

      structure Prim = Primitive.C

      structure CS =
	 struct
	    open Prim.CS

	    fun toCharArrayOfLength (cs, n) = toArrayOfLength (cs, sub, n)

	    val toWord8ArrayOfLength =
	       charArrayToWord8Array o toCharArrayOfLength
	       
	    val toStringOfLength = String.fromArray o toCharArrayOfLength

	    val length = makeLength (sub, fn #"\000" => true | _ => false)

	    fun toString cs = toStringOfLength (cs, length cs)

	    fun extractToChar (s: t, c: char): string =
	       toStringOfLength (s, makeLength (sub, fn c' => c = c') s)
	 end
      
      structure CSS =
	 struct
	    open Prim.CSS

	    val length = makeLength (sub, Primitive.Cpointer.isNull)

	    val toArrayOfLength =
	       fn (css, n) => toArrayOfLength (css, CS.toString o sub, n)

	    fun toArray css = toArrayOfLength (css, length css)

	    val toList = Array.toList o toArray

	    (* The C side converts the last element of the array, "",
	     * to the null terminator that C primitives expect.
	     * As far as C can tell, the other elements of the array
	     * are just char*'s.
	     *)
	    fun fromList l =
	       let
		  val a = Array.array (1 +? List.length l, "")
		  val _ =
		     List.foldl (fn (s, i) =>
				 (Array.update (a, i, String.nullTerm s)
				  ; i +? 1))
		     0 l
	       in
		  a
	       end
	 end
   end
