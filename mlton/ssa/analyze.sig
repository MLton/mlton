(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature ANALYZE_STRUCTS = 
   sig
      include DIRECT_EXP
   end

signature ANALYZE = 
   sig
      include ANALYZE_STRUCTS
      
      val analyze:
	 {
	  coerce: {
		   from: 'a,
		   to: 'a
		  } -> unit,
	  conApp: {
		   con: Con.t,
		   args: 'a vector
		   } -> 'a,
	  const: Const.t -> 'a,
	  copy: 'a -> 'a,
	  filter: 'a * Con.t * 'a vector -> unit,
	  filterChar: 'a -> unit,
	  filterInt: 'a -> unit,
	  filterWord: 'a -> unit,
	  filterWord8: 'a -> unit,
	  fromType: Type.t -> 'a,
	  layout: 'a -> Layout.t,
	  primApp: {
		    prim: Prim.t,
		    targs: Type.t vector,
		    args: 'a vector,
		    resultType: Type.t,
		    resultVar: Var.t option
		    } -> 'a,
	  program: Program.t,
	  select: {
		   tuple: 'a,
		   offset: int,
		   resultType: Type.t
		  } -> 'a,
	  tuple: 'a vector -> 'a,
	  useFromTypeOnBinds: bool
	 }
	 -> {
	     value: Var.t -> 'a,
	     func: Func.t -> {args: 'a vector,
			      raises: 'a vector option,
			      returns: 'a vector option},
	     label: Label.t -> 'a vector
	    }
   end
