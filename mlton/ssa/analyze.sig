(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature ANALYZE_STRUCTS = 
   sig
      include SSA_TREE
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
			      returns: 'a vector},
	     label: Label.t -> 'a vector,
	     exnVal: 'a option
	    }
   end
