(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature ANALYZE2_STRUCTS = 
   sig
      include SSA_TREE2
   end

signature ANALYZE2 = 
   sig
      include ANALYZE2_STRUCTS
      
      val analyze:
	 {coerce: {from: 'a,
		   to: 'a} -> unit,
	  const: Const.t -> 'a,
	  (* In filter, the variant is an 'a option because the targets of Case
	   * branches may ignore the test (by taking 0 args).
	   *)
	  filter: {con: Con.t,
		   test: 'a,
		   variant: 'a option} -> unit,
	  filterWord: 'a * WordSize.t -> unit,
	  fromType: Type.t -> 'a,
	  inject: {sum: Tycon.t, variant: 'a} -> 'a,
	  layout: 'a -> Layout.t,
	  object: {args: 'a Prod.t,
		   con: Con.t option,
		   resultType: Type.t} -> 'a,
	  primApp: {args: 'a vector,
		    prim: Type.t Prim.t,
		    resultType: Type.t,
		    resultVar: Var.t option} -> 'a,
	  program: Program.t,
	  select: {object: 'a,
		   offset: int,
		   resultType: Type.t} -> 'a,
	  update: {object: 'a,
		   offset: int,
		   value: 'a} -> unit,
	  useFromTypeOnBinds: bool,
	  vectorSub: {index: 'a,
		      offset: int,
		      vector: 'a} -> 'a,
	  vectorUpdate: {index: 'a,
			 offset: int,
			 value: 'a,
			 vector: 'a} -> unit}
	 -> {func: Func.t -> {args: 'a vector,
			      raises: 'a vector option,
			      returns: 'a vector option},
	     label: Label.t -> 'a vector,
	     value: Var.t -> 'a}
   end
