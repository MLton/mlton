
signature DIRECT_EXP_STRUCTS =
  sig
     include SSA_TREE
  end

signature DIRECT_EXP =
  sig
     include DIRECT_EXP_STRUCTS

     type t

     datatype cases =
        Char of (char * t) vector
      | Con of {con: Con.t,
		args: (Var.t * Type.t) vector,
		body: t} vector
      | Int of (int * t) vector
      | Word of (word * t) vector
      | Word8 of (Word8.t * t) vector
	
     val toBlocks: t -> Label.t * Block.t list

     val conApp: {con: Con.t, 
		  args: t vector, ty: 
		  Type.t} -> t
     val falsee: t
     val truee: t
     val const: Const.t -> t
     val int: int -> t
     val primApp: {prim: Prim.t, 
		   targs: Type.t vector, 
		   args: t vector, 
		   ty: Type.t} -> t
     val eq: t * t * Type.t -> t
     val select: {tuple: t, 
		  offset: int, 
		  ty: Type.t} -> t
     val tuple: t vector * Type.t -> t
     val var: Var.t * Type.t -> t


     val name: t * (Var.t -> t) -> t
     val seq: t * t -> t

     val call: {func: Func.t, args: t vector, ty: Type.t} -> t
     val casee: {test: t, 
		 cases: cases,
		 default: t option,
		 ty: Type.t} -> t

     val sendGoto: t * Label.t -> Label.t * Block.t list
  end