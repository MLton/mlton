(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature DECS_STRUCTS =
   sig
      structure Ast: AST
      structure CoreML: CORE_ML
      sharing Ast = CoreML.Ast
   end

signature DECS =
   sig
      include DECS_STRUCTS
      
      type dec = CoreML.Dec.t
	 
      type t

      val add: t * dec -> t      (* add a dec to the end of the list *)
      val append: t * t -> t
      val appends: t list -> t
      val empty: t
      val fold: t * 'a * (dec * 'a -> 'a) -> 'a
      val foreach: t * (dec -> unit) -> unit
      val fromList: dec list -> t
      val fromVector: dec vector -> t
      val layout: t -> Layout.t
      val map: t * (dec -> dec) -> t
      val single: dec -> t
      val toAst: t -> Ast.Dec.t
      val toList: t -> dec list
      val toVector: t -> dec vector
   end

