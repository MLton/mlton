(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature AST_ID_STRUCTS =
   sig
      (* the name to print out for error messages *)
      val className: string
   end

signature AST_ID =
   sig
      include AST_ID_STRUCTS
      include WRAPPED

      type t
      sharing type obj = t

      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool
      val bogus: t
      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val fromString: string * Region.t -> t
      val hash: t -> Word.t
      val layout: t -> Layout.t
      val toString: t -> string
      val unbound: t -> unit  (* report an unbound error message *)
   end
