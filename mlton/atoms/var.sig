(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature VAR_STRUCTS =
   sig
      structure AstId: AST_ID
   end

signature VAR =
   sig
      (*include VAR_STRUCTS*)
      include HASH_ID
   end
