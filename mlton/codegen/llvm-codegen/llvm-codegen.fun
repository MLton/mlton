(* Copyright (C) 2009 Wesley W. Terpstra.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor LLVMCodegen(S: LLVM_CODEGEN_STRUCTS): LLVM_CODEGEN =
   struct
      open S
      
      fun implementsPrim _ = true
      fun output _ = ()
   end
