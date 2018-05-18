(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LAMBDA_FREE_STRUCTS = 
   sig
      include SXML
   end

signature LAMBDA_FREE = 
   sig
      include LAMBDA_FREE_STRUCTS

      structure Status:
         sig
            type t

            val init: t
         end
      (*
       * When called, descends the entire program and attaches a property
       * to each lambda primExp in the program.  Then, you can use
       * lambdaFree to get free variables of that lambda.
       * For lambdas bound in a Fun dec, lambdaFree gives the union of the
       * frees of the entire group of mutually recursive functions.  Hence,
       * lambdaFree for every lambda in a single Fun dec is the same.
       * Furthermore, for a lambda bound in a Fun dec, lambdaRec gives
       * the list of other funs bound in the same dec that the lambda refers
       * to.  For example:
       * 
       * val rec f = fn x => ... y ... g ... f ...
       * and g = fn z => ... f ... w ...
       * 
       * lambdaFree(fn x =>) = [y, w]
       * lambdaFree(fn z =>) = [y, w]
       * lambdaRec(fn x =>) = [g, f]
       * lambdaRec(fn z =>) = [f]
       *)
      val lambdaFree:
         {program: Program.t,
          overflow: Var.t,
          varInfo: Var.t -> {frees: Var.t list ref ref,
                             status: Status.t ref},
          lambdaInfo: Lambda.t -> {frees: Var.t vector ref,
                                   recs: Var.t vector ref}}
         -> unit
   end
