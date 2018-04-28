(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ANALYZE_STRUCTS = 
   sig
      include DIRECT_EXP
   end

signature ANALYZE = 
   sig
      include ANALYZE_STRUCTS

      val analyze:
         {coerce: {from: 'a,
                   to: 'a} -> unit,
          conApp: {args: 'a vector,
                   con: Con.t} -> 'a,
          const: Const.t -> 'a,
          filter: 'a * Con.t * 'a vector -> unit,
          filterWord: 'a * WordSize.t -> unit,
          fromType: Type.t -> 'a,
          layout: 'a -> Layout.t,
          primApp: {args: 'a vector,
                    prim: Type.t Prim.t,
                    resultType: Type.t,
                    resultVar: Var.t option,
                    targs: Type.t vector} -> 'a,
          program: Program.t,
          select: {offset: int,
                   resultType: Type.t,
                   tuple: 'a} -> 'a,
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
