(* Copyright (C) 2011 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor DeadCode (S: DEAD_CODE_STRUCTS): DEAD_CODE = 
struct

open S
open CoreML
open Dec

fun deadCode {prog} =
   let
      val {get = varIsUsed, set = setVarIsUsed, destroy, ...} =
         Property.destGetSet (Var.plist, Property.initConst false)
      fun patVarIsUsed (p: Pat.t): bool =
         Exn.withEscape
         (fn escape =>
          (Pat.foreachVar (p, fn x => if varIsUsed x
                                         then escape true
                                      else ())
           ; false))
      fun decIsWildOrUnit (d: Dec.t): bool =
         case d of
            Val {rvbs, vbs, ...} =>
               0 = Vector.length rvbs
               andalso 1 = Vector.length vbs
               andalso let
                          val pat = #pat (Vector.first vbs)
                       in
                          Pat.isWild pat orelse Pat.isUnit pat
                       end
          | _ => false
      fun decIsNeeded (d: Dec.t): bool =
         case d of
            Datatype _ => true
          | Exception _ => true
          | Fun {decs, ...} => Vector.exists (decs, varIsUsed o #var)
          | Val {rvbs, vbs, ...} =>
               Vector.exists (rvbs, varIsUsed o #var)
               orelse Vector.exists (vbs, patVarIsUsed o #pat)
      fun useVar x = setVarIsUsed (x, true)
      fun useExp (e: Exp.t): unit = Exp.foreachVar (e, useVar)
      fun useLambda (l: Lambda.t): unit =
         useExp (#body (Lambda.dest l))
      fun useDec (d: Dec.t): unit = 
         case d of
            Datatype _ => ()
          | Exception _ => ()
          | Fun {decs, ...} => Vector.foreach (decs, useLambda o #lambda)
          | Val {rvbs, vbs, ...} =>
               (Vector.foreach (rvbs, useLambda o #lambda)
                ; Vector.foreach (vbs, useExp o #exp))

      val n = Vector.length prog
      val m = n - 1
      val prog =
         Vector.tabulate
         (n, fn i =>
          let val (decs, deadCode) = Vector.sub (prog, m - i)
          in
             if deadCode
                then List.fold (rev decs, [], fn (dec, decs) =>
                                if decIsWildOrUnit dec orelse decIsNeeded dec
                                   then (useDec dec; dec :: decs)
                                   else decs)
                else (List.foreach (decs, useDec)
                      ; decs)
          end)
      val _ = destroy ()
   in {prog = Vector.rev prog}
   end

end
