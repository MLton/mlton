(* Copyright (C) 2009,2019,2022-2023 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor FixReal(PReal: sig include PERVASIVE_REAL val zero : real end) : REAL =
   struct
      open PReal

      (* SML/NJ uses an old version of datatype IEEEReal.float_class. *)
      (* Fixed in SML/NJ 110.99.3. *)
      local
         val toGoodFC = IEEERealExtra.toGoodFC
         val toGoodDA = IEEERealExtra.toGoodDA
         val toBadDA = IEEERealExtra.toBadDA
      in
         val class = toGoodFC o class
         val fromDecimal = SOME o fromDecimal o toBadDA
         val toDecimal = toGoodDA o toDecimal
      end

      (* SML/NJ doesn't support EXACT. *)
      fun fmt f =
         PReal.fmt
         (let
             datatype z = datatype StringCvt.realfmt
          in
             case f of
                EXACT => StringCvt.GEN NONE
              | FIX io => StringCvt.FIX io
              | GEN io => StringCvt.GEN io
              | SCI io => StringCvt.SCI io
          end)

      val fromString = PReal.fromString
      (* SML/NJ raises Overflow on large exponents. *)
      (* Fixed in SML/NJ 110.83. *)
      val fromString = fn s =>
         (case SOME (fromString s) handle Overflow => NONE of
             NONE =>
                let
                   val manexp =
                      String.tokens
                      (fn c => c = #"e" orelse c = #"E")
                      s
                   fun isNeg s =
                      String.sub (s, 0) = #"~"
                      orelse String.sub (s, 0) = #"+"
                   fun isNonzero s =
                      CharVector.exists
                      (fn c => Char.<= (#"1", c) andalso Char.<= (c, #"9"))
                      s
                in
                   case manexp of
                      [man,exp] =>
                         if isNeg exp
                            then SOME zero
                         else if isNonzero man
                            then SOME posInf
                         else SOME zero
                     | _ => NONE
                end
           | SOME ro => ro)
      (* SML/NJ doesn't handle "[+~-]?(inf|infinity|nan)". *)
      (* Fixed in SML/NJ 110.99.3. *)
      val fromString = fn s =>
         case s of
            "inf" => SOME posInf
          | "infinity" => SOME posInf
          | "+inf" => SOME posInf
          | "+infinity" => SOME posInf
          | "~inf" => SOME negInf
          | "~infinity" => SOME negInf
          | "-inf" => SOME negInf
          | "-infinity" => SOME negInf
          | "nan" => SOME (negInf + posInf)
          | "+nan" => SOME (negInf + posInf)
          | "~nan" => SOME (negInf + posInf)
          | "-nan" => SOME (negInf + posInf)
          | _ => fromString s
   end

structure Real = FixReal(struct open Pervasive.Real val zero : real = 0.0 end)
