(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ImplementSuffix (S: XML_TRANSFORM_STRUCTS): XML_TRANSFORM = 
struct

open S
datatype z = datatype Dec.t
datatype z = datatype PrimExp.t
structure Dexp = DirectExp

fun transform (Program.T {datatypes, body, overflow, ...}): Program.t =
   let
      (* topLevelSuffix holds the ref cell containing the function of
       * type unit -> unit that should be called on program exit.
       *)
      val topLevelSuffixType = Type.arrow (Type.unit, Type.unit)
      val topLevelSuffixVar = Var.newNoname ()

      fun loop (e: Exp.t): Exp.t =
         let
            val {decs, result} = Exp.dest e
            val decs = List.rev (List.fold (decs, [], fn (d, ds) =>
                                            loopDec d :: ds))
         in
            Exp.make {decs = decs,
                      result = result}
         end
      and loopDec (dec: Dec.t): Dec.t =
         case dec of
            MonoVal b => loopMonoVal b
          | Fun {decs, ...} =>
               Fun {tyvars = Vector.new0 (),
                    decs = Vector.map (decs, fn {var, ty, lambda} =>
                                       {var = var,
                                        ty = ty,
                                        lambda = loopLambda lambda})}
          | Exception {...} => dec
          | _ => Error.bug "ImplementSuffix: saw unexpected dec"
      and loopMonoVal {var, ty, exp} : Dec.t =
         let
            fun primExp e = MonoVal {var = var, ty = ty, exp = e}
            fun keep () = primExp exp
         in
            case exp of
               Case {test, cases, default} =>
                  primExp (Case {cases = Cases.map (cases, loop),
                                 default = (Option.map
                                            (default, fn (e, r) =>
                                             (loop e, r))),
                                 test = test})
             | ConApp {...} => keep ()
             | Handle {try, catch = (catch, ty), handler} =>
                  primExp (Handle {try = loop try,
                                   catch = (catch, ty),
                                   handler = loop handler})
             | Lambda l => primExp (Lambda (loopLambda l))
             | PrimApp {args, prim, ...} =>
                  let
                     datatype z = datatype Prim.Name.t
                     fun deref (var, ty) =
                        primExp
                        (PrimApp {prim = Prim.deref,
                                  targs = Vector.new1 ty,
                                  args = Vector.new1 (VarExp.mono var)})
                     fun assign (var, ty) =
                        primExp
                        (PrimApp {prim = Prim.assign,
                                  targs = Vector.new1 ty,
                                  args = Vector.new2 (VarExp.mono var,
                                                      Vector.first args)})
                  in
                     case Prim.name prim of
                        TopLevel_getSuffix =>
                           deref (topLevelSuffixVar,
                                  topLevelSuffixType)
                      | TopLevel_setSuffix =>
                           assign (topLevelSuffixVar,
                                   topLevelSuffixType)
                      | _ => keep ()
                  end
             | _ => keep ()
         end
      and loopLambda l =
         let
            val {arg, argType, body, mayInline} = Lambda.dest l
         in
            Lambda.make {arg = arg,
                         argType = argType,
                         body = loop body,
                         mayInline = mayInline}
         end
      val body = Dexp.fromExp (loop body, Type.unit)
      val body =
         (Dexp.sequence o Vector.new2)
         (body,
          Dexp.app {func = (Dexp.deref
                            (Dexp.monoVar
                             (topLevelSuffixVar,
                              Type.reff topLevelSuffixType))),
                    arg = Dexp.unit (),
                    ty = Type.unit})
      val body =
         Dexp.let1
         {var = topLevelSuffixVar,
          exp = Dexp.reff (Dexp.lambda
                           {arg = Var.newNoname (),
                            argType = Type.unit,
                            body = Dexp.bug "toplevel suffix not installed",
                            bodyType = Type.unit,
                            mayInline = true}),
          body = body}
      val body = Dexp.toExp body
   in
      Program.T {datatypes = datatypes,
                 body = body,
                 overflow = overflow}
   end
end
