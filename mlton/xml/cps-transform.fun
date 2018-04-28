(* Copyright (C) 2007-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CPSTransform (S: XML_TRANSFORM_STRUCTS): XML_TRANSFORM = 
struct

open S
datatype z = datatype Dec.t
datatype z = datatype PrimExp.t

fun transform (prog: Program.t): Program.t =
   let
      val Program.T {datatypes, body, overflow} = prog

      (* Answer type is always unit in an XML IL program. *)
      val ansTy = Type.unit
      (* Exception type is always exn in an XML IL program. *)
      val exnTy = Type.exn


      (* Style of function-type translation. *)
      datatype style = Curried | Mixed | Uncurried
      val style = Uncurried

      val {hom = transType, destroy = destroyTransType} =
         Type.makeMonoHom
         {con = fn (_, c, tys) =>
          if Tycon.equals (c, Tycon.arrow)
             then let
                     val argTy = Vector.sub (tys, 0)
                     val resTy = Vector.sub (tys, 1)
                  in
                     case style of
                        Curried =>
                           Type.arrow 
                           (Type.arrow (resTy, ansTy),
                            Type.arrow
                            (Type.arrow (exnTy, ansTy),
                             Type.arrow (argTy, ansTy)))
                      | Mixed =>
                           Type.arrow 
                           ((Type.tuple o Vector.new2)
                            (Type.arrow (resTy, ansTy),
                             Type.arrow (exnTy, ansTy)),
                            Type.arrow (argTy, ansTy))
                      | Uncurried => 
                           Type.arrow
                           ((Type.tuple o Vector.new3)
                            (Type.arrow (resTy, ansTy),
                             Type.arrow (exnTy, ansTy),
                             argTy), 
                            ansTy)
                  end
          else Type.con (c, tys)}

      (* A property to record (original) type of each bound variable. *)
      val {get = getVarOrigType: Var.t -> Type.t, set = setVarOrigType, ...} =
         Property.getSetOnce 
         (Var.plist, Property.initRaise ("getVarOrigType", Var.layout))
      val getVarExpOrigType = getVarOrigType o VarExp.var

      (* A mayOverflow primitive needs a special translation with a wrapper
       * datatype.  See transPrimExp:PrimApp.
       *)
      val wrapDatatypes = ref []
      val {get = getWrap, destroy = destroyWrap, ...} =
         Property.destGet
         (Type.plist, Property.initFun (fn ty =>
          let
             val successCon = Con.newString "Success"
             val failureCon = Con.newString "Failure"
             val wrapTycon = Tycon.newString "Wrap"
             val wrapTy = Type.con (wrapTycon, Vector.new0 ())
             val wrapDatatype = 
                {cons = Vector.new2
                        ({arg = SOME ty, con = successCon},
                         {arg = SOME exnTy, con = failureCon}),
                 tycon = wrapTycon,
                 tyvars = Vector.new0 ()}
             val () = List.push (wrapDatatypes, wrapDatatype)
          in
             {successCon = successCon, 
              failureCon = failureCon, 
              wrapTy = wrapTy}
          end))

      fun transVarExpWithType (x: VarExp.t) : DirectExp.t * Type.t =
         let
            val xTy = transType (getVarExpOrigType x)
         in
            (DirectExp.varExp (x, xTy), xTy)
         end
      val transVarExp = #1 o transVarExpWithType

      fun transLambda (l: Lambda.t): Lambda.t =
         let
            val {arg = argVar, argType = argTy, body, mayInline} = Lambda.dest l
            val resTy = getVarExpOrigType (Exp.result body)

            val argTy = transType argTy
            val resTy = transType resTy
            val kVar = Var.newString "k"
            val kTy = Type.arrow (resTy, ansTy)
            val hVar = Var.newString "h"
            val hTy = Type.arrow (exnTy, ansTy) 
            val bodyKHA = transExp (body, kVar, kTy, hVar, hTy)
         in
            case style of
               Curried => 
                  let
                     val bodyKH =
                        DirectExp.lambda
                        {arg = argVar,
                         argType = argTy,
                         body = bodyKHA,
                         bodyType = ansTy,
                         mayInline = mayInline}
                     val bodyK =
                        DirectExp.lambda
                        {arg = hVar,
                         argType = hTy,
                         body = bodyKH,
                         bodyType = Type.arrow (argTy, ansTy),
                         mayInline = true}
                  in
                     Lambda.make
                     {arg = kVar,
                      argType = kTy,
                      body = DirectExp.toExp bodyK,
                      mayInline = true}
                  end
             | Mixed => 
                  let
                     val xVar = Var.newNoname ()
                     val xTy = Type.tuple (Vector.new2 (kTy, hTy))
                     val x = DirectExp.monoVar (xVar, xTy)
                     val bodyKH =
                        DirectExp.lambda
                        {arg = argVar,
                         argType = argTy,
                         body = bodyKHA,
                         bodyType = ansTy,
                         mayInline = mayInline}
                     val bodyXK =
                        DirectExp.let1 
                        {var = hVar, 
                         exp = (DirectExp.select {tuple = x,
                                                  offset = 1,
                                                  ty = hTy}), 
                         body = bodyKH}
                     val bodyX =
                        DirectExp.let1 
                        {var = kVar, 
                         exp = (DirectExp.select {tuple = x,
                                                  offset = 0,
                                                  ty = kTy}), 
                         body = bodyXK}
                  in
                     Lambda.make
                     {arg = xVar,
                      argType = xTy,
                      body = DirectExp.toExp bodyX,
                      mayInline = true}
                  end
             | Uncurried =>
                  let
                     val xVar = Var.newNoname ()
                     val xTy = Type.tuple (Vector.new3 (kTy, hTy, argTy))
                     val x = DirectExp.monoVar (xVar, xTy)
                     val bodyXKH =
                        DirectExp.let1 
                        {var = argVar, 
                         exp = (DirectExp.select {tuple = x,
                                                  offset = 2,
                                                  ty = argTy}), 
                         body = bodyKHA}
                     val bodyXK =
                        DirectExp.let1 
                        {var = hVar, 
                         exp = (DirectExp.select {tuple = x,
                                                  offset = 1,
                                                  ty = hTy}), 
                         body = bodyXKH}
                     val bodyX =
                        DirectExp.let1 
                        {var = kVar, 
                         exp = (DirectExp.select {tuple = x,
                                                  offset = 0,
                                                  ty = kTy}), 
                         body = bodyXK}
                  in
                     Lambda.make
                     {arg = xVar,
                      argType = xTy,
                      body = DirectExp.toExp bodyX,
                      mayInline = mayInline}
                  end
         end
      and transPrimExp (e: PrimExp.t, eTy: Type.t,
                        kVar: Var.t, kTy: Type.t,
                        hVar: Var.t, hTy: Type.t): DirectExp.t =
         let
            val eTy = transType eTy
            val k = DirectExp.monoVar (kVar, kTy)
            val h = DirectExp.monoVar (hVar, hTy)
            fun return x = DirectExp.app {func = k, arg = x, ty = ansTy}
         in 
            case e of
               App {arg, func} =>
                  let
                     val (arg, argTy) = transVarExpWithType arg
                     val func = transVarExp func
                  in
                     case style of
                        Curried => 
                           let
                              val app1 =
                                 DirectExp.app
                                 {func = func,
                                  arg = k,
                                  ty = Type.arrow (hTy, Type.arrow (argTy, ansTy))}
                              val app2 =
                                 DirectExp.app
                                 {func = app1,
                                  arg = h,
                                  ty = Type.arrow (argTy, ansTy)}
                              val app3 =
                                 DirectExp.app
                                 {func = app2,
                                  arg = arg,
                                  ty = ansTy}
                           in
                              app3
                           end
                      | Mixed => 
                           let
                              val arg2 =
                                 DirectExp.tuple 
                                 {exps = Vector.new2 (k, h),
                                  ty = (Type.tuple o Vector.new2) (kTy, hTy)}
                              val app2 = 
                                 DirectExp.app
                                 {func = func,
                                  arg = arg2,
                                  ty = Type.arrow (argTy, ansTy)}
                              val app3 =
                                 DirectExp.app
                                 {func = app2,
                                  arg = arg,
                                  ty = ansTy}
                           in
                              app3
                           end
                      | Uncurried =>
                           let
                              val arg3 =
                                 DirectExp.tuple 
                                 {exps = Vector.new3 (k, h, arg),
                                  ty = (Type.tuple o Vector.new3) (kTy, hTy, argTy)}
                              val app3 =
                                 DirectExp.app
                                 {func = func,
                                  arg = arg3,
                                  ty = ansTy}
                           in
                              app3
                           end
                  end
             | Case {cases, default, test} =>
                  let
                     val cases = 
                        case cases of
                           Cases.Con cases =>
                              let
                                 val cases =
                                    Vector.map
                                    (cases, fn (Pat.T {arg, con, targs}, e) =>
                                     let
                                        val arg =
                                           Option.map
                                           (arg, fn (arg, argTy) =>
                                            (arg, transType argTy))
                                        val targs = Vector.map (targs, transType)
                                     in
                                        (Pat.T {arg = arg, con = con, targs = targs},
                                         transExp (e, kVar, kTy, hVar, hTy))
                                     end)
                              in
                                 Cases.Con cases
                              end
                         | Cases.Word (ws, cases) =>
                              let
                                 val cases =
                                    Vector.map
                                    (cases, fn (w, e) => 
                                     (w, transExp (e, kVar, kTy, hVar, hTy)))
                              in
                                 Cases.Word (ws, cases)
                              end
                     val default =
                        Option.map
                        (default, fn (e, r) =>
                         (transExp (e, kVar, kTy, hVar, hTy), r))
                  in
                     DirectExp.casee
                     {cases = cases,
                      default = default,
                      test = transVarExp test,
                      ty = ansTy}
                  end
             | ConApp {arg, con, targs} =>
                  (return o DirectExp.conApp)
                  {arg = Option.map (arg, transVarExp),
                   con = con,
                   targs = Vector.map (targs, transType), 
                   ty = eTy}
             | Const c => return (DirectExp.const c)
             | Handle {catch = (cVar, _), handler, try} =>
                  let
                     val h'Var = Var.newString "h"
                     val h'Ty = Type.arrow (exnTy, ansTy)
                     val h'Body =
                        DirectExp.lambda
                        {arg = cVar,
                         argType = exnTy,
                         body = transExp (handler, kVar, kTy, hVar, hTy),
                         bodyType = ansTy,
                         mayInline = true}
                  in
                     DirectExp.let1 {var = h'Var, exp = h'Body, body =
                     transExp (try, kVar, kTy, h'Var, h'Ty)}
                  end
             | Lambda l => 
                  let
                     val l = transLambda l
                  in 
                     return (DirectExp.fromLambda (l, eTy))
                  end
             | PrimApp {args, prim, targs} => 
                  let
                     val primAppExp =
                        DirectExp.primApp
                        {args = Vector.map (args, transVarExp),
                         prim = prim,
                         targs = Vector.map (targs, transType),
                         ty = eTy}
                  in
                     if Prim.mayOverflow prim
                        then let
                                (* A mayOverflow primitive has an
                                 * implicit raise, which is introduced
                                 * explicitly by closure-convert
                                 * (transformation from SXML to SSA).
                                 *
                                 * We leave an explicit Handle around
                                 * the primitive to catch the
                                 * exception.  The non-exceptional
                                 * result goes to the (normal)
                                 * continuation, while the exception
                                 * goes to the exception continuation.
                                 *
                                 * Naively, we would do:
                                 *   (k (primApp)) handle x => h x
                                 * But, this evaluates the (normal)
                                 * continuation in the context of the
                                 * handler.
                                 * 
                                 * Rather, we do:
                                 *   case ((Success (primApp)) 
                                 *         handle x => Failure x) of
                                 *     Success x => k x
                                 *     Failure x => h x
                                 * This evaluates the (normal)
                                 * continuation outside the context of
                                 * the handler.  
                                 * 
                                 * See <src>/lib/mlton/basic/exn0.sml
                                 * and "Exceptional Syntax" by Benton
                                 * and Kennedy.
                                 * 
                                 *)

                                val {successCon, failureCon, wrapTy} = 
                                   getWrap eTy

                                val testExp =
                                   let
                                      val xVar = Var.newNoname ()
                                      val x = DirectExp.monoVar (xVar, exnTy)
                                   in
                                      DirectExp.handlee
                                      {try = DirectExp.conApp
                                             {arg = SOME primAppExp,
                                              con = successCon,
                                              targs = Vector.new0 (),
                                              ty = wrapTy},
                                       catch = (xVar, exnTy),
                                       handler = DirectExp.conApp
                                                 {arg = SOME x,
                                                  con = failureCon,
                                                  targs = Vector.new0 (),
                                                  ty = wrapTy},
                                       ty = wrapTy}
                                   end

                                val successCase =
                                   let
                                      val xVar = Var.newNoname ()
                                   in
                                      (Pat.T {arg = SOME (xVar, eTy),
                                              con = successCon,
                                              targs = Vector.new0 ()},
                                       DirectExp.app
                                       {func = k,
                                        arg = DirectExp.monoVar (xVar, eTy),
                                        ty = ansTy})
                                   end
                                val failureCase =
                                   let
                                      val xVar = Var.newNoname ()
                                   in
                                      (Pat.T 
                                       {arg = SOME (xVar, exnTy),
                                        con = failureCon,
                                        targs = Vector.new0 ()},
                                       DirectExp.app
                                       {func = h,
                                        arg = DirectExp.monoVar (xVar, exnTy),
                                        ty = ansTy})
                                   end
                                val cases =
                                   Cases.Con (Vector.new2 (successCase, failureCase))
                             in
                                DirectExp.casee
                                {test = testExp,
                                 cases = cases,
                                 default = NONE,
                                 ty = ansTy}
                             end
                     else return primAppExp
                  end
             | Profile _ => 
                  let
                     (* Profile statements won't properly nest after
                      * CPS conversion.
                      *)
                  in 
                     Error.bug "CPSTransform.transPrimExp: Profile"
                  end
             | Raise {exn, ...} => 
                  DirectExp.app
                  {func = h,
                   arg = transVarExp exn,
                   ty = ansTy}
             | Select {offset, tuple} => 
                  (return o DirectExp.select)
                  {tuple = transVarExp tuple,
                   offset = offset,
                   ty = eTy}
             | Tuple xs =>
                  (return o DirectExp.tuple)
                  {exps = Vector.map (xs, transVarExp),
                   ty = eTy}
             | Var x => return (transVarExp x)
         end
      and transDec (d: Dec.t,
                    kBody: DirectExp.t, 
                    hVar: Var.t, hTy: Type.t): DirectExp.t =
         let
         in
            case d of
               Exception _ => Error.bug "CPSTransform.transDec: Exception"
             | Fun {decs, tyvars} => 
                  let
                     val decs =
                        Vector.map
                        (decs, fn {var, ty, lambda} =>
                         {var = var,
                          ty = transType ty,
                          lambda = transLambda lambda})
                     val d = Fun {decs = decs, tyvars = tyvars}
                  in
                     DirectExp.lett {decs = [d], body = kBody}
                  end
             | MonoVal {var, ty, exp} => 
                  let
                     val expTy = ty
                     val argVar = var
                     val argTy = transType ty
                     val k'Var = Var.newString "k"
                     val k'Ty = Type.arrow (argTy, ansTy)
                     val k'Body =
                        DirectExp.lambda
                        {arg = argVar,
                         argType = argTy,
                         body = kBody,
                         bodyType = ansTy,
                         mayInline = true}
                  in
                     DirectExp.let1 {var = k'Var, exp = k'Body, body =
                     transPrimExp (exp, expTy, k'Var, k'Ty, hVar, hTy)}
                  end
             | PolyVal _ => Error.bug "CPSTransform.transDec: PolyVal"
         end
      and transExp (e: Exp.t, 
                    kVar: Var.t, kTy: Type.t, 
                    hVar: Var.t, hTy: Type.t): DirectExp.t =
         let
            val {decs, result} = Exp.dest e
            val k = DirectExp.monoVar (kVar, kTy)
            val k'Body =
               DirectExp.app
               {func = k, arg = transVarExp result, ty = ansTy}
         in
            List.foldr
            (decs, k'Body, fn (dec, kBody) =>
             transDec (dec, kBody, hVar, hTy))
         end

      (* Set (original) type of each bound variable. *)
      val () =
         Exp.foreachBoundVar
         (body, fn (v, _, ty) => 
          setVarOrigType (v, ty))

      (* Translate datatypes. *)
      val datatypes =
         Vector.map
         (datatypes, fn {cons, tycon, tyvars} =>
          {cons = Vector.map (cons, fn {arg, con} =>
                              {arg = Option.map (arg, transType),
                               con = con}),
           tycon = tycon,
           tyvars = tyvars})

      (* Initial continuation. *)
      val k0 = Var.newString "k0"
      val k0Body =
         DirectExp.lambda
         {arg = Var.newNoname (),
          argType = ansTy,
          body = DirectExp.unit (),
          bodyType = ansTy,
          mayInline = true}
      val k0Ty = Type.arrow (ansTy, Type.unit)
      (* Initial exception continuation. *)
      val h0 = Var.newString "h0"
      val h0Body =
         DirectExp.lambda
         {arg = Var.newNoname (),
          argType = exnTy,
          body = DirectExp.unit (),
          bodyType = ansTy,
          mayInline = true}
      val h0Ty = Type.arrow (exnTy, Type.unit)

      (* Translate body, in context of initial continuations. *)
      val body = DirectExp.let1 {var = k0, exp = k0Body, body =
                 DirectExp.let1 {var = h0, exp = h0Body, body =
                 transExp (body, k0, k0Ty, h0, h0Ty)}}

      (* Closure-convert (transformation from SXML to SSA) introduces 
       * every (non-main) SSA function with "raises = [exn]"; 
       * we need a top-level handler to avoid a "raise mismatch" type
       * error in the SSA IL. 
       *)
      val body = DirectExp.handlee
                 {try = body,
                  catch = (Var.newNoname (), exnTy),
                  handler = DirectExp.unit (),
                  ty = ansTy}
      val body = DirectExp.toExp body

      (* Fetch accumulated wrap datatypes. *)
      val wrapDatatypes = Vector.fromList (!wrapDatatypes)
      val datatypes = Vector.concat [datatypes, wrapDatatypes]

      val prog = Program.T {datatypes = datatypes, 
                            body = body, 
                            overflow = overflow}

      (* Clear and destroy properties. *)
      val () = Exp.clear body
      val () = destroyTransType ()
      val () = destroyWrap ()
   in
      prog
   end

end
