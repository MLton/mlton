(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ImplementExceptions (S: IMPLEMENT_EXCEPTIONS_STRUCTS):
   IMPLEMENT_EXCEPTIONS = 
struct

open S
datatype z = datatype Dec.t
datatype z = datatype PrimExp.t
structure Dexp = DirectExp

fun doit (Program.T {datatypes, body, ...}): Program.t =
   let
      (* topLevelHandler holds the ref cell containing the function of
       * type exn -> unit that should be called on unhandled exceptions.
       *)
      val topLevelHandler = Var.newNoname ()
      val extendExtraVar = Var.newNoname ()
      val exnName = Var.newString "exnName"
      (* sumType is the type of the datatype with all of the exn constructors. *)
      val {dropVar,
           extendExtraType,
           extra,
           extraDatatypes,
           extract,
           extractSum,
           inject,
           raisee,
           sumTycon,
           sumType,
           wrapBody
           } =
         if not (!Control.exnHistory)
            then {dropVar = fn _ => false,
                  extendExtraType = Type.unit,
                  extra = fn _ => Error.bug "ImplementExceptions: no extra",
                  extraDatatypes = Vector.new0 (),
                  extract = fn (exn, _, f) => f (Dexp.monoVar (exn, Type.exn)),
                  extractSum = fn e => e,
                  inject = fn e => e,
                  raisee = (fn {exn, extend, ty, var} =>
                            [MonoVal {var = var, ty = ty,
                                      exp = Raise {exn = exn,
                                                   extend = extend}}]),
                  sumTycon = Tycon.exn,
                  sumType = Type.exn,
                  wrapBody = Dexp.toExp}
         else
            let
               val sumTycon = Tycon.newNoname ()
               val sumType = Type.con (sumTycon, Vector.new0 ())
               fun find (nameString: string, isName: Type.t Prim.Name.t -> bool)
                  : Var.t * Type.t * PrimExp.t =
                  let
                     val var =
                        Exn.withEscape
                        (fn escape =>
                         let
                            val _ =
                               Exp.foreachPrimExp
                               (body, fn (_, _, e) =>
                                case e of
                                   PrimApp {args, prim, ...} =>
                                      if isName (Prim.name prim)
                                         then escape (VarExp.var
                                                      (Vector.sub (args, 0)))
                                      else ()
                                 | _ => ())
                         in
                            Error.bug 
                            (concat ["ImplmentExceptions: can't find var for", 
                                     nameString])
                         end)
                     val (ty, exp) =
                        Exn.withEscape
                        (fn escape =>
                         let
                            val _ = Exp.foreachPrimExp (body, fn (x, t, e) =>
                                                        if Var.equals (x, var)
                                                           then escape (t, e)
                                                        else ())
                         in
                            Error.bug
                            (concat ["ImplementExceptions: can't find ", 
                                     Var.toString var])
                         end)
                  in
                     (var, ty, exp)
                  end
               val (initExtraVar, initExtraType, initExtraExp) =
                  find ("Exn_setInitExtra",
                        fn Prim.Name.Exn_setInitExtra => true | _ => false)
               val extraType = initExtraType
               val extendExtraType = Type.arrow (extraType, extraType)
               local
                  open Type
               in
                  val exnCon = Con.newNoname ()
                  val exnConArgType = tuple (Vector.new2 (extraType, sumType))
                  val seType = tuple (Vector.new2 (string, extraType))
               end
               fun wrapBody body =
                  let
                     val body =
                        Dexp.let1
                        {body = body,
                         exp = (Dexp.reff
                                (Dexp.lambda
                                 {arg = Var.newNoname (),
                                  argType = extraType,
                                  body = Dexp.bug ("extendExtra unimplemented",
                                                   extraType),
                                  bodyType = extraType,
                                  mayInline = true})),
                         var = extendExtraVar}
                  in
                     Exp.prefix (Dexp.toExp body,
                                 Dec.MonoVal {var = initExtraVar,
                                              ty = initExtraType,
                                              exp = initExtraExp})
                  end
               fun makeExn {exn, extra} =
                  let
                     open Dexp
                  in
                     conApp
                     {con = exnCon,
                      targs = Vector.new0 (),
                      ty = Type.exn,
                      arg = SOME (tuple {exps = Vector.new2 (extra, exn),
                                         ty = exnConArgType})}
                  end
               fun inject (exn: Dexp.t): Dexp.t =
                  makeExn {exn = exn,
                           extra = Dexp.monoVar (initExtraVar, initExtraType)}
               fun extractSum x =
                  Dexp.select {tuple = x, offset = 1, ty = sumType}
               fun extract (exn: Var.t, ty, f: Dexp.t -> Dexp.t): Dexp.t =
                  let
                     open Dexp
                     val tuple = Var.newNoname ()
                  in
                     casee
                     {test = monoVar (exn, Type.exn),
                      default = NONE,
                      ty = ty,
                      cases =
                      Cases.Con (Vector.new1
                                 (Pat.T {con = exnCon,
                                         targs = Vector.new0 (),
                                         arg = SOME (tuple, exnConArgType)},
                                  f (monoVar (tuple, exnConArgType))))}
                  end
               fun extra (x: Var.t) =
                  extract (x, extraType, fn tuple =>
                           Dexp.select {tuple = tuple,
                                        offset = 0,
                                        ty = extraType})
               fun raisee {exn: VarExp.t,
                           extend: bool,
                           ty: Type.t,
                           var = x : Var.t}: Dec.t list =
                  let
                     open Dexp
                     val exp =
                        if not extend
                           then raisee {exn = varExp (exn, Type.exn),
                                        extend = false, ty = ty}
                        else
                           extract
                           (VarExp.var exn, ty, fn tup =>
                            raisee
                            {exn = makeExn
                             {exn = select {tuple = tup,
                                            offset = 1,
                                            ty = sumType},
                              extra =
                              app
                              {func = deref (monoVar
                                             (extendExtraVar,
                                              Type.reff extendExtraType)),
                               arg = tuple {exps = (Vector.new1
                                                    (select {tuple = tup,
                                                             offset = 0,
                                                             ty = extraType})),
                                            ty = seType},
                               ty = extraType}},
                             extend = false,
                             ty = ty})
                  in
                     vall {exp = exp, var = x}
                  end
               val extraDatatypes =
                  Vector.new1 {tycon = Tycon.exn,
                               tyvars = Vector.new0 (),
                               cons = Vector.new1 {con = exnCon,
                                                   arg = SOME exnConArgType}}
               fun dropVar x = Var.equals (x, initExtraVar)
            in
               {dropVar = dropVar,
                extendExtraType = extendExtraType,
                extra = extra,
                extraDatatypes = extraDatatypes,
                extract = extract,
                extractSum = extractSum,
                inject = inject,
                raisee = raisee,
                sumTycon = sumTycon,
                sumType = sumType,
                wrapBody = wrapBody}
            end
      val {get = exconInfo: Con.t -> {refVar: Var.t,
                                      make: VarExp.t option -> Dexp.t} option,
           set = setExconInfo, destroy} =
         Property.destGetSetOnce (Con.plist, Property.initConst NONE)
      val setExconInfo = 
         Trace.trace2 
         ("ImplementExceptions.setExconInfo", 
          Con.layout, Layout.ignore, Unit.layout) 
         setExconInfo
      val exconInfo =
         Trace.trace 
         ("ImplementExceptions.exconInfo", 
          Con.layout, Layout.ignore) 
         exconInfo
      fun isExcon c =
         case exconInfo c of
            NONE => false
          | SOME _ => true
      val exnValCons: {con: Con.t, arg: Type.t} list ref = ref []
      val overflow = ref NONE
      val traceLoopDec =
         Trace.trace
         ("ImplementExceptions.loopDec", Dec.layout, List.layout Dec.layout)
      fun loop (e: Exp.t): Exp.t =
         let
            val {decs, result} = Exp.dest e
            val decs = List.concatRev (List.fold (decs, [], fn (d, ds) =>
                                                  loopDec d :: ds))
         in
            Exp.make {decs = decs,
                      result = result}
         end
      and loopDec arg: Dec.t list =
         traceLoopDec
         (fn (dec: Dec.t) =>
         case dec of
            MonoVal b => loopMonoVal b
          | Fun {decs, ...} =>
               [Fun {tyvars = Vector.new0 (),
                     decs = Vector.map (decs, fn {var, ty, lambda} =>
                                        {var = var,
                                         ty = ty,
                                         lambda = loopLambda lambda})}]
          | Exception {con, arg} =>
               let
                  open Dexp
                  val r = Var.newString "exnRef"
                  val uniq = monoVar (r, Type.unitRef)
                  fun conApp arg =
                     inject (Dexp.conApp {con = con,
                                          targs = Vector.new0 (),
                                          ty = sumType,
                                          arg = SOME arg})
                  val (arg, decs, make) =
                     case arg of
                        NONE =>
                           (* If the exception is not value carrying, then go
                            * ahead and make it now.
                            *)
                           let
                              val exn = Var.newNoname ()
                              val _ =
                                 if Con.equals (con, Con.overflow)
                                    then overflow := SOME exn
                                 else ()
                           in (Type.unitRef,
                               Dexp.vall {var = exn, exp = conApp uniq},
                               fn NONE => monoVar (exn, Type.exn)
                                | _ => Error.bug "ImplementExceptions: nullary excon applied to arg")
                           end
                      | SOME t =>
                           let
                              val tupleType =
                                 Type.tuple (Vector.new2 (Type.unitRef, t))
                           in (tupleType,
                               [],
                               fn SOME x =>
                               conApp (tuple {exps = Vector.new2 (uniq,
                                                                  varExp (x, t)),
                                              ty = tupleType})
                                | _ =>
                                     Error.bug "ImplmentExceptions: unary excon not applied to arg")
                           end
               in setExconInfo (con, SOME {refVar = r, make = make})
                  ; List.push (exnValCons, {con = con, arg = arg})
                  ; vall {var = r, exp = reff (unit ())} @ decs
               end
          | _ => Error.bug "ImplementExceptions: saw unexpected dec") arg
      and loopMonoVal {var, ty, exp} : Dec.t list =
         if dropVar var
            then []
         else
         let
            fun primExp e = [MonoVal {var = var, ty = ty, exp = e}]
            fun keep () = primExp exp
            fun makeExp e = Dexp.vall {var = var, exp = e}
         in
            case exp of
               Case {test, cases, default} =>
                  let
                     fun normal () =
                        primExp (Case {cases = Cases.map (cases, loop),
                                       default = (Option.map
                                                  (default, fn (e, r) =>
                                                   (loop e, r))),
                                       test = test})
                  in
                     case cases of
                        Cases.Con cases =>
                           if Vector.isEmpty cases
                              then normal ()
                           else
                              let
                                 val (Pat.T {con, ...}, _) =
                                    Vector.sub (cases, 0)
                              in
                                 if not (isExcon con)
                                    then normal ()
                                 else (* convert to an exception match *)
                                    let
                                       open Dexp
                                       val defaultVar = Var.newString "default"
                                       fun callDefault () =
                                          app {func = (monoVar
                                                       (defaultVar,
                                                        Type.arrow
                                                        (Type.unit, ty))),
                                               arg = unit (),
                                               ty = ty}
                                       val unit = Var.newString "unit"
                                       val (body, region) =
                                          case default of
                                             NONE =>
                                                Error.bug "ImplementExceptions: no default for exception case"
                                           | SOME (e, r) =>
                                                (fromExp (loop e, ty), r)
                                       val decs =
                                          vall
                                          {var = defaultVar,
                                           exp = lambda {arg = unit,
                                                         argType = Type.unit,
                                                         body = body,
                                                         bodyType = ty,
                                                         mayInline = true}}
                                    in
                                       makeExp
                                       (lett
                                        {decs = decs,
                                         body =
                                         extract
                                         (VarExp.var test, ty, fn tuple =>
                                          casee
                                          {test = extractSum tuple,
                                           ty = ty,
                                           default = SOME (callDefault (),
                                                           region),
                                           cases =
                                           Cases.Con
                                           (Vector.map
                                            (cases, fn (Pat.T {con, arg, ...}, e) =>
                                             let
                                                val refVar = Var.newNoname ()
                                                val body =
                                                   iff {test =
                                                        equal
                                                        (monoVar
                                                         (refVar, Type.unitRef),
                                                         monoVar
                                                         (#refVar (valOf (exconInfo con)),
                                                          Type.unitRef)),
                                                        ty = ty,
                                                        thenn = (fromExp
                                                                 (loop e, ty)),
                                                        elsee = callDefault ()}
                                                fun make (arg, body) = 
                                                   (Pat.T
                                                    {con = con,
                                                     targs = Vector.new0 (),
                                                     arg = SOME arg},
                                                    body)
                                             in case arg of
                                                NONE => make ((refVar, Type.unitRef), body)
                                              | SOME (x, t) =>
                                                   let
                                                      val tuple =
                                                         (Var.newNoname (),
                                                          Type.tuple (Vector.new2
                                                                      (Type.unitRef, t)))
                                                   in
                                                      make (tuple,
                                                            detupleBind
                                                            {tuple = monoVar tuple,
                                                             components =
                                                             Vector.new2 (refVar, x),
                                                             body = body})
                                                   end
                                             end))})})
                                    end
                              end
                      | _ => normal ()
                  end
             | ConApp {con, arg, ...} =>
                  (case exconInfo con of
                      NONE => keep ()
                    | SOME {make, ...} => makeExp (make arg))
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
                                                      Vector.sub (args, 0))})
                  in
                     case Prim.name prim of
                        Exn_extra => makeExp (extra (VarExp.var
                                                     (Vector.sub (args, 0))))
                      | Exn_name =>
                           primExp (App {func = VarExp.mono exnName,
                                         arg = Vector.sub (args, 0)})
                      | Exn_setExtendExtra =>
                           assign (extendExtraVar, extendExtraType)
                      | Exn_setInitExtra => primExp (Tuple (Vector.new0 ()))
                      | TopLevel_getHandler =>
                           deref (topLevelHandler,
                                  Type.arrow (Type.exn, Type.unit))
                      | TopLevel_setHandler =>
                           assign (topLevelHandler,
                                   Type.arrow (Type.exn, Type.unit))
                      | _ => primExp exp
                  end
             | Raise {exn, extend} =>
                  raisee {exn = exn, extend = extend, ty = ty, var = var}
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
      val body =
         let
            val x = (Var.newNoname (), Type.exn)
         in
            Dexp.handlee
            {try = Dexp.fromExp (loop body, Type.unit),
             ty = Type.unit,
             catch = x,
             handler = Dexp.app {func = (Dexp.deref
                                         (Dexp.monoVar
                                          (topLevelHandler,
                                           let open Type
                                           in reff (arrow (exn, unit))
                                           end))),
                                 arg = Dexp.monoVar x,
                                 ty = Type.unit}}
         end
      val body =
         Dexp.let1
         {var = topLevelHandler,
          exp = Dexp.reff (Dexp.lambda
                           {arg = Var.newNoname (),
                            argType = Type.exn,
                            body = Dexp.bug ("toplevel handler not installed",
                                             Type.unit),
                            bodyType = Type.unit,
                            mayInline = true}),
          body = body}
      val body = wrapBody body
      val (datatypes, body) =
         case !exnValCons of
            [] => (datatypes, body)
          | cons =>
               let
                  val cons = Vector.fromList cons
                  val exnNameDec =
                     MonoVal
                     {var = exnName,
                      ty = Type.arrow (Type.exn, Type.string),
                      exp =
                      let
                         val exn = Var.newNoname ()
                      in
                         Lambda
                         (Lambda.make
                          {arg = exn,
                           argType = Type.exn,
                           mayInline = true,
                           body =
                           let
                              open Dexp
                           in toExp
                              (extract
                               (exn, Type.string, fn tuple =>
                                casee
                                {test = extractSum tuple,
                                 cases =
                                 Cases.Con
                                 (Vector.map
                                  (cons, fn {con, arg} =>
                                   (Pat.T {con = con,
                                           targs = Vector.new0 (),
                                           arg = SOME (Var.newNoname (), arg)},
                                    const
                                    (Const.string
                                     (Con.originalName con))))),
                                 default = NONE,
                                 ty = Type.string}))
                           end})
                       end}
               in
                  (Vector.concat
                   [Vector.new1
                    {tycon = sumTycon,
                     tyvars = Vector.new0 (),
                     cons = Vector.map (cons, fn {con, arg} =>
                                        {con = con, arg = SOME arg})},
                    extraDatatypes,
                    datatypes],
                   Exp.prefix (body, exnNameDec))
               end
      val body =
         Exp.fromPrimExp
         (Handle {try = body,
                  catch = (Var.newNoname (), Type.exn),
                  handler =
                  let
                     val s = Var.newNoname ()
                  in Exp.prefix
                     (Exp.fromPrimExp
                      (PrimApp {prim = Prim.bug,
                                targs = Vector.new1 Type.unit,
                                args = Vector.new1 (VarExp.mono s)},
                       Type.unit),
                      MonoVal {var = s,
                               ty = Type.string,
                               exp = Const (Const.string
                                            "toplevel handler not installed")})
                  end},
          Type.unit)
      val program =
         Program.T {datatypes = datatypes,
                    body = body,
                    overflow = !overflow}
      val _ = destroy ()
   in
      program
   end

end
