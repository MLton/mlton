(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Uncurry(S: XML_TRANSFORM_STRUCTS): XML_TRANSFORM = 
struct

open S
open Dec PrimExp

fun transform (program as Program.T{datatypes, body, overflow}) =
  let
    datatype D = T of {var: Var.t, lambda : Lambda.t}

    val {get = getArity: Var.t -> int, 
         set = setArity, ...} =
      Property.getSet(Var.plist, 
                      Property.initConst 0)

    val {get = curriedRep: Var.t -> {unCurriedFun: D, curriedFun: D} option,
         set = setCurriedRep, ...} = 
      Property.getSet(Var.plist, 
                      Property.initConst NONE)

    val {get = getType: Var.t -> {args: Type.t vector, result: Type.t}, 
         set = setType, ...} =
      Property.getSet(Var.plist, 
                      Property.initConst {args = Vector.new1 Type.unit,
                                          result = Type.unit})

    fun getResultType(exp) =                                                    
      let 
        val {decs,result} = Exp.dest(exp)
      in 
        List.fold
        (decs, Type.unit, fn (d, i) => 
         case d of
           MonoVal {var, ty, exp} =>
             if Var.equals(var,VarExp.var(result))
               then ty
               else i
         | Fun {tyvars, decs} =>
                 Vector.fold
                 (decs, Type.unit, fn ({var,ty,lambda}, i) =>
                  if Var.equals(var,VarExp.var(result))
                    then ty
                    else i)
         | _ => i)
      end

    fun buildLambda(f,args,types,resultType) =
      let 
        val newArg' = Var.newString("c")
        val newArg'' = Var.newString("c")
      in
        Lambda.new
        {arg = Vector.last(args),
         argType = Vector.last(types),
         body = Vector.fold2
                (Vector.tabulate(Vector.length args - 2,
                                 fn i => Vector.sub(args, i + 1)),
                 Vector.tabulate(Vector.length types - 2,
                                 fn i => Vector.sub(types, i + 1)),
                 let 
                   val newVar = Var.newString("c")
                   val arg = Vector.sub(args,0)
                   val argType = Vector.sub(types,0)
                   val decs =
                     [MonoVal
                      {var = newVar,
                       ty = Type.arrow(argType,resultType),
                       exp = Lambda 
                             (Lambda.new
                              {arg = arg,
                               argType = argType,
                               body = Exp.new 
                                      {decs =
                                       [MonoVal
                                        {var = newArg',
                                         ty = Type.tuple(Vector.rev(types)),
                                         exp = Tuple(Vector.map
                                                     (Vector.rev(args),
                                                      fn a => VarExp.mono(a)))},
                                        MonoVal
                                        {var = newArg'',
                                         ty = resultType,
                                         exp = App {func = f,
                                                    arg = VarExp.mono(newArg')}}],
                                       result = VarExp.mono(newArg'')}})}]
                   val result = VarExp.mono(newVar)
                 in 
                   Exp.new
                   {decs = decs, result = result}
                 end,
                 fn (a, atype, i) => 
                 let
                   val newVar = Var.newString("c")
                 in 
                   Exp.new
                   {decs = [MonoVal
                            {var = newVar,
                             ty = Type.arrow(atype, getResultType(i)),
                             exp = Lambda(Lambda.new {arg = a,
                                                      argType = atype,
                                                      body = i})}],
                    result = VarExp.mono(newVar)}
                 end)}
      end

    fun uncurryFun(dec) = 
      let 
        fun lamExp(decs,result,args,types,newDecs,e) =
          case decs of
            [] => (args,types,e)
          | d::rest =>
              case d of
                Dec.MonoVal{var, ty, exp = Const c} =>
                  lamExp(rest, result, args,types,d::newDecs,e)
              | Dec.MonoVal{var, ty, exp = Var v} =>
                  lamExp(rest, result, args,types,d::newDecs,e)
              | Dec.MonoVal{var, ty, exp = Select tuple} =>
                  lamExp(rest, result, args,types,d::newDecs,e)
              | Dec.MonoVal{var, ty, exp = Lambda l} =>
                  let 
                    val body = Lambda.body(l)
                    val r = result
                    val {decs,result} = Exp.dest(body)
                    val newDecs = List.append(newDecs,decs)
                    val new = Exp.new{decs = newDecs,result = result}
                  in 
                    if Var.equals(var, VarExp.var(r))
                       andalso List.isEmpty(rest)
                      then lamExp(newDecs,
                                  result,
                                  Lambda.arg(l)::args,
                                  Lambda.argType(l)::types,
                                  [],
                                  new)
                      else (args,types,e)
                  end
              | _ => (args,types,e)
        val lamExp = fn x =>
          let val (args,types,e) = lamExp x 
          in (Vector.fromList args, Vector.fromList types, e)
          end

        val T{var,lambda} = dec
        val (f, r) = let 
                       val arg = Lambda.arg(lambda)
                       val argType = Lambda.argType(lambda)
                       val body = Lambda.body(lambda)
                       val {decs,result} = Exp.dest(body)
                     in 
                       (var,  lamExp(decs, result, [arg], [argType], [],body))
                     end

        fun buildCurried (f,args,types,e) =
          let 
            val newVar = Var.newString("c")
            val newArg = Var.newString("c")
            val (newDecs,n) = 
              Vector.fold2
              (Vector.rev(args),
               Vector.rev(types),
               ([],0), fn (a, mtype, (l, i)) =>
               (MonoVal 
                {var = a,
                 ty = mtype,
                 exp = PrimExp.Select {tuple = VarExp.mono(newArg),
                                       offset = i}}::l,
                i+1))
            val newExp = Exp.new {decs = List.append(newDecs, Exp.decs(e)),
                                  result = Exp.result(e)}
            val resultType = getResultType(newExp)
            val unCurriedFun =
              T{var = newVar,
                lambda = Lambda.new {arg = newArg,
                                     argType = Type.tuple(Vector.rev(types)), 
                                     body = newExp}}
            val newArgs = Vector.map(args, fn z => Var.newString("c"))
            val newFun = buildLambda(VarExp.mono(newVar),newArgs,types,resultType)

            val newFunBinding = T{var = f,  lambda = newFun}
          in 
            setCurriedRep(f, SOME {unCurriedFun = unCurriedFun,
                                   curriedFun = newFunBinding})
          end
      in 
        case r of
          (args,types,e) =>
            (setArity(f, Vector.length(args));
             setType(f, {args = types, result = getResultType(e)});
             if getArity(f) > 1 
               then buildCurried(f,args,types,e)
               else ())
      end

    fun replaceVar(decs,old,new) =
      let 
        fun compare(v) = if Var.equals(VarExp.var(v),old)
                           then new
                           else v
        fun replaceExp(e) = let 
                              val {decs,result} = Exp.dest(e)
                              val newDecs = replaceVar(decs,old,new)
                              val newResult = compare(result)
                            in 
                              Exp.new {decs = newDecs,
                                       result = newResult}
                            end
      in  
        List.map
        (decs, fn d =>
         (case d of
            MonoVal {var, ty, exp} =>
              MonoVal {var=var,
                       ty = ty,
                       exp = (case exp of
                                Var v => PrimExp.Var(compare(v))
                              | Tuple vs =>
                                  Tuple(Vector.map(vs, fn v => compare(v)))
                              | Select {tuple,offset} =>
                                  Select {tuple = compare(tuple),
                                          offset = offset}
                              | Lambda l =>
                                  let 
                                    val {arg,argType,body} = Lambda.dest(l)
                                    val {decs,result} = Exp.dest(body)
                                    val newDecs = replaceVar(decs,old,new)
                                  in 
                                    Lambda (Lambda.new
                                            {arg=arg,
                                             argType=argType,
                                             body=Exp.new {decs = newDecs,
                                                           result = result}})
                                  end
                              | ConApp {con,targs,arg} =>
                                  (case arg of
                                     NONE => exp
                                   | SOME v => ConApp {con = con,
                                                       targs = targs,
                                                       arg = SOME (compare(v))})
                              | PrimApp {prim,targs,args} =>
                                  PrimApp {prim = prim,
                                           targs = targs,
                                           args = Vector.map(args, fn a => compare(a))}
                              | App {func,arg} =>
                                  App {func = compare(func),
                                       arg = compare(arg)}
                              | Raise {exn,filePos} => 
                                  Raise {exn = compare(exn), 
                                         filePos = filePos}
                              | Case {test,cases,default} =>
                                  Case {test=compare(test),
                                        cases = Cases.map
                                                (cases,fn e =>
                                                 replaceExp(e)),
                                        default = Option.map
                                                  (default, fn (e,r) =>
                                                   (replaceExp e,r))}
                              | Handle {try,catch,handler} =>
                                  Handle {try = replaceExp(try),
                                          catch = catch,
                                          handler = replaceExp(handler)}
                              | _ => exp)}
          | Fun {tyvars,decs} =>
              Fun {tyvars=tyvars,
                   decs = Vector.map
                          (decs, fn {var,ty,lambda} =>
                           {var = var,
                            ty = ty,
                            lambda = let 
                                       val {arg,argType,body} =
                                         Lambda.dest(lambda)
                                     in 
                                       Lambda.new
                                       ({arg = arg,
                                         argType = argType,
                                         body = replaceExp(body)})
                                     end})}
          | _ => d))
      end

    fun uncurryApp(decs,expResult) =
      let 
        fun makeUncurryApp(f,arguments,lastCall) =
          let 
            val newArg = Var.newString("c")
            val newArg' = Var.newString("c")
            val varF = VarExp.var(f)
            val {args,result} = getType(varF)
            val var = (case curriedRep(varF) of
                         NONE => Error.bug "Uncurry: uncurryApp"
                       | SOME {unCurriedFun,curriedFun} =>
                           let val T{var,lambda} = unCurriedFun
                           in var
                           end)
            val argDec = MonoVal{var = newArg,
                                 ty = Type.tuple(Vector.rev(args)),
                                 exp = Tuple(Vector.rev(arguments))}
            val appDec = MonoVal{var = newArg',
                                 ty = result,
                                 exp = App {func = VarExp.mono(var),
                                            arg = VarExp.mono(newArg)}}
            val newR = if Var.equals(lastCall, VarExp.var(expResult))
                         then (SOME newArg')
                         else NONE
          in (appDec::[argDec],newR,newArg')
          end
      in case decs of
        [] => Error.bug "Uncurry: uncurryApp"
      | d::r => (case d of
                   MonoVal {var, ty, exp = App {func,arg}} =>
                     (case curriedRep(VarExp.var(func)) of
                        NONE => Error.bug "Uncurry: uncurryApp"
                      | SOME _ => let 
                                    val arity = getArity(VarExp.var(func))
                                    fun loop(args,arity,d,f) =
                                      if arity = 0
                                        then SOME (Vector.fromList args,d,f)
                                        else
                                          case d of
                                            [] => NONE
                                          |  h::r =>
                                              (case h of
                                                 MonoVal {var,ty,
                                                          exp = App {func,arg}} =>
                                                   if Var.equals(VarExp.var(func),f)
                                                     then loop(arg::args,
                                                               arity-1,
                                                               r,
                                                               var)
                                                     else NONE
                                               | _ => NONE)
                                  in 
                                    case loop([arg],arity-1,r,var) of
                                      NONE => ([d],r,NONE)
                                    | SOME (args,r,lastCall) =>
                                        let 
                                          val (newDecs,newR,newArg) =
                                            makeUncurryApp(func,args,lastCall)
                                          val r = (replaceVar(r,lastCall,
                                                              VarExp.mono(newArg)))
                                        in 
                                          (newDecs,r,newR)
                                        end
                                  end)
                 | _ => Error.bug "Uncurry: uncurryApp")
      end

    fun singleUse(var,decs) =
      let 
        fun compare(e) = (case e of
                            App {func,arg} => Var.equals(VarExp.var(func),var)
                          | _ => false)
      in 
        List.fold
        (decs, false, fn (d,r) => 
         case d of
           MonoVal {var,ty,exp} => compare(exp)
         | _ => false)
      end


    fun transform(body) =
      let 
        val {decs,result} = Exp.dest(body)
        val newR = ref NONE
      in
        Exp.new
        {decs =
         List.rev
         (let 
            fun loop(decs,newDecs) =
              case decs of
                [] => newDecs
              | d::rest =>
                  (case d of
                     MonoVal {var,ty, exp = Lambda l} =>
                       (case curriedRep(var) of
                          NONE => 
                            let 
                              val lamBody = Lambda.body(l)
                              val arg = Lambda.arg(l)
                              val argType = Lambda.argType(l)
                              val newLam =
                                Lambda.new{arg=arg,
                                           argType = argType,
                                           body = transform(lamBody)}
                              val newDec = MonoVal{var=var,
                                                   ty=ty,
                                                   exp = Lambda newLam}
                            in 
                              loop(rest,newDec::newDecs)
                            end
                        | SOME {unCurriedFun,curriedFun} =>
                            let 
                              val T{var,lambda} = unCurriedFun
                              val body = Lambda.body(lambda)
                              val newBody = transform(body)
                              val resultType = getResultType(newBody)
                              val argType = Lambda.argType(lambda)
                              val l = Lambda(Lambda.new
                                             {arg =
                                              Lambda.arg(lambda),
                                              argType = argType,
                                              body = newBody})
                              val b1 = MonoVal{var=var,
                                               ty = Type.arrow(argType,resultType),
                                               exp = l}
                              val T{var,lambda} = curriedFun
                              val argType = Lambda.argType(lambda)
                              val resultType = getResultType(Lambda.body(lambda))
                              val b2 = MonoVal{var=var,
                                               ty =
                                               Type.arrow(argType, resultType),
                                               exp = Lambda lambda}
                            in loop(rest,b2::(b1::newDecs))
                            end)
                   | MonoVal {var,ty,exp = App {func,arg}} => 
                       (case curriedRep(VarExp.var(func)) of
                          NONE => loop(rest,d::newDecs)
                        | SOME _ => 
                            if singleUse(var,rest)
                              then 
                                let 
                                  val (appDecs,r,newResult) =
                                    uncurryApp(decs,result)
                                   in (newR := newResult;
                                       loop(r,List.append(appDecs,newDecs)))
                                end
                              else loop(rest,d::newDecs))
                   | MonoVal {var,ty,exp = Case {test,cases,default}} =>
                       let 
                         val newCases =
                           Cases.map(cases, fn e => transform(e))
                         val default = Option.map
                                       (default, fn (e,r) =>
                                        (transform(e),r))
                       in 
                         loop(rest,
                              (MonoVal{var=var,
                                       ty=ty,
                                       exp = Case {test = test,
                                                   cases = newCases,
                                                   default = default}}::
                               newDecs))
                       end
                   | MonoVal {var,ty, exp = Handle {try,catch,handler}} =>
                       loop(rest, 
                            (MonoVal{var=var,
                                     ty=ty,
                                     exp = Handle {try = transform(try),
                                                   catch = catch,
                                                   handler = transform(handler)}}::
                             newDecs))
                   | Fun {tyvars,decs} => 
                       loop(rest,
                            Fun {tyvars = Vector.new0 (),
                                 decs =
                                 Vector.fromList(
                                 Vector.fold
                                 (decs,
                                  []:{var:Var.t,
                                      ty:Type.t,
                                      lambda:Lambda.t} list,
                                  fn (d as {var,
                                            ty,
                                            lambda:Lambda.t},
                                      acc) => 
                                  (case curriedRep(var) of
                                     NONE =>
                                       let 
                                         val body = Lambda.body(lambda)
                                         val arg = Lambda.arg(lambda)
                                         val argType = Lambda.argType(lambda)
                                         val newBody = transform(body)
                                         val newLam = Lambda.new{arg = arg,
                                                                 argType = argType,
                                                                 body = newBody}
                                       in 
                                         {var=var,
                                          ty=ty,
                                          lambda=newLam}::acc
                                       end
                                   | SOME {unCurriedFun,curriedFun} =>
                                       let 
                                         val T{var,lambda} = unCurriedFun
                                         val body = Lambda.body(lambda)
                                         val newBody = transform(body)
                                         val argType = Lambda.argType(lambda)
                                         val resultType = getResultType(newBody)
                                         val b1 = {var=var,
                                                   ty = Type.arrow(argType,resultType),
                                                   lambda =
                                                   Lambda.new{arg =  Lambda.arg(lambda),
                                                              argType = argType,
                                                              body = newBody}}
                                         val T{var,lambda} = curriedFun
                                         val argType = Lambda.argType(lambda)
                                         val newBody = transform(Lambda.body(lambda))
                                         val resultType = getResultType(newBody)
                                         val b2 = {var=var,
                                                   ty = Type.arrow(argType,resultType),
                                                   lambda = lambda}
                                       in b1::(b2::acc)
                                       end)))}::newDecs)
                   | _ => loop(rest,d::newDecs))
          in loop(decs,[])
          end),
         result = (case !newR of
                     NONE => result
                   | SOME r => VarExp.mono(r))}
      end
  in
    Exp.foreachExp
    (body, fn e =>
     let 
       val {decs,result} = Exp.dest(e)
     in 
       List.foreach
       (decs, fn d =>
        case d of
          MonoVal {var,ty,exp = Lambda l} =>
            uncurryFun(T{var=var,lambda=l})
        | Fun {tyvars,decs} =>
            Vector.foreach
            (decs, fn {var,ty,lambda} =>
             uncurryFun(T{var=var,lambda=lambda}))
        | _ => ())
     end); 
    let val newBody = transform(body)
    in 
      Program.T{datatypes = datatypes, body = newBody, overflow = overflow}
    end
  end
end
