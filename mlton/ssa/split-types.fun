(* Copyright (C) 2018 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
functor SplitTypes(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

   open S

   structure TypeInfo = struct
      datatype t = Unchanged of Type.t
      (* each other type has a constructor set with arguments to coerce *)
                 | Fresh of (Tycon.t option * con list ref) Equatable.t
                 | Tuple of t vector
      and con = ConData of Con.t * (t vector)

      fun layoutCon (ConData (con, ts)) =
         Layout.fill [ Con.layout con, Vector.layout layout ts ]
      and layoutFresh (ty, cons) =
         Layout.fill [ Option.layout Tycon.layout ty, Layout.str " # ",
                        (* can't layout arguments due to recursion *)
                       Ref.layout (List.layout (fn ConData (con, _) => Con.layout con)) cons]
      and layout (t: t) =
         case t of
              Unchanged t => Type.layout t
            | Fresh eq => Equatable.layout (eq, layoutFresh)
            | Tuple vect => Vector.layout layout vect

      (* lattice join of options, where NONE is taken as less than SOME a
       * calling inequal if inconsistent
       *
       * if SOME x and SOME y and x = y then return SOME x = SOME y
       * else if one is SOME x and the other NONE then return SOME x
       * else if both NONE then return NONE
       * else call inequal *)
      fun optionJoin (opt1, opt2, equals, inequal) =
         case (opt1, opt2) of
              (NONE, NONE) => NONE
            | (SOME x1, NONE) => SOME x1
            | (NONE, SOME x2) => SOME x2
            | (SOME x1, SOME x2) =>
                 if equals (x1, x2)
                 then SOME x1
                 else inequal (x1, x2)
      fun hash (t : t) : word =
         case t of
              Unchanged ty => Type.hash ty
            | Fresh eq =>
                 (case #1 (Equatable.value eq) of
                      NONE => 0wx008534c (* absolutely arbitrary *)
                    | SOME tycon => Tycon.hash tycon)
                    (* also fairly arbitrary, other than 17 being a good enough prime *)
            | Tuple vect => Vector.fold (vect, 0w1, fn (t, c) => c * 0wx11 + hash t)

      (* Equality, accounting for equating, so it may become more coarse over time *)
      fun equated (t1, t2) =
         case (t1, t2) of
              (Unchanged ty1, Unchanged ty2) => Type.equals (ty1, ty2)
            | (Fresh eq1, Fresh eq2) => Equatable.equals (eq1, eq2)
            | (Tuple tup1, Tuple tup2) => Vector.equals (tup1, tup2, equated)
            | _ => false

      fun mergeFresh ((tycon1, cons1), (tycon2, cons2)) =
         let
            val _ = Control.diagnostics
               (fn display =>
                     display ( Layout.fill [
                     Layout.str "Merging ",
                     Option.layout Tycon.layout tycon1,
                     Layout.str " with ",
                     Ref.layout (List.layout layoutCon) cons1,
                     Layout.str " and ",
                     Option.layout Tycon.layout tycon2,
                     Layout.str " with ",
                     Ref.layout (List.layout layoutCon) cons2]))
            val tycon = (optionJoin (tycon1, tycon2, Tycon.equals,
            fn (tycon1', tycon2') => Error.bug "Inconsistent tycons"))
            val _ = cons1 := List.append (!cons1, !cons2)
         in
            (tycon, cons1)
         end
      and coerce (from, to) =
         case (from, to) of
              (Fresh a, Fresh b) => Equatable.equate (a, b, mergeFresh)
            | (Tuple a, Tuple b) =>
                 let
                    val _ = Vector.map2 (a, b, coerce)
                 in
                    ()
                 end
            | (Unchanged t1, Unchanged t2) =>
                 if Type.equals(t1, t2)
                 then ()
                 else Error.bug "SplitTypes.TypeInfo.coerce: Bad merge of unchanged types"
            | _ =>
                 Error.bug (Layout.toString (Layout.fill [
                 Layout.str "SplitTypes.TypeInfo.coerce: Strange coercion: ",
                 layout from, Layout.str " coerced to ", layout to ]))

                 (* TODO: Non-constructor types should always be equated *)
                 (* delayed types will be dropped, reducing memory in some cases *)
      fun fromType (ty: Type.t) =
         case Type.dest ty of
              Type.Datatype tycon =>
                  if Tycon.equals (tycon, Tycon.bool)
                  then Unchanged ty
                  else Fresh (Equatable.new (SOME tycon, ref []))
            | Type.Tuple ts => Tuple (Vector.map(ts, fromType))
            | _ => Unchanged ty
      fun fromCon {con: Con.t, args: t vector} =
         if Con.equals (con, Con.truee) orelse Con.equals (con, Con.falsee)
            then Unchanged Type.bool
            else Fresh (Equatable.new (NONE, ref [ConData (con, args)]))
      fun fromTuple (vect: t vector) = Tuple vect
      fun const (c: Const.t): t = fromType (Type.ofConst c)
                 end

   fun transform (program as Program.T {datatypes, globals, functions, main}): Program.t =
      let
         fun fromCon {con: Con.t, args: TypeInfo.t vector} =
            let
               val _ = Control.diagnostics
                  (fn display =>
                     display ( Layout.fill
                        [ Layout.str "Analyzing conApp ",
                          Con.layout con,
                          Layout.str " on ",
                          Vector.layout TypeInfo.layout args]))
            in
               TypeInfo.fromCon {con=con,args=args}
            end
         fun fromType (ty: Type.t) =
            let
               val _ = Control.diagnostics
                  (fn display =>
                     display ( Layout.fill
                        [ Layout.str "Analyzing type ",
                          Type.layout ty ]))
            in
               TypeInfo.fromType ty
            end
         val { value, func, label } =
            analyze
            { coerce = fn {from, to} => TypeInfo.coerce (from, to),
              conApp = fromCon,
              const = TypeInfo.const,
              filter = fn _ => (),
              filterWord = fn _ => (),
              fromType = fromType,
              layout = TypeInfo.layout,
              primApp = fn { resultType: Type.t, ...} => TypeInfo.fromType resultType,
              program = program,
              select = fn { resultType: Type.t, ...} => TypeInfo.fromType resultType,
              tuple = TypeInfo.fromTuple,
              useFromTypeOnBinds = true }

         (* value, old type, new type *)
         (* old type mostly to verify consistency *)
         val tyMap : (TypeInfo.t * Type.t * Type.t) HashSet.t =
            HashSet.new {hash = fn (v, oldTy, newTy) => TypeInfo.hash v}

         fun makeTy ty value =
            (case value of
                  TypeInfo.Unchanged ty => ty
                | TypeInfo.Fresh eq =>
                     (case Equatable.value eq of
                           (SOME tycon, cons) => Type.datatypee (Tycon.new tycon)
                         | (NONE, cons) =>
                              Error.bug (Layout.toString (Layout.fill [
                                 Layout.str "SplitTypes.transform.makeTy: ", TypeInfo.layout value,
                                 Layout.str " for type ", Type.layout ty])))
                | TypeInfo.Tuple typeInfos =>
                     (case Type.dest ty of
                           Type.Tuple tys => Type.tuple (Vector.map2 (tys, typeInfos, getTy))
                         | _ => Error.bug "SplitTypes.transform.makeTy: TypeInfo was Tuple but type wasn't"))
         and getTy (oldTy, typeInfo) =
            let
               val (_, oldTy', newTy) =
                  HashSet.lookupOrInsert (tyMap, TypeInfo.hash typeInfo,
                     fn (t', oldTy, newTy) =>
                        TypeInfo.equated (typeInfo, t'),
                     fn () =>
                        (typeInfo, oldTy, makeTy oldTy typeInfo))
               val _ =
                  if not (Type.equals (oldTy, oldTy'))
                  then Error.bug (Layout.toString (Layout.fill [
                       Layout.str "SplitTypes.TypeInfo.coerce: Inconsistent types from analysis: ",
                       TypeInfo.layout typeInfo, Layout.str " unified with ",
                       Type.layout oldTy, Layout.str " and ", Type.layout oldTy']))
                  else ()
            in
               newTy
            end



         fun mergeTyVectorOpt (oldTysOpt, tsOpt, name) =
            case (oldTysOpt, tsOpt) of
                 (NONE, NONE) => NONE
               | (SOME oldTys, SOME ts) =>
                    SOME (Vector.map2 (oldTys, ts, getTy))
               | _ => Error.bug ("SplitTypes.TypeInfo.coerce: Inconsistent " ^ name)


         fun loopExp exp =
            exp
         fun loopStatement (st as Statement.T {exp, ty, var=varopt}) =
            let
               val _ = Control.diagnostics
                  (fn display =>
                        display ( Layout.fill [
                        Layout.str "Looping over statement ",
                        Statement.layout st]))
               val newTy =
                  case varopt of
                        NONE => ty
                      | SOME var => getTy (ty, (value var))
               val newExp = loopExp exp
            in
               Statement.T {exp=newExp, ty=newTy, var=varopt}
            end
         fun loopTransfer transfer =
            transfer
         fun loopBlock (Block.T { args, label, statements, transfer }) =
            let
               val newArgs = Vector.map (args, fn (var, oldTy) => (var, getTy (oldTy, value var)))
               val newStatements = Vector.map (statements, loopStatement)
            in
               Block.T {args=newArgs, label=label, statements=newStatements ,transfer=transfer}
            end


         val functions =
            List.map(functions, fn f =>
               let
                  val { args, blocks, mayInline, name, raises, returns, start } =
                     Function.dest f
                  val { args = argTys, raises = raiseTys, returns = returnTys } = func name
               in
                  Function.new
                  { args = Vector.map2 (args, argTys, fn ((v, oldTy), typeInfo) => (v, getTy (oldTy, typeInfo))),
                    blocks = Vector.map (blocks, loopBlock),
                    mayInline = mayInline,
                    name = name,
                    raises = mergeTyVectorOpt (raises, raiseTys, "raises"),
                    returns = mergeTyVectorOpt (returns, returnTys, "returns"),
                    start = start
                    }
               end)
         val globals =
            Vector.map(globals, loopStatement)



         val datatypes =
            let
               fun hardLookup typeInfo =
                  let
                     val found =
                        HashSet.peek (tyMap, TypeInfo.hash typeInfo,
                           fn (t', oldTy, newTy) =>
                              TypeInfo.equated (typeInfo, t'))
                  in
                     case found of
                          SOME (_,_,ty) => ty
                        | NONE => Error.bug "SplitTypes.datatypes.hardLookup: type info not found"
                  end
               fun reifyCon (TypeInfo.ConData (con, ts)) =
                  {con=con, args=Vector.map (ts, hardLookup)}
               fun reifyCons conList =
                  Vector.fromList (List.map (conList, reifyCon))
               val bool =
                  case Vector.peek (datatypes, fn Datatype.T {tycon, ...} =>
                     Tycon.equals (tycon, Tycon.bool))
                  of
                       SOME ty => ty
                     | NONE => Error.bug "SplitTypes.datatypes.bool: Could not find boolean datatype"
            in
               (Vector.fromList (bool ::
                  (List.keepAllMap (HashSet.toList tyMap,
                     fn (typeInfo, _, newTy) =>
                        case typeInfo of
                             TypeInfo.Fresh eq =>
                                 (case Equatable.value eq of
                                       (_, consRef) =>
                                          SOME (Datatype.T
                                          {cons=reifyCons (!consRef), tycon=Type.deDatatype newTy }))
                           | _ => NONE))))
            end

         val program =
            Program.T
            { datatypes = datatypes,
              globals = globals,
              functions = functions,
              main = main }
            in program end
      end
