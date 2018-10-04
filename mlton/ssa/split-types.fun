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
                 | Heap of t (* Ref Weak or Vector *)
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
            | Tuple vect => Layout.tuple (Vector.toList (Vector.map(vect, layout)))
            | Heap t => Layout.fill [ layout t, Layout.str " heap" ]

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
            | Heap t => 0wx11 * hash t

      (* Equality, accounting for equating, so it may become more coarse over time *)
      fun equated (t1, t2) =
         case (t1, t2) of
              (Unchanged ty1, Unchanged ty2) => Type.equals (ty1, ty2)
            | (Fresh eq1, Fresh eq2) => Equatable.equals (eq1, eq2)
            | (Tuple tup1, Tuple tup2) => Vector.equals (tup1, tup2, equated)
            | (Heap t1, Heap t2) => equated (t1, t2)
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
            val _ = List.foreach (!cons2, fn conData as ConData (con1, args1) =>
               let
                  val found = List.peek (!cons1, fn ConData (con2, _) =>
                     Con.equals (con1, con2))
               in
                  case found of
                       SOME (ConData (con2, args2)) => Vector.foreach2(args1, args2, coerce)
                     | NONE => cons1 := conData :: !cons1
               end)
         in
            (tycon, cons1)
         end
      and coerce (from, to) =
         case (from, to) of
              (Fresh a, Fresh b) => Equatable.equate (a, b, mergeFresh)
            | (Tuple a, Tuple b) => Vector.foreach2 (a, b, coerce)
            | (Unchanged t1, Unchanged t2) =>
                 if Type.equals(t1, t2)
                 then ()
                 else Error.bug "SplitTypes.TypeInfo.coerce: Bad merge of unchanged types"
            | (Heap t1, Heap t2) =>
                 coerce (t1, t2)
            | _ =>
                 Error.bug (Layout.toString (Layout.fill [
                 Layout.str "SplitTypes.TypeInfo.coerce: Strange coercion: ",
                 layout from, Layout.str " coerced to ", layout to ]))
      fun fromType (ty: Type.t) =
         case Type.dest ty of
              Type.Datatype tycon =>
                  if Tycon.equals (tycon, Tycon.bool)
                  then Unchanged ty
                  else Fresh (Equatable.new (SOME tycon, ref []))
            | Type.Tuple ts => Tuple (Vector.map(ts, fromType))
            | Type.Array t => Heap (fromType t)
            | Type.Ref t => Heap (fromType t)
            | Type.Vector t => Heap (fromType t)
            | Type.Weak t => Heap (fromType t)
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
         (* TODO: Choose type from tuple instead *)
         fun fromTuple { offset, tuple, resultType} =
            case tuple of
                 TypeInfo.Tuple ts => Vector.sub (ts, offset)
               | _ => Error.bug "SplitTypes.transform.fromTuple: Tried to select from non-tuple info"
         fun primApp {args, prim, resultType, resultVar = _, targs} =
            let
               fun derefPrim args =
                  case Vector.sub (args, 0) of
                       TypeInfo.Heap t => t
                     | _ => Error.bug "SplitTypes.transform.primApp: Strange deref"
               fun refPrim args = TypeInfo.Heap (Vector.sub (args, 0))
               fun assignPrim args = let
                  val _ = TypeInfo.coerce (Vector.sub (args, 0), TypeInfo.Heap (Vector.sub (args, 1)))
               in
                  TypeInfo.fromType Type.unit
               end
               fun updatePrim args = let
                  val _ = TypeInfo.coerce (Vector.sub (args, 0), TypeInfo.Heap (Vector.sub (args, 2)))
               in
                  TypeInfo.fromType resultType
               end
               val result =
                  case Prim.name prim of
                       Prim.Name.Array_sub => derefPrim args
                     | Prim.Name.Array_toArray => Vector.sub (args, 0)
                     | Prim.Name.Array_toVector => Vector.sub (args, 0)
                     | Prim.Name.Array_update => updatePrim args
                     | Prim.Name.Ref_ref => refPrim args
                     | Prim.Name.Ref_deref => derefPrim args
                     | Prim.Name.Ref_assign => assignPrim args
                     | Prim.Name.Vector_sub => derefPrim args
                     | Prim.Name.Vector_vector => refPrim args
                     | Prim.Name.Weak_get => refPrim args
                     | Prim.Name.Weak_new => derefPrim args
                     | _ => TypeInfo.fromType resultType
            in
               result
            end
         val { value, func, label } =
            analyze
            { coerce = fn {from, to} => TypeInfo.coerce (from, to),
              conApp = fromCon,
              const = TypeInfo.const,
              filter = fn (ty, con, args) => TypeInfo.coerce (ty, TypeInfo.fromCon {con=con,args=args}),
              filterWord = fn _ => (),
              fromType = fromType,
              layout = TypeInfo.layout,
              primApp = primApp,
              program = program,
              select = fromTuple,
              tuple = TypeInfo.fromTuple,
              useFromTypeOnBinds = true }

         (* value, old type, new type *)
         (* old type mostly to verify consistency *)
         val tyMap : (TypeInfo.t * Type.t * Type.t) HashSet.t =
            HashSet.new {hash = fn (v, oldTy, newTy) => TypeInfo.hash v}

         fun makeTy ty value =
            case value of
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
                         | _ => Error.bug "SplitTypes.transform.makeTy: TypeInfo was Tuple but type wasn't")
                | TypeInfo.Heap typeInfo' =>
                     (case Type.dest ty of
                           Type.Array oldTy' => Type.array (getTy (oldTy', typeInfo'))
                         | Type.Ref oldTy' => Type.reff (getTy (oldTy', typeInfo'))
                         | Type.Weak oldTy' => Type.weak (getTy (oldTy', typeInfo'))
                         | Type.Vector oldTy' => Type.vector (getTy (oldTy', typeInfo'))
                         | _ => Error.bug "SplitTypes.transform.makeTy: TypeInfo was Heap but type wasn't Ref/Weak/Vector")
         and getTy (oldTy, typeInfo) =
            let
               val (_, oldTy', newTy) =
                  HashSet.lookupOrInsert (tyMap, TypeInfo.hash typeInfo,
                     fn (t', oldTy, newTy) =>
                        TypeInfo.equated (typeInfo, t'),
                     fn () =>
                        (typeInfo, oldTy, makeTy oldTy typeInfo))
               val _ =
                  if (not (Type.equals (oldTy, oldTy'))) andalso false
                  then Error.bug (Layout.toString (Layout.fill [
                       Layout.str "SplitTypes.TypeInfo.coerce: Inconsistent types from analysis: ",
                       TypeInfo.layout typeInfo, Layout.str " unified with ",
                       Type.layout oldTy, Layout.str " and ", Type.layout oldTy']))
                  else ()
            in
               newTy
            end


         fun remappedConsHash (oldCon, ty) = Type.hash ty + 0wx11 * Con.hash oldCon
         val remappedCons: (Con.t * Type.t * Con.t) HashSet.t =
            HashSet.new {hash = fn (oldCon, ty, _) => remappedConsHash (oldCon, ty)}
         fun remapCon (oldCon, newTy) =
            if Con.equals (oldCon, Con.truee) orelse Con.equals (oldCon, Con.falsee)
            then oldCon
            else #3 (HashSet.lookupOrInsert (remappedCons, remappedConsHash (oldCon, newTy),
               fn (con2, ty2, _) => Type.equals (newTy, ty2) andalso Con.equals (oldCon, con2),
               fn () => (oldCon, newTy, Con.new oldCon)))

         fun loopExp (exp, newTy) =
            case exp of
                 Exp.ConApp {con, args} =>
                     let
                        val newCon = remapCon (con, newTy)
                     in
                        Exp.ConApp {con=newCon, args=args}
                     end
               (*| PrimApp {prim, targs, args} =>
                  PrimApp {prim=loopPrim prim, targs=*)
               | _ => exp
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
                      | SOME var => getTy (ty, value var)
               val newExp = loopExp (exp, newTy)
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


         fun mergeTyVectorOpt (oldTysOpt, tsOpt, name) =
            case (oldTysOpt, tsOpt) of
                 (NONE, NONE) => NONE
               | (SOME oldTys, SOME ts) =>
                    SOME (Vector.map2 (oldTys, ts, getTy))
               | _ => Error.bug ("SplitTypes.TypeInfo.coerce: Inconsistent " ^ name)
         val globals =
            Vector.map(globals, loopStatement)
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

         (* this must occur after the loops to ensure that we can find types correctly.
          * The con remapping is shared though *)
         fun reifyCon newTy (TypeInfo.ConData (con, ts)) =
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
               val newCon = remapCon (con, newTy)
            in
               {con=newCon, args=Vector.map (ts, hardLookup)}
            end
         fun reifyCons newTy conList =
            Vector.fromList (List.map (conList, reifyCon newTy))
         val datatypes =
            let
               fun hash (ty, oldCon) = Type.hash ty + 0wx11 * Con.hash oldCon
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
                                          {cons=reifyCons newTy (!consRef), tycon=Type.deDatatype newTy }))
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
