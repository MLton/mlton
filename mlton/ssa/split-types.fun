(* Copyright (C) 2018 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
functor SplitTypes(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

   open S

   structure TypeInfo = struct
      datatype heapType = Array | Ref | Vector | Weak
      datatype t = Unchanged of Type.t
                 | Fresh of (Tycon.t option * con list ref) Equatable.t
                 | Tuple of t vector
                 | Heap of (t * heapType)
      and con = ConData of Con.t * (t vector)

      fun layoutFresh (ty, cons) =
         Layout.fill [ Option.layout Tycon.layout ty, Layout.str " # ",
                       Ref.layout (List.layout (fn ConData (con, _) => Con.layout con)) cons]
      and layout (t: t) =
         case t of
              Unchanged t => Type.layout t
            | Fresh eq => Equatable.layout (eq, layoutFresh)
            | Tuple vect => Layout.tuple (Vector.toList (Vector.map(vect, layout)))
            | Heap (t,_) => Layout.fill [ layout t, Layout.str " heap" ]

      (* lattice join of options, where NONE is taken as less than SOME a,
       * with a fallback method if the join would be inconsistent
       *
       * if SOME x and SOME y and equals (x, y) then return SOME x (which is SOME y)
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
            | Fresh eq => Option.fold (#1 (Equatable.value eq), 0w0,
                  fn (tycon, h) => Hash.combine (h, Tycon.hash tycon))
            | Tuple vect => Hash.vectorMap(vect, hash)
            | Heap (t,htype) => Hash.combine (hash t,
               (case htype of
                    Array => 0w0
                  | Ref => 0w1
                  | Vector => 0w2
                  | Weak => 0w3))

      (* The equality of types becomes more coarse during analysis,
       * so it may be unsafe to use as equality *)
      fun equated (t1, t2) =
         case (t1, t2) of
              (Unchanged ty1, Unchanged ty2) => Type.equals (ty1, ty2)
            | (Fresh eq1, Fresh eq2) => Equatable.equals (eq1, eq2)
            | (Tuple tup1, Tuple tup2) => Vector.equals (tup1, tup2, equated)
            | (Heap (t1, _), Heap (t2, _)) => equated (t1, t2)
            | _ => false

      fun mergeFresh coerceList ((tycon1, cons1), (tycon2, cons2)) =
         let
            val tycon = (optionJoin (tycon1, tycon2, Tycon.equals,
               fn (_, _) => Error.bug "splitTypes.TypeInfo.mergeFresh: Inconsistent tycons"))
            val _ = List.foreach (!cons2, fn conData as ConData (con2, args2) =>
               let
                  val found = List.peek (!cons1, fn ConData (con1, _) =>
                     Con.equals (con1, con2))
               in
                  case found of
                       SOME (ConData (_, args1)) => coerceList := (args1, args2) :: !coerceList
                     | NONE => cons1 := conData :: !cons1
               end)
         in
            (tycon, cons1)
         end
      fun coerce (from, to) =
         case (from, to) of
              (Fresh a, Fresh b) =>
              let
                 (* in some situations, recursive data types may cause lost updates
                  * so we need to completely finish the coercion before we recurse *)
                 val coerceList = ref []
                 val result = Equatable.equate (a, b, mergeFresh coerceList)
                 val _ = List.foreach(!coerceList, fn (args1, args2) =>
                    Vector.foreach2(args1, args2, coerce))
              in
                 result
              end
            | (Tuple a, Tuple b) => Vector.foreach2 (a, b, coerce)
            | (Unchanged t1, Unchanged t2) =>
                 if Type.equals(t1, t2)
                 then ()
                 else Error.bug "SplitTypes.TypeInfo.coerce: Bad merge of unchanged types"
            | (Heap (t1, _), Heap (t2, _)) =>
                 coerce (t1, t2)
            | _ =>
                 Error.bug (Layout.toString (Layout.fill [
                 Layout.str "SplitTypes.TypeInfo.coerce: Strange coercion: ",
                 layout from, Layout.str " coerced to ", layout to ]))
      fun fromType (ty: Type.t) =
         case Type.dest ty of
              Type.Datatype tycon => Fresh (Equatable.new (SOME tycon, ref []))
            | Type.Tuple ts => Tuple (Vector.map(ts, fromType))
            | Type.Array t => Heap (fromType t, Array)
            | Type.Ref t => Heap (fromType t, Ref)
            | Type.Vector t => Heap (fromType t, Vector)
            | Type.Weak t => Heap (fromType t, Weak)
            | _ => Unchanged ty
      fun fromCon {con: Con.t, args: t vector} =
         Fresh (Equatable.new (NONE, ref [ConData (con, args)]))
      fun fromTuple (vect: t vector) = Tuple vect
      fun const (c: Const.t): t = fromType (Type.ofConst c)
  end

fun transform (program as Program.T {datatypes, globals, functions, main}) =
   let
      fun fromCon {con: Con.t, args: TypeInfo.t vector} =
            TypeInfo.fromCon {con=con,args=args}
      fun fromType (ty: Type.t) =
            TypeInfo.fromType ty
      fun fromTuple { offset, tuple, resultType=_} =
         case tuple of
              TypeInfo.Tuple ts => Vector.sub (ts, offset)
            | _ => Error.bug "SplitTypes.transform.fromTuple: Tried to select from non-tuple info"

      (* primitives may return this boolean *)
      val primBoolTy = Type.datatypee Tycon.bool
      val primBoolInfo = TypeInfo.fromType (Type.datatypee Tycon.bool)
      val _ = List.map ([Con.truee, Con.falsee], fn con =>
         TypeInfo.coerce (TypeInfo.fromCon {con=con, args=Vector.new0 ()}, primBoolInfo))

      fun primApp {args, prim, resultType, resultVar = _, targs} =
         let
            fun derefPrim args =
               case Vector.sub (args, 0) of
                    TypeInfo.Heap (t, _) => t
                  | _ => Error.bug "SplitTypes.transform.primApp: Strange deref"
            fun refPrim heapType args = TypeInfo.Heap (Vector.sub (args, 0), heapType)
            fun assignPrim heapType args = let
               val _ = TypeInfo.coerce (Vector.sub (args, 0), TypeInfo.Heap (Vector.sub (args, 1), heapType))
            in
               TypeInfo.fromType Type.unit
            end
            fun updatePrim heapType args = let
               val _ = TypeInfo.coerce (Vector.sub (args, 0), TypeInfo.Heap (Vector.sub (args, 2), heapType))
            in
               Vector.sub (args, 0)
            end
            fun equalPrim args =
               let
                  val _ = TypeInfo.coerce (Vector.sub (args, 0), Vector.sub (args, 1))
               in
                  primBoolInfo
               end
            fun default () =
              if case Type.dest resultType of
                      Type.Datatype tycon => Tycon.equals (tycon, Tycon.bool)
                    | _ => false
                 then primBoolInfo
                 else TypeInfo.fromType resultType

         in
            case Prim.name prim of
                 Prim.Name.Array_sub => derefPrim args
               | Prim.Name.Array_toArray => Vector.sub (args, 0)
               | Prim.Name.Array_toVector => Vector.sub (args, 0)
               | Prim.Name.Array_update => updatePrim TypeInfo.Array args
               | Prim.Name.Ref_ref => refPrim TypeInfo.Ref args
               | Prim.Name.Ref_deref => derefPrim args
               | Prim.Name.Ref_assign => assignPrim TypeInfo.Ref args
               | Prim.Name.Vector_sub => derefPrim args
               | Prim.Name.Vector_vector => TypeInfo.Heap
                  let
                     val ty = TypeInfo.fromType (Vector.sub (targs, 0))
                     val _ = Vector.fold (args, ty, fn (a, b) => (TypeInfo.coerce (a, b); a) )
                  in
                     (ty, TypeInfo.Vector)
                  end
               | Prim.Name.Weak_get => derefPrim args
               | Prim.Name.Weak_new => refPrim TypeInfo.Weak args
               | Prim.Name.MLton_equal => equalPrim args
               | Prim.Name.MLton_eq => equalPrim args
               | Prim.Name.FFI (CFunction.T {args=cargs, ...}) =>
                  let
                     (* for the C methods, we need false -> 0 and true -> 1 so they have to remain bools *)
                     val _ = Vector.map2 (args, cargs, fn (arg, carg) =>
                       if Type.equals (carg, primBoolTy)
                         then TypeInfo.coerce (arg, primBoolInfo)
                         else ())
                  in
                    default ()
                  end
               | _ => default ()
         end
      val { value, func, ... } =
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

      val tyMap : (TypeInfo.t, Type.t) HashTable.t =
         HashTable.new {hash=TypeInfo.hash, equals=TypeInfo.equated}

      (* Always map the prim boolean to bool *)
      val _ = HashTable.lookupOrInsert (tyMap, primBoolInfo, fn () => primBoolTy)

      fun getTy typeInfo =
         let
            fun pickTycon (tycon, cons) =
               case (Tycon.equals (tycon, Tycon.bool), !Control.splitTypesBool) of
                    (true, Control.Never) => tycon
                  | (true, Control.Smart) => if List.length (!cons) < 2 then Tycon.new tycon else tycon
                  | _             => Tycon.new tycon
            fun makeTy eq =
               case Equatable.value eq of
                     (SOME tycon, cons) => Type.datatypee (pickTycon (tycon, cons))
                   | (NONE, _) =>
                        Error.bug (Layout.toString (Layout.fill [
                           Layout.str "SplitTypes.transform.makeTy: ", TypeInfo.layout
                              (TypeInfo.Fresh eq)]))
         in
            case typeInfo of
                  TypeInfo.Unchanged ty => ty
                | TypeInfo.Fresh eq =>
                     HashTable.lookupOrInsert (tyMap, typeInfo, fn () => makeTy eq)
                | TypeInfo.Tuple typeInfos =>
                     Type.tuple (Vector.map (typeInfos, getTy))
                | TypeInfo.Heap (typeInfo', heapType) =>
                     (case heapType of
                           TypeInfo.Array => Type.array (getTy typeInfo')
                         | TypeInfo.Ref => Type.reff (getTy typeInfo')
                         | TypeInfo.Weak => Type.weak (getTy typeInfo')
                         | TypeInfo.Vector => Type.vector (getTy typeInfo'))
         end


      fun remappedConsHash (oldCon, ty) = Hash.combine(Type.hash ty, Con.hash oldCon)
      val remappedCons: ((Con.t * Type.t), Con.t) HashTable.t =
         HashTable.new {hash=remappedConsHash, equals=fn ((con1, ty1), (con2, ty2)) =>
            Type.equals (ty1, ty2) andalso Con.equals (con1, con2)}
      fun remapCon (oldCon, newTy) =
         if Type.equals (newTy, primBoolTy)
         then oldCon
         else HashTable.lookupOrInsert (remappedCons, (oldCon, newTy), fn () => Con.new oldCon)


      (* Loop over the entire program, map each type to the new type,
       * and each constructor to the new constructor *)
      fun loopExp (exp, newTy) =
         case exp of
              Exp.ConApp {con, args} =>
                  let
                     val newCon = remapCon (con, newTy)
                  in
                     Exp.ConApp {con=newCon, args=args}
                  end
            | Exp.PrimApp {prim, args, ...} =>
                  let
                     val argTys = Vector.map (args, fn arg => getTy (value arg))
                     val newTargs = Prim.extractTargs (prim,
                        {args=argTys,
                         result=newTy,
                         typeOps = {deArray = Type.deArray,
                                    deArrow = fn _ => Error.bug "splitTypes.transform.loopExp: deArrow primApp",
                                    deRef = Type.deRef,
                                    deVector = Type.deVector,
                                    deWeak = Type.deWeak}})
                     val newPrim =
                        case Prim.name prim of
                           Prim.Name.FFI (cfunc as CFunction.T {args=_, return=_,
                                 convention, kind, prototype, symbolScope, target}) =>
                              let
                                 val newArgs = argTys
                                 val newReturn = newTy
                                 val newCFunc =
                                   case kind of
                                        CFunction.Kind.Runtime _ =>
                                           CFunction.T {args=newArgs, return=newReturn,
                                            convention=convention, kind=kind, prototype=prototype,
                                            symbolScope=symbolScope, target=target}
                                      | _ => cfunc
                              in
                                 Prim.ffi newCFunc
                              end
                         | _ => prim
                  in
                     Exp.PrimApp {prim=newPrim, targs=newTargs, args=args}
                  end
            | _ => exp
      fun loopStatement (Statement.T {exp, ty, var=varopt}) =
         let
            val newTy =
               case varopt of
                    NONE => ty
                  | SOME var => getTy (value var)
            val newExp = loopExp (exp, newTy)
         in
            Statement.T {exp=newExp, ty=newTy, var=varopt}
         end
      fun loopTransfer transfer =
         case transfer of
              Transfer.Case {cases, test, default} =>
                  (case cases of
                     Cases.Con cases' =>
                        let
                           val newTy = getTy (value test)
                           val newCases = Cases.Con (Vector.map
                              (cases',
                               fn (con, label) => (remapCon (con, newTy), label)))
                           (* if the cases are now exhaustive, default needs to be removed *)
                           val newDefault =
                              case (default, value test) of
                                   (SOME _, TypeInfo.Fresh eq) =>
                                       let
                                          val cons = (! o #2 o Equatable.value) eq
                                       in
                                          if Vector.length cases' < List.length cons
                                             then default
                                             else NONE
                                       end
                                 | _ => default
                        in
                           Transfer.Case {cases=newCases, default=newDefault, test=test}
                        end
                   | Cases.Word _ => transfer)
            | _ => transfer
      fun loopBlock (Block.T { args, label, statements, transfer }) =
         let
            val newArgs = Vector.map (args, fn (var, _) => (var, getTy (value var)))
            val newStatements = Vector.map (statements, loopStatement)
            val newTransfer = loopTransfer transfer
         in
            Block.T {args=newArgs, label=label, statements=newStatements ,transfer=newTransfer}
         end


      val globals =
         Vector.map(globals, loopStatement)
      val functions =
         List.map(functions, fn f =>
            let
               val { args, blocks, mayInline, name, raises, returns, start } =
                  Function.dest f
               val { args = argTys, raises = raiseTys, returns = returnTys } = func name
               fun mergeTyVectorOpt (oldTysOpt, tsOpt, name) =
                  case (oldTysOpt, tsOpt) of
                       (NONE, NONE) => NONE
                     | (SOME _, SOME ts) =>
                          SOME (Vector.map (ts, getTy))
                     | _ => Error.bug ("SplitTypes.TypeInfo.coerce: Inconsistent " ^ name)
            in
               Function.new
               { args = Vector.map2 (args, argTys, fn ((v, _), typeInfo) => (v, getTy typeInfo)),
                 blocks = Vector.map (blocks, loopBlock),
                 mayInline = mayInline,
                 name = name,
                 raises = mergeTyVectorOpt (raises, raiseTys, "raises"),
                 returns = mergeTyVectorOpt (returns, returnTys, "returns"),
                 start = start
                 }
            end)


      (* This needs to run after looping since the types/constructors were
       * duplicated at usage in the loops above *)
      val numDatatypes = Vector.length (datatypes)
      val datatypes =
         let
            fun reifyCon newTy (TypeInfo.ConData (con, ts)) =
               let
                  val newCon =
                     if Type.equals (newTy, primBoolTy)
                     then con
                     else remapCon (con, newTy)
               in
                  {con=newCon, args=Vector.map (ts, getTy)}
               end
            fun reifyCons newTy conList =
               Vector.fromList (List.map (conList, reifyCon newTy))

            (* bool may appear multiple times depending on settings *)
            val primBoolDt = Datatype.T
              {cons=Vector.fromList
                [{con=Con.truee, args=Vector.new0 ()},
                 {con=Con.falsee, args=Vector.new0 ()}], tycon=Tycon.bool}
         in
            (Vector.fromList (primBoolDt :: (List.keepAllMap (HashTable.toList tyMap,
                  fn (typeInfo, newTy) =>
                     case typeInfo of
                          TypeInfo.Fresh eq =>
                              (* Only one copy of the true prim bool type should exist *)
                              (case (Type.equals (newTy, primBoolTy), Equatable.value eq) of
                                    (false, (_, consRef)) =>
                                       SOME (Datatype.T
                                       {cons=reifyCons newTy (!consRef), tycon=Type.deDatatype newTy})
                                  | _ => NONE)
                        | _ => NONE))))
         end

      val _ = Control.diagnostics
         (fn display =>
            let
               open Layout
            in
               display ( mayAlign [
                  str "Program before splitting had ",
                  str (Int.toString numDatatypes),
                  str " datatypes.",
                  str "Program after splitting has ",
                  str (Int.toString (Vector.length datatypes)),
                  str " datatypes."
               ])
            end )
      val program =
         Program.T
         { datatypes = datatypes,
           globals = globals,
           functions = functions,
           main = main }
   in program end
end