(* Copyright (C) 2018 Jason Carr, 2025 Matthew Fluet.
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
                 | Fresh of {cons: con list ref, hash: word, tycon: Tycon.t} Equatable.t
                 | Tuple of t vector
                 | Heap of (t * heapType)
      and con = ConData of Con.t * (t vector)

      fun layoutFresh {tycon=ty, cons=cons, ...} =
         Layout.fill [Tycon.layout ty, Layout.str " # ",
                      Ref.layout (List.layout (fn ConData (con, _) => Con.layout con)) cons]
      and layout (t: t) =
         case t of
              Unchanged t => Type.layout t
            | Fresh eq => Equatable.layout (eq, layoutFresh)
            | Tuple vect => Layout.tuple (Vector.toListMap (vect, layout))
            | Heap (t, ht) => Layout.fill [layout t,
                                           case ht of
                                                Array => Layout.str " array"
                                              | Ref => Layout.str " ref"
                                              | Vector => Layout.str " vector"
                                              | Weak => Layout.str " weak"]

      and hash (t : t) : word =
         case t of
              Unchanged ty => Type.hash ty
            | Fresh eq => #hash (Equatable.value eq)
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

      fun deFresh t =
         case t of
              Fresh eq => eq
            | _ => Error.bug "SplitTypes.TypeInfo.deFresh"

      fun mergeFresh coerceList ({tycon=tycon1, cons=cons1, hash=hash1}, {tycon=tycon2, cons=cons2, hash=_}) =
         let
            val tycon =
               if Tycon.equals (tycon1, tycon2)
               then tycon1
               else Error.bug "SplitTypes.TypeInfo.mergeFresh: Inconsistent tycons"
            val cons = ref (!cons1)
            val _ = List.foreach (!cons2, fn conData as ConData (con2, args2) =>
               let
                  val found = List.peek (!cons1, fn ConData (con1, _) =>
                     Con.equals (con1, con2))
               in
                  case found of
                       SOME (ConData (_, args1)) => List.push (coerceList, (args1, args2))
                     | NONE => List.push (cons, conData)
               end)
         in
            {tycon=tycon, cons=cons, hash=hash1}
         end
      fun coerce (from, to) =
         case (from, to) of
              (Fresh a, Fresh b) =>
              let
                 (* in some situations, recursive data types may cause lost updates
                  * so we need to completely finish the coercion before we recurse *)
                 val coerceList = ref []
                 val result = Equatable.equate (a, b, mergeFresh coerceList)
                 val _ = List.foreach (!coerceList, fn (args1, args2) =>
                    Vector.foreach2 (args1, args2, coerce))
              in
                 result
              end
            | (Tuple a, Tuple b) => Vector.foreach2 (a, b, coerce)
            | (Unchanged t1, Unchanged t2) =>
                 if Type.equals (t1, t2)
                 then ()
                 else Error.bug "SplitTypes.TypeInfo.coerce: Bad merge of unchanged types"
            | (Heap (t1, _), Heap (t2, _)) =>
                 coerce (t1, t2)
            | _ =>
                 Error.bug (Layout.toString (Layout.fill [
                 Layout.str "SplitTypes.TypeInfo.coerce: Strange coercion: ",
                 layout from, Layout.str " coerced to ", layout to ]))
      fun newFresh (tycon, cons) =
        Equatable.new {tycon=tycon, cons=ref cons, hash=Random.word ()}
      fun fromType (ty: Type.t) =
         case Type.dest ty of
              Type.Datatype tycon => Fresh (newFresh (tycon, []))
            | Type.Tuple ts => Tuple (Vector.map (ts, fromType))
            | Type.Array t => Heap (fromType t, Array)
            | Type.Ref t => Heap (fromType t, Ref)
            | Type.Vector t => Heap (fromType t, Vector)
            | Type.Weak t => Heap (fromType t, Weak)
            | _ => Unchanged ty
      fun fromCon {con: Con.t, args: t vector, tycon: Tycon.t} =
         Fresh (newFresh (tycon, [ConData (con, args)]))
      fun fromTuple (vect: t vector) = Tuple vect
      fun const (c: Const.t): t = fromType (Type.ofConst c)

      fun select {tuple, offset, resultType=_} =
         case tuple of
              Tuple ts => Vector.sub (ts, offset)
            | _ => Error.bug "SplitTypes.TypeInfo.select: Tried to select from non-tuple info"
  end

fun transform (program as Program.T {datatypes, globals, functions, main}) =
   let
      val conTyconMap =
         HashTable.new {hash=Con.hash, equals=Con.equals}
      fun conTycon con =
         HashTable.lookupOrInsert (conTyconMap, con, fn () =>
            Error.bug ("SplitTypes.transform.conTycon: " ^ (Con.toString con)))
      val _ =
         Vector.foreach (datatypes, fn Datatype.T {cons, tycon} =>
            Vector.foreach (cons, fn {con, ...} =>
               ignore (HashTable.lookupOrInsert (conTyconMap, con, fn () => tycon))))

      (* primitives may return this boolean *)
      val primBoolTy = Type.bool
      val primBoolTycon = Type.deDatatype primBoolTy
      val primBoolInfo = TypeInfo.fromType primBoolTy
      val _ = List.map ([Con.truee, Con.falsee], fn con =>
         TypeInfo.coerce (TypeInfo.fromCon {con=con, args=Vector.new0 (), tycon = primBoolTycon}, primBoolInfo))

      fun primApp {args, prim, resultType, resultVar=_, targs} =
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
               TypeInfo.fromType Type.unit
            end
            fun equalPrim args =
               let
                  val _ = TypeInfo.coerce (Vector.sub (args, 0), Vector.sub (args, 1))
               in
                  primBoolInfo
               end
            fun copyPrim args =
               let
                  val _ = TypeInfo.coerce (Vector.sub (args, 2), Vector.sub (args, 0))
               in
                  TypeInfo.fromType Type.unit
               end
            fun default () =
              if case Type.dest resultType of
                      Type.Datatype tycon => Tycon.equals (tycon, primBoolTycon)
                    | _ => false
                 then primBoolInfo
                 else TypeInfo.fromType resultType

         in
            case prim of
                 Prim.Array_array => TypeInfo.Heap
                  let
                     val ty = TypeInfo.fromType (Vector.sub (targs, 0))
                     val _ = Vector.foreach (args, fn a => TypeInfo.coerce (a, ty))
                  in
                     (ty, TypeInfo.Array)
                  end
               | Prim.Array_copyArray => copyPrim args
               | Prim.Array_copyVector => copyPrim args
               | Prim.Array_sub => derefPrim args
               | Prim.Array_toArray => Vector.sub (args, 0)
               | Prim.Array_toVector => Vector.sub (args, 0)
               | Prim.Array_update => updatePrim TypeInfo.Array args
               | Prim.Ref_ref => refPrim TypeInfo.Ref args
               | Prim.Ref_deref => derefPrim args
               | Prim.Ref_assign => assignPrim TypeInfo.Ref args
               | Prim.Vector_sub => derefPrim args
               | Prim.Vector_vector => TypeInfo.Heap
                  let
                     val ty = TypeInfo.fromType (Vector.sub (targs, 0))
                     val _ = Vector.foreach (args, fn a => TypeInfo.coerce (a, ty))
                  in
                     (ty, TypeInfo.Vector)
                  end
               | Prim.Weak_get => derefPrim args
               | Prim.Weak_new => refPrim TypeInfo.Weak args
               | Prim.MLton_equal => equalPrim args
               | Prim.MLton_eq => equalPrim args
               | Prim.CFunction (CFunction.T {args=cargs, ...}) =>
                  let
                     (* for the C methods, we need false -> 0 and true -> 1 so they have to remain bools *)
                     val _ = Vector.foreach2 (args, cargs, fn (arg, carg) =>
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
           conApp = fn {con, args} => TypeInfo.fromCon {con = con, args = args, tycon = conTycon con},
           const = TypeInfo.const,
           filter = fn (ty, con, args) => TypeInfo.coerce (ty, TypeInfo.fromCon {con=con, args=args, tycon = conTycon con}),
           filterWord = fn _ => (),
           fromType = TypeInfo.fromType,
           layout = TypeInfo.layout,
           primApp = primApp,
           program = program,
           select = TypeInfo.select,
           tuple = TypeInfo.fromTuple,
           useFromTypeOnBinds = true }

      val tyconMap =
         HashTable.new {hash=(#hash o Equatable.value), equals=Equatable.equals}

      (* Always map the prim boolean to bool *)
      val _ = HashTable.lookupOrInsert (tyconMap, TypeInfo.deFresh primBoolInfo, fn () => primBoolTycon)

      fun getTy typeInfo =
         let
            fun pickTycon {tycon, cons, hash = _} =
               case (Tycon.equals (tycon, primBoolTycon), !Control.splitTypesBool) of
                    (true, Control.Always) => Tycon.new tycon
                  | (true, Control.Never) => tycon
                  | (true, Control.Smart) => if List.length (!cons) < 2 then Tycon.new tycon else tycon
                  | (false, _) => Tycon.new tycon
            fun makeTy eq = pickTycon (Equatable.value eq)
         in
            case typeInfo of
                  TypeInfo.Unchanged ty => ty
                | TypeInfo.Fresh eq =>
                     Type.datatypee (HashTable.lookupOrInsert (tyconMap, eq, fn () => makeTy eq))
                | TypeInfo.Tuple typeInfos =>
                     Type.tuple (Vector.map (typeInfos, getTy))
                | TypeInfo.Heap (typeInfo', heapType) =>
                     (case heapType of
                           TypeInfo.Array => Type.array (getTy typeInfo')
                         | TypeInfo.Ref => Type.reff (getTy typeInfo')
                         | TypeInfo.Weak => Type.weak (getTy typeInfo')
                         | TypeInfo.Vector => Type.vector (getTy typeInfo'))
         end


      fun remappedConsHash (oldCon, tycon) = Hash.combine (Tycon.hash tycon, Con.hash oldCon)
      val remappedCons: ((Con.t * Tycon.t), Con.t) HashTable.t =
         HashTable.new {hash=remappedConsHash, equals=fn ((con1, tycon1), (con2, tycon2)) =>
            Tycon.equals (tycon1, tycon2) andalso Con.equals (con1, con2)}
      fun remapCon (oldCon, newTycon) =
         if Tycon.equals (newTycon, primBoolTycon)
         then oldCon
         else HashTable.lookupOrInsert (remappedCons, (oldCon, newTycon), fn () => Con.new oldCon)


      (* Loop over the entire program, map each type to the new type,
       * and each constructor to the new constructor *)
      fun loopExp (exp, newTy) =
         case exp of
              Exp.ConApp {con, args} =>
                  let
                     val newCon = remapCon (con, Type.deDatatype newTy)
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
                                    deArrow = fn _ => Error.bug "SplitTypes.transform.loopExp: deArrow primApp",
                                    deRef = Type.deRef,
                                    deVector = Type.deVector,
                                    deWeak = Type.deWeak}})
                     val newPrim =
                        case prim of
                           Prim.CFunction (cfunc as CFunction.T {args=_, return=_,
                                 convention, inline, kind, prototype, symbolScope, target}) =>
                              let
                                 val newArgs = argTys
                                 val newReturn = newTy
                                 val newCFunc =
                                   case kind of
                                        CFunction.Kind.Runtime _ =>
                                           CFunction.T {args=newArgs, return=newReturn,
                                            convention=convention, inline=inline, kind=kind, prototype=prototype,
                                            symbolScope=symbolScope, target=target}
                                      | _ => cfunc
                              in
                                 Prim.CFunction newCFunc
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
                           val newTycon = Type.deDatatype (getTy (value test))
                           val newCases = Cases.Con (Vector.map
                              (cases',
                               fn (con, label) => (remapCon (con, newTycon), label)))
                           (* if the cases are now exhaustive, default needs to be removed *)
                           val newDefault =
                              case (default, value test) of
                                   (SOME _, TypeInfo.Fresh eq) =>
                                       let
                                          val cons = (! o #cons o Equatable.value) eq
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
         Vector.map (globals, loopStatement)
      val functions =
         List.map (functions, fn f =>
            let
               val { args, blocks, mayInline, name, start, ... } =
                  Function.dest f
               val { args = argTys, raises = raiseTys, returns = returnTys } = func name
            in
               Function.new
               { args = Vector.map2 (args, argTys, fn ((v, _), typeInfo) => (v, getTy typeInfo)),
                 blocks = Vector.map (blocks, loopBlock),
                 mayInline = mayInline,
                 name = name,
                 raises = Option.map (raiseTys, fn tys => Vector.map (tys, getTy)),
                 returns = Option.map (returnTys, fn tys => Vector.map (tys, getTy)),
                 start = start }
            end)


      (* This needs to run after looping since the types/constructors were
       * duplicated at usage in the loops above *)
      val numOldDatatypes = Vector.length datatypes
      val datatypes =
         let
            fun reifyCon newTycon (TypeInfo.ConData (con, ts)) =
               let
                  val newCon = remapCon (con, newTycon)
               in
                  {con=newCon, args=Vector.map (ts, getTy)}
               end
            fun reifyCons (conList, newTycon) =
               Vector.fromList (List.map (conList, reifyCon newTycon))

            (* bool may appear multiple times depending on settings *)
            val primBoolDt = Datatype.T
              {cons=Vector.new2
                ({con=Con.truee, args=Vector.new0 ()},
                 {con=Con.falsee, args=Vector.new0 ()}), tycon=primBoolTycon}
         in
            (Vector.fromList (primBoolDt :: (List.keepAllMap (HashTable.toList tyconMap,
                  fn (eq, tycon) =>
                     (* Only one copy of the true prim bool type should exist *)
                     if Tycon.equals (tycon, primBoolTycon)
                     then NONE
                     else let
                             val {cons, ...} = Equatable.value eq
                          in
                             SOME (Datatype.T
                             {cons=reifyCons (!cons, tycon), tycon=tycon})
                          end))))
         end

      val _ = Control.diagnostics
         (fn display =>
            let
               open Layout
            in
               display ( mayAlign [
                  str "Program before splitting had ",
                  str (Int.toString numOldDatatypes),
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
