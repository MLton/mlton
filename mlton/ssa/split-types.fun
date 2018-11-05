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
            | Heap (t,_) => Layout.fill [ layout t, Layout.str " heap" ]

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
            | Fresh eq => Option.fold (#1 (Equatable.value eq), 0w0,
                  fn (tycon, h) => Hash.combine (h, Tycon.hash tycon))
            | Tuple vect => Hash.vectorMap(vect, hash)
            | Heap (t,htype) => Hash.combine (hash t,
               (case htype of
                    Array => 0w0
                  | Ref => 0w1
                  | Vector => 0w2
                  | Weak => 0w3))

      (* Equality, accounting for equating, so it may become more coarse over time *)
      fun equated (t1, t2) =
         case (t1, t2) of
              (Unchanged ty1, Unchanged ty2) => Type.equals (ty1, ty2)
            | (Fresh eq1, Fresh eq2) => Equatable.equals (eq1, eq2)
            | (Tuple tup1, Tuple tup2) => Vector.equals (tup1, tup2, equated)
            | (Heap (t1, _), Heap (t2, _)) => equated (t1, t2)
            | _ => false

      fun mergeFresh coerceList ((tycon1, cons1), (tycon2, cons2)) =
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
            val _ = Control.diagnostics
               (fn display =>
                     display ( Layout.fill [
                     Layout.str "Merge result is ",
                     Option.layout Tycon.layout tycon,
                     Layout.str " # ",
                     Ref.layout (List.layout layoutCon) cons1]))
         in
            (tycon, cons1)
         end
      and coerce (from, to) =
         case (from, to) of
              (Fresh a, Fresh b) =>
              let
                 (* in recursive situations we could get lost updates, so we have to be very
                  * careful here about the coercion order *)
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
              Type.Datatype tycon =>
                  if Tycon.equals (tycon, Tycon.bool)
                  then Unchanged ty
                  else Fresh (Equatable.new (SOME tycon, ref []))
            | Type.Tuple ts => Tuple (Vector.map(ts, fromType))
            | Type.Array t => Heap (fromType t, Array)
            | Type.Ref t => Heap (fromType t, Ref)
            | Type.Vector t => Heap (fromType t, Vector)
            | Type.Weak t => Heap (fromType t, Weak)
            | _ => Unchanged ty
      fun fromCon {con: Con.t, args: t vector} =
         if Con.equals (con, Con.truee) orelse Con.equals (con, Con.falsee)
            then Unchanged Type.bool
            else Fresh (Equatable.new (NONE, ref [ConData (con, args)]))
      fun fromTuple (vect: t vector) = Tuple vect
      fun const (c: Const.t): t = fromType (Type.ofConst c)
  end

fun dupGlobals (program as Program.T {datatypes, globals, functions, main}): Program.t =
   let
      val globalVars: (Var.t, Var.t list ref) HashTable.t
         = HashTable.new {hash=Var.hash, equals=Var.equals}
      fun freshenIfGlobal (var: Var.t) =
         case HashTable.peek (globalVars, var) of
              NONE => var
            | SOME vs =>
                 let
                    val newVar = Var.new var
                    val _ = vs := (newVar :: !vs)
                 in
                    newVar
                 end
      fun freshenVec (vars: Var.t vector) = Vector.map (vars, freshenIfGlobal)

      (* so what's going on here is that each *usage* of a global will be duplicated *)
      fun loopExp exp =
         case exp of
              Exp.ConApp {args, con} => Exp.ConApp {con=con, args= freshenVec args}
            | Exp.PrimApp {args, prim, targs} =>
                 Exp.PrimApp {args= freshenVec args, prim=prim, targs=targs}
            | Exp.Select {offset, tuple} =>
                 Exp.Select {offset=offset, tuple= freshenIfGlobal tuple}
            | Exp.Tuple vs => Exp.Tuple (freshenVec vs)
            | Exp.Var v => Exp.Var (freshenIfGlobal v)
            | _ => exp
      fun loopStatement (Statement.T {exp, ty, var}) =
         (* in the main program tree, the variable won't be a global, but we'll loop over
          * globals since some may depend on each other *)
         Statement.T {exp=loopExp exp, ty=ty, var=Option.map (var, freshenIfGlobal)}

      fun loopTransfer transfer =
         case transfer of
              Transfer.Arith {args, overflow, prim, success, ty} =>
                 Transfer.Arith {args= freshenVec args, overflow=overflow, prim=prim, success=success, ty=ty}
            | Transfer.Bug => Transfer.Bug
            | Transfer.Call {args, func, return} =>
                 Transfer.Call {args= freshenVec args, func=func, return=return}
            | Transfer.Case {cases, default, test} =>
                 Transfer.Case {cases=cases, default=default, test= freshenIfGlobal test}
            | Transfer.Goto {args, dst} =>
                 Transfer.Goto {args= freshenVec args, dst=dst}
            | Transfer.Raise vs =>
                 Transfer.Raise (freshenVec vs)
            | Transfer.Return vs =>
                 Transfer.Return (freshenVec vs)
            | Transfer.Runtime {args, prim, return} =>
                 Transfer.Runtime {args=freshenVec args, prim=prim, return=return}
      fun loopBlock (Block.T {args, label, statements, transfer}) =
         Block.T {args = args, label=label,
            statements = Vector.map (statements, loopStatement),
            transfer = loopTransfer transfer}
      fun loopFunction func =
         let
            val {args, blocks, mayInline, name, raises, returns, start} = Function.dest func
            val newBlocks = Vector.map(blocks, loopBlock)
         in
            Function.new {args=args, blocks=newBlocks, mayInline=mayInline, name=name,
               raises=raises, returns=returns, start=start}
         end

      val newFunctions = List.map (functions, loopFunction)

      (* We do not recurse on global definintions, so if g1 = C1 (g0) and g2 = C2 (g0), we
       * will not duplicate g0. We expect this shouldn't improve much over proper
       * duplication of single-depth usages *)
      val newGlobals =
         let
            fun globalToClones (Statement.T {exp, ty, var}) =
               let
                  val realVar = case var of
                     SOME v => v
                   | NONE => Error.bug "SplitTypes.dupGlobals.newGlobals: Global variable was NONE"
                  val newVars = case HashTable.peek (globalVars, realVar) of
                     SOME vs => !vs
                   (* it's possible a global is referenced only in globals, not in the program *)
                   | NONE => []
               in
                  (Vector.fromList o List.map) (newVars, fn var =>
                     Statement.T {
                        exp = exp,
                        var = SOME (Var.new var),
                        ty = ty })
               end
         in
            Vector.concat [globals, Vector.concatV (Vector.map (globals, globalToClones))]
         end
   in
      Program.T {datatypes=datatypes, globals=newGlobals, functions=newFunctions, main=main}
   end

fun transform program: Program.t =
   let
      val program  as Program.T {datatypes, globals, functions, main} = program

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
      fun fromTuple { offset, tuple, resultType} =
         case tuple of
              TypeInfo.Tuple ts => Vector.sub (ts, offset)
            | _ => Error.bug "SplitTypes.transform.fromTuple: Tried to select from non-tuple info"
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
               | Prim.Name.Weak_get => refPrim TypeInfo.Weak args
               | Prim.Name.Weak_new => derefPrim args
               | _ => TypeInfo.fromType resultType
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

      val tyMap : (TypeInfo.t, Type.t) HashTable.t =
         HashTable.new {hash=TypeInfo.hash, equals=TypeInfo.equated}


      fun getTy typeInfo =
         let
            fun makeTy eq =
               (case Equatable.value eq of
                     (SOME tycon, cons) => Type.datatypee (Tycon.new tycon)
                   | (NONE, cons) =>
                        Error.bug (Layout.toString (Layout.fill [
                           Layout.str "SplitTypes.transform.makeTy: ", TypeInfo.layout
                              (TypeInfo.Fresh eq)])))
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
         if Con.equals (oldCon, Con.truee) orelse Con.equals (oldCon, Con.falsee)
         then oldCon
         else HashTable.lookupOrInsert (remappedCons, (oldCon, newTy), fn () => Con.new oldCon)

      fun loopExp (exp, newTy) =
         case exp of
              Exp.ConApp {con, args} =>
                  let
                     val newCon = remapCon (con, newTy)
                  in
                     Exp.ConApp {con=newCon, args=args}
                  end
            | Exp.PrimApp {prim, targs, args} =>
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
                  in
                     Exp.PrimApp {prim=prim, targs=newTargs, args=args}
                  end
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
                   | SOME var => getTy (value var)
            val newExp = loopExp (exp, newTy)
         in
            Statement.T {exp=newExp, ty=newTy, var=varopt}
         end
      fun loopCases (cases, test) =
         case cases of
              Cases.Con cases' =>
                  Cases.Con (Vector.map (cases',
                     fn (con, label) =>
                        (remapCon (con, getTy (value test)), label)))
            | Cases.Word _ => cases
      fun loopTransfer transfer =
         case transfer of
              Transfer.Case {cases, test, default} =>
                  let
                    val newCases = loopCases (cases, test)
                  in
                     Transfer.Case {cases=newCases, test=test, default=default}
                  end
            | _ => transfer
      fun loopBlock (Block.T { args, label, statements, transfer }) =
         let
            val newArgs = Vector.map (args, fn (var, _) => (var, getTy (value var)))
            val newStatements = Vector.map (statements, loopStatement)
            val newTransfer = loopTransfer transfer
         in
            Block.T {args=newArgs, label=label, statements=newStatements ,transfer=newTransfer}
         end


      fun mergeTyVectorOpt (oldTysOpt, tsOpt, name) =
         case (oldTysOpt, tsOpt) of
              (NONE, NONE) => NONE
            | (SOME oldTys, SOME ts) =>
                 SOME (Vector.map (ts, getTy))
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
               { args = Vector.map2 (args, argTys, fn ((v, _), typeInfo) => (v, getTy typeInfo)),
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
            val newCon = remapCon (con, newTy)
         in
            {con=newCon, args=Vector.map (ts, getTy)}
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
               (List.keepAllMap (HashTable.toList tyMap,
                  fn (typeInfo, newTy) =>
                     case typeInfo of
                          TypeInfo.Fresh eq =>
                              (case Equatable.value eq of
                                    (_, consRef) =>
                                       SOME (Datatype.T
                                       {cons=reifyCons newTy (!consRef), tycon=Type.deDatatype newTy}))
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
