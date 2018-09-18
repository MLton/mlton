(* Copyright (C) 2018 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
functor SplitTypes(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

   open S

   structure Value = struct
      datatype t = Unchanged of Type.t
      (* each other type has a constructor set with arguments to coerce *)
                 | Fresh of (Tycon.t option * con list ref) Equatable.t
                 | Tuple of t vector
      and con = ConData of Con.t * (t vector)

      fun layoutCon (ConData (con, ts)) =
         Layout.fill [ Con.layout con, Vector.layout layout ts ]
      and layoutFresh (ty, cons) =
         Layout.fill [ Option.layout Tycon.layout ty, Layout.str " # " ]
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
                 else Error.bug "SplitTypes.Value.coerce: Bad merge of unchanged types"
            | _ =>
                 Error.bug (Layout.toString (Layout.fill [
                 Layout.str "SplitTypes.Value.coerce: Strange coercion: ",
                 layout from, Layout.str " coerced to ", layout to ]))

                 (* TODO: Non-constructor types should always be equated *)
                 (* delayed types will be dropped, reducing memory in some cases *)
      fun fromType (ty: Type.t) =
         case Type.dest ty of
              Type.Datatype tycon => Fresh (Equatable.new (SOME tycon, ref []))
            | Type.Tuple ts => Tuple (Vector.map(ts, fromType))
            | _ => Unchanged ty
      fun fromCon {con: Con.t, args: t vector} = Fresh (Equatable.delay (fn ()
         => (NONE, ref [ConData (con, args)])))
      fun fromTuple (vect: t vector) = Tuple vect
      fun const (c: Const.t): t = fromType (Type.ofConst c)
                 end

   fun transform (program as Program.T {datatypes, globals, functions, main}): Program.t =
      let
         val { value, func, label } =
            analyze
            { coerce = fn {from, to} => Value.coerce (from, to),
              conApp = Value.fromCon,
              const = Value.const,
              filter = fn _ => (),
              filterWord = fn _ => (),
              fromType = Value.fromType,
              layout = Value.layout,
              primApp = fn { resultType: Type.t, ...} => Value.fromType resultType,
              program = program,
              select = fn { resultType: Type.t, ...} => Value.fromType resultType,
              tuple = Value.fromTuple,
              useFromTypeOnBinds = false }
         fun makeTy ty value =
            (case value of
                  Value.Unchanged ty => ty
                | Value.Fresh eq =>
                     (case Equatable.value eq of
                           (SOME tycon, cons) => Type.datatypee (Tycon.new tycon)
                         | (NONE, _) =>
                              (case Type.dest ty of
                                    Type.Datatype tycon => Type.datatypee (Tycon.new tycon)
                                  | _ => ty))
                                  | Value.Tuple ts => Type.tuple (Vector.map (ts, makeTy ty)))
         (* value, old type, new type *)
         (* old type mostly to verify consistency *)
         val tyMap : (Value.t * Type.t * Type.t) HashSet.t =
            HashSet.new {hash = fn (v, oldTy, newTy) => Value.hash v}

         fun getTy (oldTy, t) =
            let
               val (_, oldTy', newTy) =
                  HashSet.lookupOrInsert (tyMap, Value.hash t,
                     fn (t', oldTy, newTy) =>
                        Value.equated (t, t'),
                     fn () =>
                        (t, oldTy, makeTy oldTy t))
               val _ =
                  if not (Type.equals (oldTy, oldTy'))
                  then Error.bug (Layout.toString (Layout.fill [
                       Layout.str "SplitTypes.Value.coerce: Inconsistent types from analyse: ",
                       Type.layout oldTy, Layout.str " already used from ", Type.layout oldTy']))
                  else ()
            in
               newTy
            end

         val globals =
            Vector.map(globals,
            fn st as Statement.T {exp, ty, var=varopt} =>
               (case varopt of
                     NONE => st
                   | SOME var => Statement.T {exp=exp, ty=getTy (ty, (value var)), var=varopt}))

         fun mergeTyVectorOpt (oldTysOpt, tsOpt, name) =
            case (oldTysOpt, tsOpt) of
                 (NONE, NONE) => NONE
               | (SOME oldTys, SOME ts) =>
                    SOME (Vector.map2 (oldTys, ts, getTy))
               | _ => Error.bug ("SplitTypes.Value.coerce: Inconsistent " ^ name)
         fun loopBlock block = block
         val functions =
            List.map(functions, fn f =>
               let
                  val { args, blocks, mayInline, name, raises, returns, start } =
                     Function.dest f
                  val { args = argTys, raises = raiseTys, returns = returnTys } = func name
               in
                  Function.new
                  { args = Vector.map2 (args, argTys, fn ((v, oldTy), t) => (v, getTy (oldTy, t))),
                    blocks = Vector.map (blocks, loopBlock),
                    mayInline = mayInline,
                    name = name,
                    raises = mergeTyVectorOpt (raises, raiseTys, "raises"),
                    returns = mergeTyVectorOpt (returns, returnTys, "returns"),
                    start = start
                    }
               end)
         val program =
            Program.T
            { datatypes = datatypes,
              globals = globals,
              functions = functions,
              main = main }
            in program end
      end
