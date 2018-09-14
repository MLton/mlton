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
                 | Fresh of (Tycon.t * con list ref) Equatable.t
                 | Tuple of t vector
                 (* singular constructor, will need to be merged into another type *)
                 | Con of con
      and con = ConData of Con.t * (t vector)

      fun layoutCon (ConData (con, ts)) =
         Layout.fill [ Con.layout con, Vector.layout layout ts ]
      and layoutFresh (ty, cons) =
         Layout.fill [ Tycon.layout ty, Layout.str " # " ]
      and layout (t: t) =
         case t of
              Unchanged t => Type.layout t
            | Fresh eq => Equatable.layout (eq, layoutFresh)
            | Tuple vect => Vector.layout layout vect
            | Con (ConData (con, _)) => Con.layout con

      fun mergeFresh ((tycon1, cons1), (tycon2, cons2)) =
         if Tycon.equals (tycon1, tycon2)
            then (tycon1, (cons1 := List.append (!cons1, !cons2) ; cons1))
            else Error.bug "SplitTypes.Value.mergeFresh: Merged different types"
      fun mergeConIntoType (conData as ConData (con, args), eq) =
         (case Equatable.value eq of
               (_, cons) =>
               (* if a con isn't present, then push it, else just merge all
                * the arguments together instead *)
                (case List.peek (!cons, fn (ConData (con2, _)) => Con.equals (con, con2)) of
                      NONE => (cons := conData :: !cons)
                    | SOME (ConData (_, args2)) => (Vector.map2 (args,
                      args2, coerce) ; ())))
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
            | (Con conData, Fresh eq) =>
               mergeConIntoType (conData, eq)
            | (Fresh eq, Con conData) =>
               mergeConIntoType (conData, eq)
            (*| (Fresh (ty, cons), Con con) => cons := con :: !cons*)
            | (Con (ConData (con1, args1)), Con (ConData (con2, args2))) =>
                 if Con.equals (con1, con2)
                 then (Vector.map2 (args1, args2, coerce) ; ())
                 else () (* do nothing, this can happen for instance if a
                 constant passes into a case statement *)
            | _ =>
                 Error.bug (Layout.toString (Layout.fill [
                 Layout.str "SplitTypes.Value.coerce: Strange coercion: ",
                 layout from, Layout.str " coerced to ", layout to ]))

            (* TODO: Non-constructor types should always be equated *)
            (* delayed types will be dropped, reducing memory in some cases *)
      fun fromType (ty: Type.t) =
         case Type.dest ty of
              Type.Datatype tycon => Fresh (Equatable.new (tycon, ref []))
            | Type.Tuple ts => Tuple (Vector.map(ts, fromType))
            | _ => Unchanged ty
      fun fromCon {con: Con.t, args: t vector} = Con (ConData (con, args))
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
       fun makeTy value =
          (case value of
               Value.Unchanged ty => ty
             | Value.Fresh eq => (case Equatable.value eq of (tycon, cons) => Type.datatypee tycon)
             | Value.Tuple ts => Type.tuple (Vector.map (ts, makeTy))
             | Value.Con _ => Error.bug "SplitTypes.makeTy: type Con appeared in final result")
       val globals =
          Vector.map(globals,
          fn st as Statement.T {exp, ty, var=varopt} =>
             (case varopt of
                   NONE => st
                 | SOME var => Statement.T {exp=exp, ty=makeTy (value var), var=varopt}))
       val program =
          Program.T
          { datatypes = datatypes,
            globals = globals,
            functions = functions,
            main = main }
    in program end
 end
