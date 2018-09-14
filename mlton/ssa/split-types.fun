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
                 | Fresh of (Type.t * con list ref) Equatable.t
                 | Tuple of t vector
                 (* singular constructor, will need to be merged into another type *)
                 | Con of con
      and con = ConData of Con.t * (t vector)

      fun layoutCon (ConData (con, ts)) =
         Layout.fill [ Con.layout con, Vector.layout layout ts ]
      and layoutFresh (ty, cons) =
         Layout.fill [ Type.layout ty, Layout.str " # " ]
      and layout (t: t) =
         case t of
              Unchanged t => Type.layout t
            | Fresh eq => Equatable.layout (eq, layoutFresh)
            | Tuple vect => Vector.layout layout vect
            | Con (ConData (con, _)) => Con.layout con

      fun mergeFresh ((ty1, cons1), (ty2, cons2)) =
         if Type.equals (ty1, ty2)
            then (ty1, (cons1 := List.append (!cons1, !cons2) ; cons1))
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
            | _ =>
                 Error.bug (Layout.toString (Layout.fill [
                 Layout.str "SplitTypes.Value.coerce: Strange coercion: ",
                 layout from, layout to ]))

            (* TODO: Non-constructor types should always be equated *)
            (* delayed types will be dropped, reducing memory in some cases *)
      fun fromType (ty: Type.t) = Fresh (Equatable.new (ty, ref []))
      fun fromCon {con: Con.t, args: t vector} = 
         case con of Con (ConData (con, args))
      fun fromTuple (vect: t vector) = Tuple vect
      fun const (c: Const.t): t = fromType (Type.ofConst c)
end

 fun transform (program as Program.T {datatypes, globals, functions, main}): Program.t =
    let
       val { value, func, label } = analyze
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
    in
      Program.T {
      datatypes = datatypes,
      globals = globals,
      functions = functions,
      main = main}
    end
 end
