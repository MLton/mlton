(* Copyright (C) 2018 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
functor SplitTypes(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

open S

structure Value = struct
  (* Cons won't be typed inside the tree, so we'll lazily denote them with Con
     and assume they'll eventually be equated with a type *)
  datatype z =
      Con of Con.t
    | Ty of Type.t
  datatype t =
      Unchanged of Type.t (* leave primitive types unchanged *)
    | Fresh of z Equatable.t (* make a fresh type for constructors *)
    | Tuple of t vector
  fun layoutZ (z: z) = case z of
      Con c => Con.layout c
    | Ty t => Type.layout t
  fun layout (t: t) = case t of
      Unchanged t => Type.layout t
    | Fresh eq => Equatable.layout (eq, layoutZ)
    | Tuple vect => Vector.layout layout vect

  fun pickZ (first, second) = first
  fun coerce {from, to} = case (from, to) of
      (Fresh a, Fresh b) => Equatable.equate (a, b, pickZ)
    | (Tuple a, Tuple b) => let val _ = Vector.map2 (a, b, 
        fn (a', b') => coerce {from=a', to=b'}) in () end
    | (Unchanged t, Unchanged _) => ()
    | _ => Layout.print (Layout.fill [ Layout.str "SplitTypes.Value.coerce: Strange coercion: ",
              layout from, layout to ], Error.bug)

  (* TODO: Non-constructor types should always be equated *)
  (* delayed types will be dropped, reducing memory in some cases *)
  fun fromType (ty: Type.t) = Fresh (Equatable.delay (fn () => Ty ty))
  fun fromCon (con: Con.t) = Fresh (Equatable.delay (fn () => Con con))
  fun fromTuple (vect: t vector) = Tuple vect
  fun const (c: Const.t): t = fromType (Type.ofConst c)
end

fun transform (program as Program.T {datatypes, globals, functions, main}): Program.t =
  let
    val { value, func, label } = analyze
      { coerce = Value.coerce,
        conApp = fn { con: Con.t, ...} => Value.fromCon con,
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
