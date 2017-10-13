(* Copyright (C) 2017 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ShareZeroVec (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
  struct

    open S
    open Exp

    (* split a list of statements at the point where the test var is declared;
     * return (pre, post, match)
     *)
    fun split ((x as Statement.T {var, ...})::xs, test, pre) =
          if isSome var andalso Var.equals (valOf var, test)
          then (List.rev pre, xs, SOME x)
          else split (xs, test, x::pre)
      | split ([], _, pre) = (List.rev pre, [], NONE)


    fun transform (Program.T {datatypes, globals, functions, main}) =
      let
        val functions' =
          List.revMap
          (functions, fn f =>
            let
              val {args, blocks, mayInline, name, raises, returns, start} =
                  Function.dest f

              (* 1st pass: assemble a set of arrays to be frozen to vectors *)
              val funArrays =
                Vector.foldr
                (blocks,
                [],
                (fn (block as Block.T {statements, ...}, acc) =>
                  let
                    val blockArrays =
                      Vector.foldr
                      (statements,
                      [],
                      (fn (statement as Statement.T {exp, ...}, acc') =>
                        case exp of
                          PrimApp ({prim, args, ...}) =>
                            let
                              fun arg () = Vector.first args
                            in
                              case Prim.name prim of
                                  Array_toVector =>
                                    (arg ()) :: acc'
                                | _ => acc'
                            end
                        | _ => acc'))
                  in
                    blockArrays @ acc
                  end))

              (* TODO: 2nd iteration: new branching/merging *)

            in
              functions
            end)

      in
          Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = functions,
                    main = main}
      end

  end
