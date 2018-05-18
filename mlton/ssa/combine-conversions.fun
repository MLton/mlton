(* Copyright (C) 2009 Wesley W. Tersptra.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CombineConversions (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

open S

(*
 * This pass looks for and simplifies nested calls to (signed)
 * extension/truncation.
 *
 * It processes each block in dfs order (visiting defs before uses):
 *   If the statement is not a PrimApp Word_extdToWords, skip it.
 *   After processing a conversion, it tags the Var for subsequent
 *   use.
 *   When inspecting a conversion, check if the Var operand is also
 *   the result of a conversion.  If it is, try to combine the two
 *   operations.  Repeatedly simplify until hitting either a
 *   non-conversion Var or a case where the conversion cannot be
 *   simplified.
 *
 * The optimization rules are very simple:
 *    x1 : word<W1> = ...
 *    x2 : word<W2> = Word_extdToWord (W1, W2, {signed=s1}) x1
 *    x3 : word<W3> = Word_extdToWord (W2, W3, {signed=s2}) x2
 *
 *    If W1 = W2, then there is no conversions before x_1.
 *    This is guaranteed because W2 = W3 will always trigger optimization.
 *
 *    Case W1 <= W3 <= W2:
 *       x3 = Word_extdToWord (W1, W3, {signed=s1}) x1
 *    Case W1 <  W2 <  W3 AND (NOT s1 OR s2):
 *       x3 = Word_extdToWord (W1, W3, {signed=s1}) x1
 *    Case W1 =  W2 <  W3:
 *       unoptimized
 *       because there are no conversions past W1 and x2 = x1
 *
 *    Case W3 <= W2 <= W1:
 *    Case W3 <= W1 <= W2:
 *       x_3 = Word_extdToWord (W1, W3, {signed=_}) x1
 *       because W3 <= W1 && W3 <= W2, just clip x1
 *
 *    Case W2 < W1 <= W3:
 *    Case W2 < W3 <= W1:
 *       unoptimized
 *       because W2 < W1 && W2 < W3, has truncation effect
 *
 *    Case W1 < W2 < W3 AND s1 AND (NOT s2):
 *       unoptimized
 *       because each conversion affects the result separately
 *)

val { get : Var.t -> ((WordSize.t 
                       * WordSize.t 
                       * {signed:bool}) 
                      * Var.t) option,
      set, ... } =
   Property.getSetOnce (Var.plist, Property.initConst NONE)

fun rules x3 (conversion as ((W2, W3, {signed=s2}), x2)) =
   let
      val { <, <=, ... } = Relation.compare WordSize.compare

      fun stop () = set (x3, SOME conversion)
      fun loop ((W1, _, {signed=s1}), x1) =
         rules x3 ((W1, W3, {signed=s1}), x1)
   in
      case get x2 of
         NONE => stop ()
       | SOME (prev as ((W1, _, {signed=s1}), _)) =>
            if W1 <= W3 andalso W3 <= W2 then loop prev else
            if W1 <  W2 andalso W2 <  W3 andalso (not s1 orelse s2)
               then loop prev else
            if W3 <= W1 andalso W3 <= W2 then loop prev else
            (* If W2=W3, we never reach here *)
            stop ()
   end

fun markStatement stmt =
   case stmt of
      Statement.T { exp = Exp.PrimApp { args, prim, targs=_ },
                    ty = _,
                    var = SOME v } =>
        (case Prim.name prim of
            Prim.Name.Word_extdToWord a => rules v (a, Vector.first args)
          | _ => ())
    | _ => ()

fun mapStatement stmt =
   let
      val Statement.T { exp, ty, var } = stmt
      val exp =
         case Option.map (var, get) of
            SOME (SOME (prim as (W2, W3, _), x2)) =>
               if WordSize.equals (W2, W3)
               then Exp.Var x2
               else Exp.PrimApp { args  = Vector.new1 x2,
                                  prim  = Prim.wordExtdToWord prim,
                                  targs = Vector.new0 () }
          | _ => exp
   in
      Statement.T { exp = exp, ty = ty, var = var }
   end

fun transform program =
   let
      val Program.T { datatypes, functions, globals, main } = program
      val shrink = shrinkFunction {globals = globals}

      val functions =
         List.revMap
         (functions, fn f =>
          let
             (* Traverse blocks in dfs order, marking their statements *)
             fun markBlock (Block.T {statements, ... }) =
                (Vector.foreach (statements, markStatement); fn () => ())
             val () = Function.dfs (f, markBlock)

             (* Map the statements using the marks *)
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f

             fun mapBlock block =
                let
                   val Block.T {args, label, statements, transfer} = block
                in
                   Block.T {args = args,
                            label = label,
                            statements = Vector.map (statements, mapStatement),
                            transfer = transfer}
                end

             val f =
                Function.new {args = args,
                              blocks = Vector.map (blocks, mapBlock),
                              mayInline = mayInline,
                              name = name,
                              raises = raises,
                              returns = returns,
                              start = start}

             val f = shrink f
          in
             f
          end)

      val () = Vector.foreach (globals, Statement.clear)
   in
      Program.T { datatypes = datatypes,
                  functions = functions,
                  globals = globals,
                  main = main }
   end

end
