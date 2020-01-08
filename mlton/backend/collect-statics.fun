(* Copyright (C) 2020 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CollectStatics (S: RSSA_TRANSFORM_STRUCTS):
   sig
      structure Globals: RSSA_TRANSFORM
      structure RealConsts: RSSA_TRANSFORM
      structure WordXVectorConsts: RSSA_TRANSFORM
   end =
struct

open S

structure Block =
   struct
      open Block
      fun replace (T {args, kind, label, statements, transfer},
                   {const: Const.t -> Operand.t}): t =
         T {args = args,
            kind = kind,
            label = label,
            statements = Vector.map (statements, fn s =>
                                     Statement.replace (s, {const = const,
                                                            var = Operand.Var})),
            transfer = Transfer.replace (transfer, {const = const,
                                                    label = fn l => l,
                                                    var = Operand.Var})}
   end

structure Function =
   struct
      open Function
      fun replace (f, {const: Const.t -> Operand.t}) =
         let
            val {args, blocks, name, raises, returns, start} = Function.dest f
         in
            Function.new {args = args,
                          blocks = Vector.map (blocks, fn b =>
                                               Block.replace (b, {const = const})),
                          name = name,
                          raises = raises,
                          returns = returns,
                          start = start}
         end
   end

fun collectConsts (p, {const, newStatics}) =
   let
      val Program.T {functions,
                     handlesSignals,
                     main,
                     objectTypes,
                     profileInfo,
                     statics} = p
      val main = Function.replace (main, {const = const})
      val functions = List.map (functions, fn f =>
                                Function.replace (f, {const = const}))
      val statics = Vector.concat [statics, newStatics ()]
   in
      Program.T {functions = functions,
                 handlesSignals = handlesSignals,
                 main = main,
                 objectTypes = objectTypes,
                 profileInfo = profileInfo,
                 statics = statics}
   end

structure WordXVectorConsts =
   struct
      open S
      fun transform p =
         let
            val newStatics = ref []

            val table =
               HashTable.new
               {equals = WordXVector.equals,
                hash = WordXVector.hash}
            fun const c =
               case c of
                  Const.WordVector wv =>
                     HashTable.lookupOrInsert
                     (table, wv, fn () =>
                      let
                         val var = Var.newNoname ()
                         val ws = WordXVector.elementSize wv
                         val ty = Type.wordVector ws
                         val init =
                            WordXVector.toVectorMap
                            (wv, fn w =>
                             Vector.new1 {offset = Bytes.zero,
                                          src = Operand.word w,
                                          ty = Type.word ws})
                         val obj =
                            Object.Sequence
                            {dst = (var, ty),
                             elt = Type.word ws,
                             init = init,
                             tycon = ObjptrTycon.wordVector ws}
                      in
                         List.push (newStatics, obj)
                         ; Operand.Var {var = var, ty = ty}
                      end)
                | _ => Operand.Const c
         in
            collectConsts (p, {const = const,
                               newStatics = fn () => Vector.fromListRev (!newStatics)})
         end
   end

structure RealConsts =
   struct
      open S
      fun transform p =
         let
            local
               fun make rsz =
                  let
                     val vecVar =
                        Var.newString (concat ["real", RealSize.toString rsz, "Consts"])
                     val vecTycon = ObjptrTycon.realVector rsz
                     val vecTy = Type.objptr vecTycon
                     val vecBase = Operand.Var {ty = vecTy, var = vecVar}
                     val elt = Type.real rsz
                     val scale = (case rsz of
                                     RealSize.R32 => Scale.Four
                                   | RealSize.R64 => Scale.Eight)

                     val next = Counter.generator 0
                     val rs = ref []

                     fun add r =
                        (List.push (rs, r)
                         ; Operand.SequenceOffset
                           {base = vecBase,
                            index = Operand.word
                                    (WordX.fromIntInf
                                     (IntInf.fromInt (next ()),
                                      WordSize.seqIndex ())),
                            offset = Bytes.zero,
                            scale = scale,
                            ty = elt})
                     fun newStatic () =
                        if List.isEmpty (!rs)
                           then NONE
                           else let
                                   val init =
                                      Vector.fromListMap
                                      (List.rev (!rs), fn r =>
                                       Vector.new1
                                       {offset = Bytes.zero,
                                        src = Operand.Const (Const.Real r),
                                        ty = elt})
                                   val obj =
                                      Object.Sequence
                                      {dst = (vecVar, vecTy),
                                       elt = elt,
                                       init = init,
                                       tycon = vecTycon}
                                in
                                   SOME obj
                                end
                  in
                     (add, newStatic)
                  end
               val (add32, newStatic32) = make RealSize.R32
               val (add64, newStatic64) = make RealSize.R64
            in
               fun add r =
                  case RealX.size r of
                     RealSize.R32 => add32 r
                   | RealSize.R64 => add64 r
               fun newStatics () =
                  Vector.fromListRev
                  (List.fold ([newStatic32, newStatic64], [],
                              fn (newStatic, newStatics) =>
                              Option.fold (newStatic (), newStatics, op ::)))
            end

            val table =
               HashTable.new
               {equals = RealX.equals,
                hash = RealX.hash}
            fun const c =
               case c of
                  Const.Real r =>
                     HashTable.lookupOrInsert
                     (table, r, fn () => add r)
                | _ => Operand.Const c
         in
            collectConsts (p, {const = const,
                               newStatics = newStatics})
         end

   end

structure Globals =
   struct
      open S
      fun transform p =
         let
            val Program.T {functions,
                           handlesSignals,
                           main,
                           objectTypes,
                           profileInfo,
                           statics} = p

            val {get = varIsStatic,
                 set = setVarIsStatic,
                 destroy = destroyVarIsStatic} =
               Property.destGetSet
               (Var.plist, Property.initConst false)

            fun operandIsStatic oper =
               case oper of
                  Operand.Cast (oper, _) => operandIsStatic oper
                | Operand.Const _ => true
                | Operand.Var {var, ...} => varIsStatic var
                | _ => false

            val newStatics = ref []

            fun objectIsStatic obj =
               let
                  fun hasIdentity tycon =
                     case Vector.sub (objectTypes, ObjptrTycon.index tycon) of
                        ObjectType.Normal {hasIdentity, ...} => hasIdentity
                      | ObjectType.Sequence {hasIdentity, ...} => hasIdentity
                      | _ => Error.bug "CollectStatics.Globals.transform.objectIsStatic.hasIdentity"
                  fun initIsStatic init =
                     Vector.forall (init, fn {offset = _, src, ty = _} =>
                                    operandIsStatic src)
               in
                  case obj of
                     Object.Normal {dst = (dst, _), init, tycon, ty, ...} =>
                        if initIsStatic init
                           andalso (not (hasIdentity tycon)
                                    orelse (* Address of a static object
                                            * won't map to a valid card slot.
                                            *)
                                           not (Type.exists (ty, Type.isObjptr)
                                                andalso !Control.markCards))
                           then (List.push (newStatics, obj)
                                 ; setVarIsStatic (dst, true)
                                 ; true)
                           else false
                   | Object.Sequence {dst = (dst, _), elt, init, tycon, ...} =>
                        if Vector.forall (init, initIsStatic)
                           andalso (not (hasIdentity tycon)
                                    orelse (* Address of a static object
                                            * won't map to a valid card slot.
                                            *)
                                           not (Type.exists (elt, Type.isObjptr)
                                                andalso !Control.markCards)
                                    orelse (* But an empty sequence will never be updated
                                            * and trigger a card marking.
                                            *)
                                           Vector.isEmpty init)
                           then (List.push (newStatics, obj)
                                 ; setVarIsStatic (dst, true)
                                 ; true)
                           else false
               end
            fun statementIsStatic stmt =
               case stmt of
                  Statement.Object obj => objectIsStatic obj
                | _ => false

            val () =
               Vector.foreach
               (statics, fn obj => setVarIsStatic (#1 (Object.dst obj), true))

            val main =
               let
                  val {args, name, raises, returns, start, ...} = Function.dest main
                  val blocks = ref []
                  val () =
                     Function.dfs
                     (main, fn Block.T {args, kind, label, statements, transfer} =>
                      let
                         val statements =
                            Vector.keepAll (statements, not o statementIsStatic)
                         val block =
                            Block.T {args = args,
                                     kind = kind,
                                     label = label,
                                     statements = statements,
                                     transfer = transfer}
                      in
                         List.push (blocks, block)
                         ; fn () => ()
                      end)
               in
                  Function.new {args = args,
                                blocks = Vector.fromList (!blocks),
                                name = name,
                                raises = raises,
                                returns = returns,
                                start = start}
               end

            val statics = Vector.concat [statics, Vector.fromListRev (!newStatics)]

            val () = destroyVarIsStatic ()
         in
            Program.T {functions = functions,
                       handlesSignals = handlesSignals,
                       main = main,
                       objectTypes = objectTypes,
                       profileInfo = profileInfo,
                       statics = statics}
         end
   end

end
