(* Copyright (C) 2009,2016-2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Rssa (S: RSSA_STRUCTS): RSSA =
struct

open S

local
   open Prim
in
   structure ApplyArg = ApplyArg
   structure ApplyResult = ApplyResult
end
local
   open Runtime
in
   structure CFunction = CFunction
   structure GCField = GCField
end

fun constrain (ty: Type.t): Layout.t =
   let
      open Layout
   in
      if !Control.showTypes
         then seq [str ": ", Type.layout ty]
      else empty
   end

structure Operand =
   struct
      datatype t =
         ArrayOffset of {base: t,
                         index: t,
                         offset: Bytes.t,
                         scale: Scale.t,
                         ty: Type.t}
       | Cast of t * Type.t
       | Const of Const.t
       | EnsuresBytesFree
       | GCState
       | Offset of {base: t,
                    offset: Bytes.t,
                    ty: Type.t}
       | ObjptrTycon of ObjptrTycon.t
       | Runtime of GCField.t
       | Var of {var: Var.t,
                 ty: Type.t}

      val null = Const Const.null

      val word = Const o Const.word

      fun zero s = word (WordX.fromIntInf (0, s))

      fun bool b =
         word (WordX.fromIntInf (if b then 1 else 0, WordSize.bool))

      val ty =
         fn ArrayOffset {ty, ...} => ty
          | Cast (_, ty) => ty
          | Const c =>
               let
                  datatype z = datatype Const.t
               in
                  case c of
                     IntInf _ => Type.intInf ()
                   | Null => Type.cpointer ()
                   | Real r => Type.real (RealX.size r)
                   | Word w => Type.ofWordX w
                   | WordVector v => Type.ofWordXVector v
               end
          | EnsuresBytesFree => Type.csize ()
          | GCState => Type.gcState ()
          | Offset {ty, ...} => ty
          | ObjptrTycon _ => Type.objptrHeader ()
          | Runtime z => Type.ofGCField z
          | Var {ty, ...} => ty

      fun layout (z: t): Layout.t =
         let
            open Layout 
         in
            case z of
               ArrayOffset {base, index, offset, scale, ty} =>
                  seq [str (concat ["X", Type.name ty, " "]),
                       tuple [layout base, layout index, Scale.layout scale,
                              Bytes.layout offset]]
             | Cast (z, ty) =>
                  seq [str "Cast ", tuple [layout z, Type.layout ty]]
             | Const c => seq [Const.layout c, constrain (ty z)]
             | EnsuresBytesFree => str "<EnsuresBytesFree>"
             | GCState => str "<GCState>"
             | Offset {base, offset, ty} =>
                  seq [str (concat ["O", Type.name ty, " "]),
                       tuple [layout base, Bytes.layout offset],
                       constrain ty]
             | ObjptrTycon opt => ObjptrTycon.layout opt
             | Runtime r => GCField.layout r
             | Var {var, ...} => Var.layout var
         end

      fun cast (z: t, t: Type.t): t =
         if Type.equals (t, ty z)
            then z
         else Cast (z, t)

      val cast = Trace.trace2 ("Rssa.Operand.cast", layout, Type.layout, layout) cast

      val rec isLocation =
         fn ArrayOffset _ => true
          | Cast (z, _) => isLocation z
          | Offset _ => true
          | Runtime _ => true
          | Var _ => true
          | _ => false

      fun 'a foldVars (z: t, a: 'a, f: Var.t * 'a -> 'a): 'a =
         case z of
            ArrayOffset {base, index, ...} =>
               foldVars (index, foldVars (base, a, f), f)
          | Cast (z, _) => foldVars (z, a, f)
          | Offset {base, ...} => foldVars (base, a, f)
          | Var {var, ...} => f (var, a)
          | _ => a

      fun replaceVar (z: t, f: Var.t -> t): t =
         let
            fun loop (z: t): t =
               case z of
                  ArrayOffset {base, index, offset, scale, ty} =>
                     ArrayOffset {base = loop base,
                                  index = loop index,
                                  offset = offset,
                                  scale = scale,
                                  ty = ty}
                | Cast (t, ty) => Cast (loop t, ty)
                | Offset {base, offset, ty} =>
                     Offset {base = loop base,
                             offset = offset,
                             ty = ty}
                | Var {var, ...} => f var
                | _ => z
         in
            loop z
         end

   end

structure Switch =
   struct
      local
         structure S = Switch (open S
                               structure Type = Type
                               structure Use = Operand)
      in
         open S
      end

      fun replaceVar (T {cases, default, size, test}, f) =
         T {cases = cases,
            default = default,
            size = size,
            test = Operand.replaceVar (test, f)}
   end

structure Statement =
   struct
      datatype t =
         Bind of {dst: Var.t * Type.t,
                  isMutable: bool,
                  src: Operand.t}
       | Move of {dst: Operand.t,
                  src: Operand.t}
       | Object of {dst: Var.t * Type.t,
                    header: word,
                    size: Bytes.t}
       | PrimApp of {args: Operand.t vector,
                     dst: (Var.t * Type.t) option,
                     prim: Type.t Prim.t}
       | Profile of ProfileExp.t
       | ProfileLabel of ProfileLabel.t
       | SetExnStackLocal
       | SetExnStackSlot
       | SetHandler of Label.t
       | SetSlotExnStack

      fun 'a foldDefUse (s, a: 'a, {def: Var.t * Type.t * 'a -> 'a,
                                    use: Var.t * 'a -> 'a}): 'a =
         let
            fun useOperand (z: Operand.t, a) = Operand.foldVars (z, a, use)
         in
            case s of
               Bind {dst = (x, t), src, ...} => def (x, t, useOperand (src, a))
             | Move {dst, src} => useOperand (src, useOperand (dst, a))
             | Object {dst = (dst, ty), ...} => def (dst, ty, a)
             | PrimApp {dst, args, ...} =>
                  Vector.fold (args,
                               Option.fold (dst, a, fn ((x, t), a) =>
                                            def (x, t, a)),
                               useOperand)
             | Profile _ => a
             | ProfileLabel _ => a
             | SetExnStackLocal => a
             | SetExnStackSlot => a
             | SetHandler _ => a
             | SetSlotExnStack => a
         end

      fun foreachDefUse (s: t, {def, use}) =
         foldDefUse (s, (), {def = fn (x, t, ()) => def (x, t),
                             use = use o #1})

      fun 'a foldDef (s: t, a: 'a, f: Var.t * Type.t * 'a -> 'a): 'a =
         foldDefUse (s, a, {def = f, use = #2})

      fun foreachDef (s:t , f: Var.t * Type.t -> unit) =
         foldDef (s, (), fn (x, t, ()) => f (x, t))

      fun 'a foldUse (s: t, a: 'a, f: Var.t * 'a -> 'a) =
         foldDefUse (s, a, {def = #3, use = f})

      fun foreachUse (s, f) = foldUse (s, (), f o #1)

      fun replaceUses (s: t, f: Var.t -> Operand.t): t =
         let
            fun oper (z: Operand.t): Operand.t =
               Operand.replaceVar (z, f)
         in
            case s of
               Bind {dst, isMutable, src} =>
                  Bind {dst = dst,
                        isMutable = isMutable,
                        src = oper src}
             | Move {dst, src} => Move {dst = oper dst, src = oper src}
             | Object _ => s
             | PrimApp {args, dst, prim} =>
                  PrimApp {args = Vector.map (args, oper),
                           dst = dst,
                           prim = prim}
             | Profile _ => s
             | ProfileLabel _ => s
             | SetExnStackLocal => s
             | SetExnStackSlot => s
             | SetHandler _ => s
             | SetSlotExnStack => s
         end

      val layout =
         let
            open Layout
         in
            fn Bind {dst = (x, t), src, ...} =>
                  mayAlign
                  [seq [Var.layout x, constrain t],
                   indent (seq [str "= ", Operand.layout src], 2)]
             | Move {dst, src} =>
                  mayAlign
                  [Operand.layout dst,
                   indent (seq [str ":= ", Operand.layout src], 2)]
             | Object {dst = (dst, ty), header, size} =>
                  mayAlign
                  [seq [Var.layout dst, constrain ty],
                   indent (seq [str "= Object ",
                                record [("header", seq [str "0x", Word.layout header]),
                                        ("size", Bytes.layout size)]],
                           2)]
             | PrimApp {dst, prim, args, ...} =>
                  mayAlign
                  [case dst of
                      NONE => seq [str "_", constrain (Type.unit)]
                    | SOME (x, t) => seq [Var.layout x, constrain t],
                   indent (seq [str "= ", Prim.layout prim, str " ",
                                Vector.layout Operand.layout args],
                           2)]
             | Profile e => ProfileExp.layout e
             | ProfileLabel p =>
                  seq [str "ProfileLabel ", ProfileLabel.layout p]
             | SetExnStackLocal => str "SetExnStackLocal"
             | SetExnStackSlot => str "SetExnStackSlot "
             | SetHandler l => seq [str "SetHandler ", Label.layout l]
             | SetSlotExnStack => str "SetSlotExnStack "
         end

      val toString = Layout.toString o layout

      fun clear (s: t) =
         foreachDef (s, Var.clear o #1)

      fun resize (src: Operand.t, dstTy: Type.t): Operand.t * t list =
         let
            val srcTy = Operand.ty src

            val (src, srcTy, ssSrc, dstTy, finishDst) =
               case (Type.deReal srcTy, Type.deReal dstTy) of
                  (NONE, NONE) => 
                     (src, srcTy, [], dstTy, fn dst => (dst, []))
                | (SOME rs, NONE) =>
                     let
                        val ws = WordSize.fromBits (RealSize.bits rs)
                        val tmp = Var.newNoname ()
                        val tmpTy = Type.word ws
                     in
                        (Operand.Var {ty = tmpTy, var = tmp},
                         tmpTy,
                         [PrimApp {args = Vector.new1 src,
                                   dst = SOME (tmp, tmpTy),
                                   prim = Prim.realCastToWord (rs, ws)}],
                         dstTy, fn dst => (dst, []))
                     end
                | (NONE, SOME rs) =>
                     let
                        val ws = WordSize.fromBits (RealSize.bits rs)
                        val tmp = Var.newNoname ()
                        val tmpTy = Type.real rs
                     in
                        (src, srcTy, [],
                         Type.word ws,
                         fn dst =>
                         (Operand.Var {ty = tmpTy, var = tmp},
                          [PrimApp {args = Vector.new1 dst,
                                    dst = SOME (tmp, tmpTy),
                                    prim = Prim.wordCastToReal (ws, rs)}]))
                     end
                | (SOME _, SOME _) =>
                     (src, srcTy, [], dstTy, fn dst => (dst, []))

            val srcW = Type.width srcTy
            val dstW = Type.width dstTy

            val (dst, ssConv) =
               if Bits.equals (srcW, dstW)
                  then (Operand.cast (src, dstTy), [])
               else let
                       val tmp = Var.newNoname ()
                       val tmpTy = dstTy
                    in
                       (Operand.Var {ty = tmpTy, var = tmp},
                        [PrimApp {args = Vector.new1 src,
                                  dst = SOME (tmp, tmpTy),
                                  prim = (Prim.wordExtdToWord 
                                          (WordSize.fromBits srcW, 
                                           WordSize.fromBits dstW, 
                                           {signed = false}))}])
                    end

            val (dst, ssDst) = finishDst dst
         in
            (dst, ssSrc @ ssConv @ ssDst)
         end
   end

datatype z = datatype Statement.t

structure Transfer =
   struct
      datatype t =
         Arith of {args: Operand.t vector,
                   dst: Var.t,
                   overflow: Label.t,
                   prim: Type.t Prim.t,
                   success: Label.t,
                   ty: Type.t}
       | CCall of {args: Operand.t vector,
                   func: Type.t CFunction.t,
                   return: Label.t option}
       | Call of {args: Operand.t vector,
                  func: Func.t,
                  return: Return.t}
       | Goto of {args: Operand.t vector,
                  dst: Label.t}
       | Raise of Operand.t vector
       | Return of Operand.t vector
       | Switch of Switch.t

      fun layout t =
         let
            open Layout
         in
            case t of
               Arith {args, dst, overflow, prim, success, ty} =>
                  seq [str "Arith ",
                       record [("args", Vector.layout Operand.layout args),
                               ("dst", Var.layout dst),
                               ("overflow", Label.layout overflow),
                               ("prim", Prim.layout prim),
                               ("success", Label.layout success),
                               ("ty", Type.layout ty)]]
             | CCall {args, func, return} =>
                  seq [str "CCall ",
                       record [("args", Vector.layout Operand.layout args),
                               ("func", CFunction.layout (func, Type.layout)),
                               ("return", Option.layout Label.layout return)]]
             | Call {args, func, return} =>
                  seq [Func.layout func, str " ",
                       Vector.layout Operand.layout args,
                       str " ", Return.layout return]
             | Goto {dst, args} =>
                  seq [Label.layout dst, str " ",
                       Vector.layout Operand.layout args]
             | Raise xs => seq [str "raise ", Vector.layout Operand.layout xs]
             | Return xs => seq [str "return ", Vector.layout Operand.layout xs]
             | Switch s => Switch.layout s
         end

      fun bug () =
         CCall {args = (Vector.new1
                        (Operand.Const
                         (Const.string "control shouldn't reach here"))),
                func = Type.BuiltInCFunction.bug (),
                return = NONE}

      fun foreachFunc (t, f : Func.t -> unit) : unit =
         case t of
            Call {func, ...} => f func
          | _ => ()

      fun 'a foldDefLabelUse (t, a: 'a,
                              {def: Var.t * Type.t * 'a -> 'a,
                               label: Label.t * 'a -> 'a,
                               use: Var.t * 'a -> 'a}): 'a =
         let
            fun useOperand (z, a) = Operand.foldVars (z, a, use)
            fun useOperands (zs: Operand.t vector, a) =
               Vector.fold (zs, a, useOperand)
         in
            case t of
               Arith {args, dst, overflow, success, ty, ...} =>
                  let
                     val a = label (overflow, a)
                     val a = label (success, a)
                     val a = def (dst, ty, a)
                     val a = useOperands (args, a)
                  in
                     a
                  end
             | CCall {args, return, ...} =>
                  useOperands (args,
                               case return of
                                  NONE => a
                                | SOME l => label (l, a))
             | Call {args, return, ...} =>
                  useOperands (args, Return.foldLabel (return, a, label))
             | Goto {args, dst, ...} => label (dst, useOperands (args, a))
             | Raise zs => useOperands (zs, a)
             | Return zs => useOperands (zs, a)
             | Switch s => Switch.foldLabelUse (s, a, {label = label,
                                                       use = useOperand})
         end

      fun foreachDefLabelUse (t, {def, label, use}) =
         foldDefLabelUse (t, (), {def = fn (x, t, ()) => def (x, t),
                                  label = label o #1,
                                  use = use o #1})

      fun foldLabel (t, a, f) = foldDefLabelUse (t, a, {def = #3,
                                                        label = f,
                                                        use = #2})

      fun foreachLabel (t, f) = foldLabel (t, (), f o #1)

      fun foldDef (t, a, f) = foldDefLabelUse (t, a, {def = f,
                                                      label = #2,
                                                      use = #2})

      fun foreachDef (t, f) =
         foldDef (t, (), fn (x, t, ()) => f (x, t))

      fun foldUse (t, a, f) = foldDefLabelUse (t, a, {def = #3,
                                                      label = #2,
                                                      use = f})

      fun foreachUse (t, f) = foldUse (t, (), f o #1)

      fun clear (t: t): unit =
         foreachDef (t, Var.clear o #1)

      local
         fun make i = WordX.fromIntInf (i, WordSize.bool)
      in
         fun ifBool (test, {falsee, truee}) =
            Switch (Switch.T
                    {cases = Vector.new2 ((make 0, falsee), (make 1, truee)),
                     default = NONE,
                     size = WordSize.bool,
                     test = test})
         fun ifZero (test, {falsee, truee}) =
            Switch (Switch.T
                    {cases = Vector.new1 (make 0, truee),
                     default = SOME falsee,
                     size = WordSize.bool,
                     test = test})
      end

      fun replaceUses (t: t, f: Var.t -> Operand.t): t =
         let
            fun oper z = Operand.replaceVar (z, f)
            fun opers zs = Vector.map (zs, oper)
         in
            case t of
               Arith {args, dst, overflow, prim, success, ty} =>
                  Arith {args = opers args,
                         dst = dst,
                         overflow = overflow,
                         prim = prim,
                         success = success,
                         ty = ty}
             | CCall {args, func, return} =>
                  CCall {args = opers args,
                         func = func,
                         return = return}
             | Call {args, func, return} =>
                  Call {args = opers args,
                        func = func,
                        return = return}
             | Goto {args, dst} =>
                  Goto {args = opers args,
                        dst = dst}
             | Raise zs => Raise (opers zs)
             | Return zs => Return (opers zs)
             | Switch s => Switch (Switch.replaceVar (s, f))
         end
   end

structure Kind =
   struct
      datatype t =
         Cont of {handler: Handler.t}
       | CReturn of {func: Type.t CFunction.t}
       | Handler
       | Jump

      fun layout k =
         let
            open Layout
         in
            case k of
               Cont {handler} =>
                  seq [str "Cont ",
                       record [("handler", Handler.layout handler)]]
             | CReturn {func} =>
                  seq [str "CReturn ",
                       record [("func", CFunction.layout (func, Type.layout))]]
             | Handler => str "Handler"
             | Jump => str "Jump"
         end

      datatype frameStyle = None | OffsetsAndSize | SizeOnly
      fun frameStyle (k: t): frameStyle =
         case k of
            Cont _ => OffsetsAndSize
          | CReturn {func, ...} =>
               if CFunction.mayGC func
                  then OffsetsAndSize
               else if !Control.profile = Control.ProfileNone
                       then None
                    else SizeOnly
          | Handler => SizeOnly
          | Jump => None
   end

local
   open Layout
in
   fun layoutFormals (xts: (Var.t * Type.t) vector) =
      Vector.layout (fn (x, t) =>
                    seq [Var.layout x,
                         if !Control.showTypes
                            then seq [str ": ", Type.layout t]
                         else empty])
      xts
end

structure Block =
   struct
      datatype t =
         T of {args: (Var.t * Type.t) vector,
               kind: Kind.t,
               label: Label.t,
               statements: Statement.t vector,
               transfer: Transfer.t}

      local
         fun make f (T r) = f r
      in
         val kind = make #kind
         val label = make #label
      end

      fun clear (T {args, label, statements, transfer, ...}) =
         (Vector.foreach (args, Var.clear o #1)
          ; Label.clear label
          ; Vector.foreach (statements, Statement.clear)
          ; Transfer.clear transfer)

      fun layout (T {args, kind, label, statements, transfer, ...}) =
         let
            open Layout
         in
            align [seq [Label.layout label, str " ",
                        Vector.layout (fn (x, t) =>
                                       if !Control.showTypes
                                          then seq [Var.layout x, str ": ",
                                                    Type.layout t]
                                       else Var.layout x) args,
                        str " ", Kind.layout kind, str " = "],
                   indent (align
                           [align
                            (Vector.toListMap (statements, Statement.layout)),
                            Transfer.layout transfer],
                           2)]
         end

      fun foreachDef (T {args, statements, transfer, ...}, f) =
         (Vector.foreach (args, f)
          ; Vector.foreach (statements, fn s => Statement.foreachDef (s, f))
          ; Transfer.foreachDef (transfer, f))

      fun foreachUse (T {statements, transfer, ...}, f) =
         (Vector.foreach (statements, fn s => Statement.foreachUse (s, f))
          ; Transfer.foreachUse (transfer, f))
   end

structure Function =
   struct
      datatype t = T of {args: (Var.t * Type.t) vector,
                         blocks: Block.t vector,
                         name: Func.t,
                         raises: Type.t vector option,
                         returns: Type.t vector option,
                         start: Label.t}

      local
         fun make f (T r) = f r
      in
         val blocks = make #blocks
         val name = make #name
      end

      fun dest (T r) = r
      val new = T

      fun clear (T {name, args, blocks, ...}) =
         (Func.clear name
          ; Vector.foreach (args, Var.clear o #1)
          ; Vector.foreach (blocks, Block.clear))

      fun layoutHeader (T {args, name, raises, returns, start, ...}): Layout.t =
         let
            open Layout
         in
            seq [str "fun ", Func.layout name,
                 str " ", layoutFormals args,
                 if !Control.showTypes
                    then seq [str ": ",
                              record [("raises",
                                       Option.layout
                                       (Vector.layout Type.layout) raises),
                                      ("returns",
                                       Option.layout
                                       (Vector.layout Type.layout) returns)]]
                 else empty,
                 str " = ", Label.layout start, str " ()"]
         end

      fun layouts (f as T {blocks, ...}, output) =
         (output (layoutHeader f)
          ; Vector.foreach (blocks, fn b =>
                            output (Layout.indent (Block.layout b, 2))))

      fun layout (f as T {blocks, ...}) =
         let
            open Layout
         in
            align [layoutHeader f,
                   indent (align (Vector.toListMap (blocks, Block.layout)), 2)]
         end

      fun foreachDef (T {args, blocks, ...}, f) =
         (Vector.foreach (args, f)
          ; (Vector.foreach (blocks, fn b => Block.foreachDef (b, f))))

      fun foreachUse (T {blocks, ...}, f) =
         Vector.foreach (blocks, fn b => Block.foreachUse (b, f))

      fun dfs (T {blocks, start, ...}, v) =
         let
            val numBlocks = Vector.length blocks
            val {get = labelIndex, set = setLabelIndex, rem, ...} =
               Property.getSetOnce (Label.plist,
                                    Property.initRaise ("index", Label.layout))
            val _ = Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
                                     setLabelIndex (label, i))
            val visited = Array.array (numBlocks, false)
            fun visit (l: Label.t): unit =
               let
                  val i = labelIndex l
               in
                  if Array.sub (visited, i)
                     then ()
                  else
                     let
                        val _ = Array.update (visited, i, true)
                        val b as Block.T {transfer, ...} =
                           Vector.sub (blocks, i)
                        val v' = v b
                        val _ = Transfer.foreachLabel (transfer, visit)
                        val _ = v' ()
                     in
                        ()
                     end
               end
            val _ = visit start
            val _ = Vector.foreach (blocks, rem o Block.label)
         in
            ()
         end

      structure Graph = DirectedGraph
      structure Node = Graph.Node

      fun dominatorTree (T {blocks, start, ...}): Block.t Tree.t =
         let
            open Dot
            val g = Graph.new ()
            fun newNode () = Graph.newNode g
            val {get = labelNode, ...} =
               Property.get
               (Label.plist, Property.initFun (fn _ => newNode ()))
            val {get = nodeInfo: unit Node.t -> {block: Block.t},
                 set = setNodeInfo, ...} =
               Property.getSetOnce
               (Node.plist, Property.initRaise ("info", Node.layout))
            val () =
               Vector.foreach
               (blocks, fn b as Block.T {label, ...}=>
                setNodeInfo (labelNode label, {block = b}))
            val () =
               Vector.foreach
               (blocks, fn Block.T {label, transfer, ...} =>
                let
                   val from = labelNode label
                   val _ =
                      Transfer.foreachLabel
                      (transfer, fn to =>
                       (ignore o Graph.addEdge) 
                       (g, {from = from, to = labelNode to}))
                in
                   ()
                end)
         in
            Graph.dominatorTree (g, {root = labelNode start,
                                     nodeValue = #block o nodeInfo})
         end

      fun dropProfile (f: t): t =
         let
            val {args, blocks, name, raises, returns, start} = dest f
            val blocks =
               Vector.map
               (blocks, fn Block.T {args, kind, label, statements, transfer} =>
                Block.T {args = args,
                         kind = kind,
                         label = label,
                         statements = Vector.keepAll
                                      (statements,
                                       fn Statement.Profile _ => false
                                        | Statement.ProfileLabel _ => false
                                        | _ => true),
                         transfer = transfer})
         in
            new {args = args,
                 blocks = blocks,
                 name = name,
                 raises = raises,
                 returns = returns,
                 start = start}
         end

      fun shrink (f: t): t =
         let
            val {args, blocks, name, raises, returns, start} = dest f
            val {get = labelInfo, rem, set = setLabelInfo, ...} =
               Property.getSetOnce
               (Label.plist, Property.initRaise ("info", Label.layout))
            val () =
               Vector.foreach
               (blocks, fn block as Block.T {label, ...} =>
                setLabelInfo (label, {block = block,
                                      inline = ref false,
                                      occurrences = ref 0}))
            fun visitLabel l = Int.inc (#occurrences (labelInfo l))
            val () = visitLabel start
            val () =
               Vector.foreach (blocks, fn Block.T {transfer, ...} =>
                               Transfer.foreachLabel (transfer, visitLabel))
            datatype z = datatype Statement.t
            datatype z = datatype Transfer.t
            val () =
               Vector.foreach
               (blocks, fn Block.T {transfer, ...} =>
                case transfer of
                   Goto {dst, ...} =>
                      let
                         val {inline, occurrences, ...} = labelInfo dst
                      in
                         if 1 = !occurrences
                            then inline := true
                         else ()
                      end
                 | _ => ())
            fun expand (ss: Statement.t vector list, t: Transfer.t)
               : Statement.t vector * Transfer.t =
               let
                  fun done () = (Vector.concat (rev ss), t)
               in
                  case t of
                     Goto {args, dst} =>
                        let
                           val {block, inline, ...} = labelInfo dst
                        in
                           if not (!inline)
                              then done ()
                           else
                              let
                                 val Block.T {args = formals, statements,
                                              transfer, ...} =
                                    block
                                 val binds =
                                    Vector.map2
                                    (formals, args, fn (dst, src) =>
                                     Bind {dst = dst,
                                           isMutable = false,
                                           src = src})
                              in
                                 expand (statements :: binds :: ss, transfer)
                              end
                        end
                   | _ => done ()
               end
            val blocks =
               Vector.fromList
               (Vector.fold
                (blocks, [],
                 fn (Block.T {args, kind, label, statements, transfer}, ac) =>
                 let
                    val {inline, ...} = labelInfo label
                 in
                    if !inline
                       then ac
                    else
                       let
                          val (statements, transfer) =
                             expand ([statements], transfer)
                       in
                          Block.T {args = args,
                                   kind = kind,
                                   label = label,
                                   statements = statements,
                                   transfer = transfer} :: ac
                       end
                 end))
            val () = Vector.foreach (blocks, rem o Block.label)
         in
            new {args = args,
                 blocks = blocks,
                 name = name,
                 raises = raises,
                 returns = returns,
                 start = start}
         end
   end

structure Program =
   struct
      datatype t =
         T of {functions: Function.t list,
               handlesSignals: bool,
               main: Function.t,
               objectTypes: ObjectType.t vector}

      fun clear (T {functions, main, ...}) =
         (List.foreach (functions, Function.clear)
          ; Function.clear main)

      fun layouts (T {functions, main, objectTypes, ...},
                   output': Layout.t -> unit): unit =
         let
            open Layout
            val output = output'
         in
            output (str "\nObjectTypes:")
            ; Vector.foreachi (objectTypes, fn (i, ty) =>
                               output (seq [str "opt_", Int.layout i,
                                            str " = ", ObjectType.layout ty]))
            ; output (str "\nMain:")
            ; Function.layouts (main, output)
            ; output (str "\nFunctions:")
            ; List.foreach (functions, fn f => Function.layouts (f, output))
         end

      fun layoutStats (T {functions, main, objectTypes, ...}) =
         let
            val numStatements = ref 0
            val numBlocks = ref 0
            val _ =
               List.foreach
               (main::functions, fn f =>
                let
                   val {blocks, ...} = Function.dest f
                in
                   Vector.foreach
                   (blocks, fn Block.T {statements, ...} =>
                    (Int.inc numBlocks
                     ; numStatements := !numStatements + Vector.length statements))
                end)
            val numFunctions = 1 + List.length functions
            val numObjectTypes = Vector.length objectTypes
            open Layout
         in
            align
            [seq [str "num functions in program = ", Int.layout numFunctions],
             seq [str "num blocks in program = ", Int.layout (!numBlocks)],
             seq [str "num statements in program = ", Int.layout (!numStatements)],
             seq [str "num object types in program = ", Int.layout (numObjectTypes)]]
         end

      fun dropProfile (T {functions, handlesSignals, main, objectTypes}) =
         (Control.profile := Control.ProfileNone
          ; T {functions = List.map (functions, Function.dropProfile),
               handlesSignals = handlesSignals,
               main = Function.dropProfile main,
               objectTypes = objectTypes})
      (* quell unused warning *)
      val _ = dropProfile

      fun dfs (p, v) =
         let
            val T {functions, main, ...} = p
            val functions = Vector.fromList (main::functions)
            val numFunctions = Vector.length functions
            val {get = funcIndex, set = setFuncIndex, rem, ...} =
               Property.getSetOnce (Func.plist,
                                    Property.initRaise ("index", Func.layout))
            val _ = Vector.foreachi (functions, fn (i, f) =>
                                     setFuncIndex (#name (Function.dest f), i))
            val visited = Array.array (numFunctions, false)
            fun visit (f: Func.t): unit =
               let
                  val i = funcIndex f
               in
                  if Array.sub (visited, i)
                     then ()
                  else
                     let
                        val _ = Array.update (visited, i, true)
                        val f = Vector.sub (functions, i)
                        val v' = v f
                        val _ = Function.dfs 
                                (f, fn Block.T {transfer, ...} =>
                                 (Transfer.foreachFunc (transfer, visit)
                                  ; fn () => ()))
                        val _ = v' ()
                     in
                        ()
                     end
               end
            val _ = visit (Function.name main)
            val _ = Vector.foreach (functions, rem o Function.name)
         in
            ()
         end

      fun orderFunctions (p as T {handlesSignals, objectTypes, ...}) =
         let
            val functions = ref []
            val () =
               dfs
               (p, fn f =>
                let
                   val {args, name, raises, returns, start, ...} =
                      Function.dest f
                   val blocks = ref []
                   val () =
                      Function.dfs
                      (f, fn b =>
                       (List.push (blocks, b)
                        ; fn () => ()))
                   val f = Function.new {args = args,
                                         blocks = Vector.fromListRev (!blocks),
                                         name = name,
                                         raises = raises,
                                         returns = returns,
                                         start = start}
                in
                   List.push (functions, f)
                   ; fn () => ()
                end)
            val (main, functions) =
               case List.rev (!functions) of
                  main::functions => (main, functions)
                | _ => Error.bug "Rssa.orderFunctions: main/functions"
         in
            T {functions = functions,
               handlesSignals = handlesSignals,
               main = main,
               objectTypes = objectTypes}
         end

      fun copyProp (T {functions, handlesSignals, main, objectTypes, ...}): t =
         let
            val tracePrimApply =
               Trace.trace3
               ("Rssa.copyProp.primApply",
                Prim.layout,
                List.layout (ApplyArg.layout (Var.layout o #var)),
                Layout.ignore,
                ApplyResult.layout (Var.layout o #var))
            val {get = replaceVar: Var.t -> Operand.t,
                 set = setReplaceVar, ...} =
               Property.getSetOnce
               (Var.plist, Property.initRaise ("replacement", Var.layout))
            fun dontReplace (x: Var.t, t: Type.t): unit =
               setReplaceVar (x, Operand.Var {var = x, ty = t})
            val setReplaceVar = fn (x: Var.t, t: Type.t, z: Operand.t) =>
               let
                  val z =
                     if Type.equals (Operand.ty z, t)
                        then z
                        else Operand.Cast (z, t)
               in
                  setReplaceVar (x, z)
               end
            fun loopStatement (s: Statement.t): Statement.t option =
               let
                  val s = Statement.replaceUses (s, replaceVar)
                  fun keep () =
                     (Statement.foreachDef (s, dontReplace)
                      ; SOME s)
               in
                  case s of
                     Bind {dst = (dst, dstTy), isMutable, src} =>
                        if isMutable
                           then keep ()
                        else
                           let
                              datatype z = datatype Operand.t
                              fun getSrc src =
                                 case src of
                                    Cast (src, _) => getSrc src
                                  | Const _ => SOME src
                                  | Var _ => SOME src
                                  | _ => NONE
                           in
                              case getSrc src of
                                 NONE => keep ()
                               | SOME src =>
                                    (setReplaceVar (dst, dstTy, src)
                                     ; NONE)
                           end
                   | PrimApp {args, dst, prim} =>
                        let
                           fun replace (z: Operand.t): Statement.t option =
                              (Option.app (dst, fn (x, t) =>
                                           setReplaceVar (x, t, z))
                               ; NONE)
                           datatype z = datatype Operand.t
                           fun getArg arg =
                              case arg of
                                 Cast (arg, _) => getArg arg
                               | Const c => SOME (ApplyArg.Const c)
                               | Var x => SOME (ApplyArg.Var x)
                               | _ => NONE
                           val applyArgs = Vector.keepAllMap (args, getArg)
                           datatype z = datatype ApplyResult.t
                        in
                           if Vector.length args <> Vector.length applyArgs
                              then keep ()
                           else
                              case (tracePrimApply
                                    Prim.apply
                                    (prim, Vector.toList applyArgs,
                                     fn ({var = x, ...}, {var = y, ...}) =>
                                     Var.equals (x, y))) of
                                 Apply (prim, args) =>
                                    let
                                       val args =
                                          Vector.fromListMap (args, Operand.Var)
                                       val () = Option.app (dst, dontReplace)
                                    in
                                       SOME (PrimApp {args = args,
                                                      dst = dst,
                                                      prim = prim})
                                    end
                               | Bool b => replace (Operand.bool b)
                               | Const c => replace (Operand.Const c)
                               | Overflow => keep ()
                               | Unknown => keep ()
                               | Var x => replace (Operand.Var x)
                        end
                | _ => keep ()
               end
            fun loopTransfer t =
               (Transfer.foreachDef (t, dontReplace)
                ; Transfer.replaceUses (t, replaceVar))
            fun loopFormals args = Vector.foreach (args, dontReplace)
            fun loopFunction (f: Function.t): Function.t =
               let
                  val {args, name, raises, returns, start, ...} =
                     Function.dest f
                  val () = loopFormals args
                  val blocks = ref []
                  val () =
                     Function.dfs
                     (f, fn Block.T {args, kind, label, statements, transfer} =>
                      let
                         val () = loopFormals args
                         val statements =
                            Vector.keepAllMap (statements, loopStatement)
                         val transfer = loopTransfer transfer
                         val () =
                            List.push
                            (blocks, Block.T {args = args,
                                              kind = kind,
                                              label = label,
                                              statements = statements,
                                              transfer = transfer})
                      in
                         fn () => ()
                      end)
                  val blocks = Vector.fromList (!blocks)
               in
                  Function.new {args = args,
                                blocks = blocks,
                                name = name,
                                raises = raises,
                                returns = returns,
                                start = start}
               end
            (* Must process main first, because it defines globals that are
             * used in other functions.
             *)
            val main = loopFunction main
            val functions = List.revMap (functions, loopFunction)
         in
            T {functions = functions,
               handlesSignals = handlesSignals,
               main = main,
               objectTypes = objectTypes}
         end

      fun shrink (T {functions, handlesSignals, main, objectTypes}) =
         let
            val p = 
               T {functions = List.revMap (functions, Function.shrink),
                  handlesSignals = handlesSignals,
                  main = Function.shrink main,
                  objectTypes = objectTypes}
            val p = copyProp p
            val () = clear p
         in
            p
         end

      structure ExnStack =
         struct
            structure ZPoint =
               struct
                  datatype t = Caller | Me

                  val equals: t * t -> bool = op =

                  val toString =
                     fn Caller => "Caller"
                      | Me => "Me"

                  val layout = Layout.str o toString
               end

            structure L = FlatLattice (structure Point = ZPoint)
            open L
            structure Point = ZPoint

            val me = point Point.Me
         end

      structure HandlerLat = FlatLattice (structure Point = Label)

      structure HandlerInfo =
         struct
            datatype t = T of {block: Block.t,
                               global: ExnStack.t,
                               handler: HandlerLat.t,
                               slot: ExnStack.t,
                               visited: bool ref}

            fun new (b: Block.t): t =
               T {block = b,
                  global = ExnStack.new (),
                  handler = HandlerLat.new (),
                  slot = ExnStack.new (),
                  visited = ref false}

            fun layout (T {global, handler, slot, ...}) =
               Layout.record [("global", ExnStack.layout global),
                              ("slot", ExnStack.layout slot),
                              ("handler", HandlerLat.layout handler)]
         end

      val traceGoto =
         Trace.trace ("Rssa.checkHandlers.goto", Label.layout, Unit.layout)

      fun checkHandlers (T {functions, ...}) =
         let
            val debug = false
            fun checkFunction (f: Function.t): unit =
               let
                  val {name, start, blocks, ...} = Function.dest f
                  val {get = labelInfo: Label.t -> HandlerInfo.t,
                       rem = remLabelInfo, 
                       set = setLabelInfo} =
                     Property.getSetOnce
                     (Label.plist, Property.initRaise ("info", Label.layout))
                  val _ =
                     Vector.foreach
                     (blocks, fn b =>
                      setLabelInfo (Block.label b, HandlerInfo.new b))
                  (* Do a DFS of the control-flow graph. *)
                  fun visitLabel l = visitInfo (labelInfo l)
                  and visitInfo
                     (hi as HandlerInfo.T {block, global, handler, slot,
                                           visited, ...}): unit =
                     if !visited
                        then ()
                     else
                        let
                           val _ = visited := true
                           val Block.T {label, statements, transfer, ...} = block
                           val _ =
                              if debug
                                 then
                                    let
                                       open Layout
                                    in
                                       outputl
                                       (seq [str "visiting ",
                                             Label.layout label],
                                        Out.error)
                                    end
                              else ()
                           datatype z = datatype Statement.t
                           val {global, handler, slot} =
                              Vector.fold
                              (statements,
                               {global = global, handler = handler, slot = slot},
                               fn (s, {global, handler, slot}) =>
                               case s of
                                  SetExnStackLocal => {global = ExnStack.me,
                                                       handler = handler,
                                                       slot = slot}
                                | SetExnStackSlot => {global = slot,
                                                      handler = handler,
                                                      slot = slot}
                                | SetSlotExnStack => {global = global,
                                                      handler = handler,
                                                      slot = global}
                                | SetHandler l => {global = global,
                                                   handler = HandlerLat.point l,
                                                   slot = slot}
                                | _ => {global = global,
                                        handler = handler,
                                        slot = slot})
                           fun fail msg =
                              (Control.message
                               (Control.Silent, fn () =>
                                let open Layout
                                in align
                                   [str "before: ", HandlerInfo.layout hi,
                                    str "block: ", Block.layout block,
                                    seq [str "after: ",
                                         Layout.record
                                         [("global", ExnStack.layout global),
                                          ("slot", ExnStack.layout slot),
                                          ("handler",
                                           HandlerLat.layout handler)]],
                                    Vector.layout
                                    (fn Block.T {label, ...} =>
                                     seq [Label.layout label,
                                          str " ",
                                          HandlerInfo.layout (labelInfo label)])
                                    blocks]
                                end)
                               ; Error.bug (concat ["Rssa.checkHandlers: handler mismatch at ", msg]))
                           fun assert (msg, f) =
                              if f
                                 then ()
                              else fail msg
                           fun goto (l: Label.t): unit =
                              let
                                 val HandlerInfo.T {global = g, handler = h,
                                                    slot = s, ...} =
                                    labelInfo l
                                 val _ =
                                    assert ("goto",
                                            ExnStack.<= (global, g)
                                            andalso ExnStack.<= (slot, s)
                                            andalso HandlerLat.<= (handler, h))
                              in
                                 visitLabel l
                              end
                           val goto = traceGoto goto
                           fun tail name =
                              assert (name,
                                      ExnStack.forcePoint
                                      (global, ExnStack.Point.Caller))
                           datatype z = datatype Transfer.t
                        in
                           case transfer of
                              Arith {overflow, success, ...} =>
                                 (goto overflow; goto success)
                            | CCall {return, ...} => Option.app (return, goto)
                            | Call {return, ...} =>
                                 assert
                                 ("return",
                                  let
                                     datatype z = datatype Return.t
                                  in
                                     case return of
                                        Dead => true
                                      | NonTail {handler = h, ...} =>
                                           (case h of
                                               Handler.Caller =>
                                                  ExnStack.forcePoint
                                                  (global, ExnStack.Point.Caller)
                                             | Handler.Dead => true
                                             | Handler.Handle l =>
                                                  let
                                                     val res =
                                                        ExnStack.forcePoint
                                                        (global,
                                                         ExnStack.Point.Me)
                                                        andalso
                                                        HandlerLat.forcePoint
                                                        (handler, l)
                                                     val _ = goto l
                                                  in
                                                     res
                                                  end)
                                      | Tail => true
                                  end)
                            | Goto {dst, ...} => goto dst
                            | Raise _ => tail "raise"
                            | Return _ => tail "return"
                            | Switch s => Switch.foreachLabel (s, goto)
                        end
                  val info as HandlerInfo.T {global, ...} = labelInfo start
                  val _ = ExnStack.forcePoint (global, ExnStack.Point.Caller)
                  val _ = visitInfo info
                  val _ =
                     Control.diagnostics
                     (fn display =>
                      let
                         open Layout
                         val _ = 
                            display (seq [str "checkHandlers ",
                                          Func.layout name])
                         val _ =
                            Vector.foreach
                            (blocks, fn Block.T {label, ...} =>
                             display (seq
                                      [Label.layout label,
                                       str " ",
                                       HandlerInfo.layout (labelInfo label)]))
                      in
                         ()
                      end)
                  val _ = Vector.foreach (blocks, fn b =>
                                          remLabelInfo (Block.label b))
               in
                  ()
               end
            val _ = List.foreach (functions, checkFunction)
         in
            ()
         end

      fun checkScopes (program as T {functions, main, ...}): unit =
         let
            datatype status =
               Defined
             | Global
             | InScope
             | Undefined
            fun make (layout, plist) =
               let
                  val {get, set, ...} =
                     Property.getSet (plist, Property.initConst Undefined)
                  fun bind (x, isGlobal) =
                     case get x of
                        Global => ()
                      | Undefined =>
                           set (x, if isGlobal then Global else InScope)
                      | _ => Error.bug ("Rssa.checkScopes: duplicate definition of "
                                        ^ (Layout.toString (layout x)))
                  fun reference x =
                     case get x of
                        Global => ()
                      | InScope => ()
                      | _ => Error.bug (concat
                                        ["Rssa.checkScopes: reference to ",
                                         Layout.toString (layout x),
                                         " not in scope"])
                  fun unbind x =
                     case get x of
                        Global => ()
                      | _ => set (x, Defined)
               in (bind, reference, unbind)
               end
            val (bindVar, getVar, unbindVar) = make (Var.layout, Var.plist)
            val bindVar =
               Trace.trace2
               ("Rssa.bindVar", Var.layout, Bool.layout, Unit.layout)
               bindVar
            val getVar =
               Trace.trace ("Rssa.getVar", Var.layout, Unit.layout) getVar
            val unbindVar =
               Trace.trace ("Rssa.unbindVar", Var.layout, Unit.layout) unbindVar
            val (bindFunc, _, _) = make (Func.layout, Func.plist)
            val bindFunc = fn f => bindFunc (f, false)
            val (bindLabel, getLabel, unbindLabel) =
               make (Label.layout, Label.plist)
            val bindLabel = fn l => bindLabel (l, false)
            fun loopFunc (f: Function.t, isMain: bool): unit =
               let
                  val bindVar = fn x => bindVar (x, isMain)
                  val {args, blocks, ...} = Function.dest f
                  val _ = Vector.foreach (args, bindVar o #1)
                  val _ = Vector.foreach (blocks, bindLabel o Block.label)
                  val _ =
                     Vector.foreach
                     (blocks, fn Block.T {transfer, ...} =>
                      Transfer.foreachLabel (transfer, getLabel))
                  (* Descend the dominator tree, verifying that variable
                   * definitions dominate variable uses.
                   *)
                  val _ =
                     Tree.traverse
                     (Function.dominatorTree f,
                      fn Block.T {args, statements, transfer, ...} =>
                      let
                         val _ = Vector.foreach (args, bindVar o #1)
                         val _ =
                            Vector.foreach
                            (statements, fn s =>
                             (Statement.foreachUse (s, getVar)
                              ; Statement.foreachDef (s, bindVar o #1)))
                         val _ = Transfer.foreachUse (transfer, getVar)
                         val _ = Transfer.foreachDef (transfer, bindVar o #1)
                      in
                         fn () =>
                         if isMain
                            then ()
                         else
                            let
                               val _ =
                                  Vector.foreach
                                  (statements, fn s =>
                                   Statement.foreachDef (s, unbindVar o #1))
                               val _ =
                                  Transfer.foreachDef (transfer, unbindVar o #1)
                               val _ = Vector.foreach (args, unbindVar o #1)
                            in
                               ()
                            end
                      end)
                  val _ = Vector.foreach (blocks, unbindLabel o Block.label)
                  val _ = Vector.foreach (args, unbindVar o #1)
               in
                  ()
               end
            val _ = List.foreach (functions, bindFunc o Function.name)
            val _ = loopFunc (main, true)
            val _ = List.foreach (functions, fn f => loopFunc (f, false))
            val _ = clear program
         in ()
         end

      fun typeCheck (p as T {functions, main, objectTypes, ...}) =
         let
            val _ =
               Vector.foreach
               (objectTypes, fn ty =>
                Err.check ("objectType",
                           fn () => ObjectType.isOk ty,
                           fn () => ObjectType.layout ty))
            fun tyconTy (opt: ObjptrTycon.t): ObjectType.t =
               Vector.sub (objectTypes, ObjptrTycon.index opt)
            val () = checkScopes p
            val {get = labelBlock: Label.t -> Block.t,
                 set = setLabelBlock, ...} =
               Property.getSetOnce (Label.plist,
                                    Property.initRaise ("block", Label.layout))
            val {get = funcInfo, set = setFuncInfo, ...} =
               Property.getSetOnce (Func.plist,
                                    Property.initRaise ("info", Func.layout))
            val {get = varType: Var.t -> Type.t, set = setVarType, ...} =
               Property.getSetOnce (Var.plist,
                                    Property.initRaise ("type", Var.layout))
            val setVarType =
               Trace.trace2 ("Rssa.setVarType", Var.layout, Type.layout,
                             Unit.layout)
               setVarType
            fun checkOperand (x: Operand.t): unit =
                let
                   datatype z = datatype Operand.t
                   fun ok () =
                      case x of
                         ArrayOffset {base, index, offset, scale, ty} =>
                            (checkOperand base
                             ; checkOperand index
                             ; Type.arrayOffsetIsOk {base = Operand.ty base,
                                                     index = Operand.ty index,
                                                     offset = offset,
                                                     tyconTy = tyconTy,
                                                     result = ty,
                                                     scale = scale})
                       | Cast (z, ty) =>
                            (checkOperand z
                            ; Type.castIsOk {from = Operand.ty z,
                                             to = ty,
                                             tyconTy = tyconTy})
                       | Const _ => true
                       | EnsuresBytesFree => true
                       | GCState => true
                       | Offset {base, offset, ty} =>
                            Type.offsetIsOk {base = Operand.ty base,
                                             offset = offset,
                                             tyconTy = tyconTy,
                                             result = ty}
                       | ObjptrTycon _ => true
                       | Runtime _ => true
                       | Var {ty, var} => Type.isSubtype (varType var, ty)
                in
                   Err.check ("operand", ok, fn () => Operand.layout x)
                end
            val checkOperand =
               Trace.trace ("Rssa.checkOperand", Operand.layout, Unit.layout)
               checkOperand
            fun checkOperands v = Vector.foreach (v, checkOperand)
            fun check' (x, name, isOk, layout) =
               Err.check (name, fn () => isOk x, fn () => layout x)
            val labelKind = Block.kind o labelBlock
            fun statementOk (s: Statement.t): bool =
               let
                  datatype z = datatype Statement.t
               in
                  case s of
                     Bind {src, dst = (_, dstTy), ...} =>
                        (checkOperand src
                         ; Type.isSubtype (Operand.ty src, dstTy))
                   | Move {dst, src} =>
                        (checkOperand dst
                         ; checkOperand src
                         ; (Type.isSubtype (Operand.ty src, Operand.ty dst)
                            andalso Operand.isLocation dst))
                   | Object {dst = (_, ty), header, size} =>
                        let
                           val tycon =
                              ObjptrTycon.fromIndex
                              (Runtime.headerToTypeIndex header)
                        in
                           Type.isSubtype (Type.objptr tycon, ty)
                           andalso
                           Bytes.equals
                           (size,
                            Bytes.align
                            (size,
                             {alignment = (case !Control.align of
                                               Control.Align4 => Bytes.inWord32
                                             | Control.Align8 => Bytes.inWord64)}))
                           andalso
                           (case tyconTy tycon of
                               ObjectType.Normal {ty, ...} =>
                                  Bytes.equals
                                  (size, Bytes.+ (Runtime.normalMetaDataSize (),
                                                  Type.bytes ty))
                              | _ => false)
                        end
                   | PrimApp {args, dst, prim} =>
                        (Vector.foreach (args, checkOperand)
                         ; (Type.checkPrimApp
                            {args = Vector.map (args, Operand.ty),
                             prim = prim,
                             result = Option.map (dst, #2)}))
                   | Profile _ => true
                   | ProfileLabel _ => true
                   | SetExnStackLocal => true
                   | SetExnStackSlot => true
                   | SetHandler l =>
                        (case labelKind l of
                            Kind.Handler => true
                          | _ => false)
                   | SetSlotExnStack => true
               end
            val statementOk = 
               Trace.trace ("Rssa.statementOk",
                            Statement.layout,
                            Bool.layout)
                           statementOk
            fun gotoOk {args: Type.t vector,
                        dst: Label.t}: bool =
               let
                  val Block.T {args = formals, kind, ...} = labelBlock dst
               in
                  Vector.equals (args, formals, fn (t, (_, t')) =>
                                 Type.isSubtype (t, t'))
                  andalso (case kind of
                              Kind.Jump => true
                            | _ => false)
               end
            fun labelIsNullaryJump l = gotoOk {dst = l, args = Vector.new0 ()}
            fun tailIsOk (caller: Type.t vector option,
                          callee: Type.t vector option): bool =
               case (caller, callee) of
                  (_, NONE) => true
                | (SOME caller, SOME callee) =>
                     Vector.equals (callee, caller, Type.isSubtype)
                | _ => false
            fun nonTailIsOk (formals: (Var.t * Type.t) vector,
                             returns: Type.t vector option): bool =
               case returns of
                  NONE => true
                | SOME ts => 
                     Vector.equals (formals, ts, fn ((_, t), t') =>
                                    Type.isSubtype (t', t))
            fun callIsOk {args, func, raises, return, returns} =
               let
                  val Function.T {args = formals,
                                  raises = raises',
                                  returns = returns', ...} =
                     funcInfo func

               in
                  Vector.equals (args, formals, fn (z, (_, t)) =>
                                 Type.isSubtype (Operand.ty z, t))
                  andalso
                  (case return of
                      Return.Dead =>
                         Option.isNone raises'
                         andalso Option.isNone returns'
                    | Return.NonTail {cont, handler} =>
                         let
                            val Block.T {args = cArgs, kind = cKind, ...} =
                               labelBlock cont
                         in
                            nonTailIsOk (cArgs, returns')
                            andalso
                            (case cKind of
                                Kind.Cont {handler = h} =>
                                   Handler.equals (handler, h)
                                   andalso
                                   (case h of
                                       Handler.Caller =>
                                          tailIsOk (raises, raises')
                                     | Handler.Dead => true
                                     | Handler.Handle l =>
                                          let
                                             val Block.T {args = hArgs,
                                                          kind = hKind, ...} =
                                                labelBlock l
                                          in
                                             nonTailIsOk (hArgs, raises')
                                             andalso
                                             (case hKind of
                                                 Kind.Handler => true
                                               | _ => false)
                                          end)
                              | _ => false)
                         end
                    | Return.Tail =>
                         tailIsOk (raises, raises')
                         andalso tailIsOk (returns, returns'))
               end

            fun checkFunction (Function.T {args, blocks, raises, returns, start,
                                           ...}) =
               let
                  val _ = Vector.foreach (args, setVarType)
                  val _ =
                     Vector.foreach
                     (blocks, fn b as Block.T {args, label, statements,
                                               transfer, ...} =>
                      (setLabelBlock (label, b)
                       ; Vector.foreach (args, setVarType)
                       ; Vector.foreach (statements, fn s =>
                                         Statement.foreachDef
                                         (s, setVarType))
                       ; Transfer.foreachDef (transfer, setVarType)))
                  val _ = labelIsNullaryJump start
                  fun transferOk (t: Transfer.t): bool =
                     let
                        datatype z = datatype Transfer.t
                     in
                        case t of
                           Arith {args, overflow, prim, success, ty, ...} =>
                              let
                                 val _ = checkOperands args
                              in
                                 Prim.mayOverflow prim
                                 andalso labelIsNullaryJump overflow
                                 andalso labelIsNullaryJump success
                                 andalso
                                 Type.checkPrimApp
                                 {args = Vector.map (args, Operand.ty),
                                  prim = prim,
                                  result = SOME ty}
                              end
                         | CCall {args, func, return} =>
                              let
                                 val _ = checkOperands args
                              in
                                 CFunction.isOk (func, {isUnit = Type.isUnit})
                                 andalso
                                 Vector.equals (args, CFunction.args func,
                                                fn (z, t) =>
                                                Type.isSubtype
                                                (Operand.ty z, t))
                                 andalso
                                 case return of
                                    NONE => true
                                  | SOME l =>
                                       case labelKind l of
                                          Kind.CReturn {func = f} =>
                                             CFunction.equals (func, f)
                                        | _ => false
                              end
                         | Call {args, func, return} =>
                              let
                                 val _ = checkOperands args
                              in
                                 callIsOk {args = args,
                                           func = func,
                                           raises = raises,
                                           return = return,
                                           returns = returns}
                              end
                         | Goto {args, dst} =>
                              (checkOperands args
                               ; gotoOk {args = Vector.map (args, Operand.ty),
                                         dst = dst})
                         | Raise zs =>
                              (checkOperands zs
                               ; (case raises of
                                     NONE => false
                                   | SOME ts =>
                                        Vector.equals
                                        (zs, ts, fn (z, t) =>
                                         Type.isSubtype (Operand.ty z, t))))
                         | Return zs =>
                              (checkOperands zs
                               ; (case returns of
                                     NONE => false
                                   | SOME ts =>
                                        Vector.equals
                                        (zs, ts, fn (z, t) =>
                                         Type.isSubtype (Operand.ty z, t))))
                         | Switch s =>
                              Switch.isOk (s, {checkUse = checkOperand,
                                               labelIsOk = labelIsNullaryJump})
                     end
                  val transferOk =
                     Trace.trace ("Rssa.transferOk",
                                  Transfer.layout,
                                  Bool.layout)
                     transferOk
                  fun blockOk (Block.T {args, kind, statements, transfer, ...})
                     : bool =
                     let
                        fun kindOk (k: Kind.t): bool =
                           let
                              datatype z = datatype Kind.t
                           in
                              case k of
                                 Cont _ => true
                               | CReturn {func} =>
                                    let
                                       val return = CFunction.return func
                                    in
                                       0 = Vector.length args
                                       orelse
                                       (1 = Vector.length args
                                        andalso
                                        let
                                           val expects =
                                              #2 (Vector.first args)
                                        in
                                           Type.isSubtype (return, expects) 
                                           andalso
                                           CType.equals (Type.toCType return,
                                                         Type.toCType expects)
                                        end)
                                    end
                               | Handler => true
                               | Jump => true
                           end
                        val _ = check' (kind, "kind", kindOk, Kind.layout)
                        val _ =
                           Vector.foreach
                           (statements, fn s =>
                            check' (s, "statement", statementOk,
                                    Statement.layout))
                        val _ = check' (transfer, "transfer", transferOk,
                                        Transfer.layout)
                     in
                        true
                     end
                  val blockOk =
                     Trace.trace ("Rssa.blockOk",
                                  Block.layout,
                                  Bool.layout)
                                 blockOk

                  val _ = 
                     Vector.foreach
                     (blocks, fn b =>
                      check' (b, "block", blockOk, Block.layout))
               in
                  ()
               end
            val _ =
               List.foreach
               (functions, fn f as Function.T {name, ...} =>
                setFuncInfo (name, f))
            val _ = checkFunction main
            val _ = List.foreach (functions, checkFunction)
            val _ =
               check'
               (main, "main function",
                fn f =>
                let
                   val {args, ...} = Function.dest f
                in
                   Vector.isEmpty args
                end,
                Function.layout)
            val _ = clear p
         in
            ()
         end handle Err.E e => (Layout.outputl (Err.layout e, Out.error)
                                ; Error.bug "Rssa.typeCheck")
   end

end
