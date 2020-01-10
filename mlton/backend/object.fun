(* Copyright (C) 2020 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Object (S: OBJECT_STRUCTS): OBJECT =
struct

open S

datatype t =
   Normal of {init: {offset: Bytes.t,
                     src: Use.t} vector,
              ty: Type.t,
              tycon: ObjptrTycon.t}
 | Sequence of {elt: Type.t,
                init: {offset: Bytes.t,
                       src: Use.t} vector vector,
                tycon: ObjptrTycon.t}

fun tycon obj =
   case obj of
      Normal {tycon, ...} => tycon
    | Sequence {tycon, ...} => tycon

val ty = Type.objptr o tycon

fun metaDataSize obj =
   case obj of
      Normal _ => Runtime.normalMetaDataSize ()
    | Sequence _ => Runtime.sequenceMetaDataSize ()

fun size obj =
   case obj of
      Normal {ty, ...} => Bytes.+ (Runtime.normalMetaDataSize (), Type.bytes ty)
    | Sequence {elt, init, ...} =>
         let
            val length = Vector.length init
            val size =
               Bytes.+ (Runtime.sequenceMetaDataSize (),
                        Bytes.* (Type.bytes elt, IntInf.fromInt length))
         in
            case !Control.align of
               Control.Align4 => Bytes.alignWord32 size
             | Control.Align8 => Bytes.alignWord64 size
         end

fun fromWordXVector wv =
   let
      val ws = WordXVector.elementSize wv
      val init =
         WordXVector.toVectorMap
         (wv, fn w =>
          Vector.new1 {offset = Bytes.zero,
                       src = Use.word w})
   in
      Sequence
      {elt = Type.word ws,
       init = init,
       tycon = ObjptrTycon.wordVector ws}
   end

fun 'a foldUse (s, a: 'a, use: Use.t * 'a -> 'a): 'a =
   let
      fun useInit (init, a) =
         Vector.fold (init, a, fn ({offset = _, src}, a) =>
                      use (src, a))
   in
      case s of
         Normal {init, ...} => useInit (init, a)
       | Sequence {init, ...} => Vector.fold (init, a, useInit)
   end

fun foreachUse (s, f) = foldUse (s, (), f o #1)

fun replace (s:t, {use: Use.t -> Use.t}): t =
   let
      fun replaceInit init =
         Vector.map (init, fn {offset, src} =>
                     {offset = offset,
                      src = use src})
   in
      case s of
         Normal {init, ty, tycon} =>
            Normal {init = replaceInit init,
                    ty = ty,
                    tycon = tycon}
       | Sequence {elt, init, tycon} =>
            Sequence {elt = elt,
                      init = Vector.map (init, replaceInit),
                      tycon = tycon}
   end

fun deString {elt: Type.t,
              init: {offset: Bytes.t,
                     src: Use.t} vector vector,
              tycon = _ : ObjptrTycon.t} =
   if Type.equals (elt, Type.word WordSize.word8)
      then Exn.withEscape
           (fn escape =>
            SOME
            (String.implodeV
             (Vector.map
              (init, fn init =>
               case Use.deWord (#src (Vector.first init)) of
                  SOME w => WordX.toChar w
                | _ => escape NONE))))
      else NONE

fun layout obj =
   let
      open Layout
      val initLayout =
         Vector.layout
         (fn {offset, src} =>
          record [("offset", Bytes.layout offset),
                  ("src", Use.layout src)])
   in
      case obj of
         Normal {init, ty, tycon} =>
            seq [str "NormalObject ",
                 record [("init", initLayout init),
                         ("ty", Type.layout ty),
                         ("tycon", ObjptrTycon.layout tycon)]]
       | Sequence (arg as {elt, init, tycon}) =>
            seq [str "SequenceObject ",
                 record [("elt", Type.layout elt),
                         ("init", (case deString arg of
                                      NONE => Vector.layout initLayout init
                                    | SOME s => seq [str String.dquote,
                                                     str (String.escapeSML s),
                                                     str String.dquote])),
                         ("tycon", ObjptrTycon.layout tycon)]]
   end

val toString = Layout.toString o layout

fun isOk (obj: t,
          {checkUse: Use.t -> unit,
           tyconTy: ObjptrTycon.t -> ObjectType.t}): bool =
   let
      fun initOk (init, offsetIsOk) =
         Exn.withEscape
         (fn esc =>
          let
             val _ =
                Vector.fold
                (init, Bytes.zero, fn ({offset, src}, next) =>
                 if Bytes.>= (offset, next)
                    andalso
                    (checkUse src
                     ; offsetIsOk {offset = offset,
                                   result = Use.ty src})
                    then Bytes.+ (offset, Type.bytes (Use.ty src))
                    else esc false)
          in
             true
          end)
   in
      case obj of
         Normal {init, ty, tycon} =>
            (case tyconTy tycon of
                ObjectType.Normal {ty = ty', ...} =>
                   Type.equals (ty, ty')
                   andalso
                   let
                      val base = Type.objptr tycon
                      fun offsetIsOk {offset, result} =
                         Type.offsetIsOk
                         {base = base,
                          offset = offset,
                          tyconTy = tyconTy,
                          result = result}
                   in
                      initOk (init, offsetIsOk)
                   end
              | _ => false)
       | Sequence {elt, init, tycon} =>
            (case tyconTy tycon of
                ObjectType.Sequence {elt = elt', ...} =>
                   Type.equals (elt, elt')
                   andalso
                   let
                      val base = Type.objptr tycon
                      val index = Type.seqIndex ()
                      val scale =
                         case Scale.fromBytes (Type.bytes elt) of
                            NONE => Scale.One
                          | SOME s => s
                   in
                      Vector.forall
                      (init, fn init =>
                       let
                          fun offsetIsOk {offset, result} =
                             Type.sequenceOffsetIsOk
                             {base = base,
                              index = index,
                              offset = offset,
                              result = result,
                              scale = scale,
                              tyconTy = tyconTy}
                       in
                          initOk (init, offsetIsOk)
                       end)
                   end
              | _ => false)
   end

end
