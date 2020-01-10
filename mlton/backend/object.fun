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
                     src: Use.t,
                     ty: Type.t} vector,
              ty: Type.t,
              tycon: ObjptrTycon.t}
 | Sequence of {elt: Type.t,
                init: {offset: Bytes.t,
                       src: Use.t,
                       ty: Type.t} vector vector,
                tycon: ObjptrTycon.t}

fun tycon obj =
   case obj of
      Normal {tycon, ...} => tycon
    | Sequence {tycon, ...} => tycon

val ty = Type.objptr o tycon

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
                       src = Use.word w,
                       ty = Type.word ws})
   in
      Sequence
      {elt = Type.word ws,
       init = init,
       tycon = ObjptrTycon.wordVector ws}
   end

fun 'a foldUse (s, a: 'a, use: Use.t * 'a -> 'a): 'a =
   let
      fun useInit (init, a) =
         Vector.fold (init, a, fn ({offset = _, src, ty = _}, a) =>
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
         Vector.map (init, fn {offset, src, ty} =>
                     {offset = offset,
                      src = use src,
                      ty = ty})
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
                     src: Use.t,
                     ty: Type.t} vector vector,
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
         (fn {offset, src, ty} =>
          record [("offset", Bytes.layout offset),
                  ("src", Use.layout src),
                  ("ty", Type.layout ty)])
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

end
