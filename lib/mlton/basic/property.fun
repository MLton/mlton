(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Property (Plist: PROPERTY_LIST):> PROPERTY where type Plist.t = Plist.t =
struct

structure Plist = Plist

datatype ('sym, 'val) init =
   Const of 'val
 | Fun of 'sym * ('sym -> 'val) -> 'val

val initRec = Fun

fun initConst c = Const c

fun initFun f = initRec (fn (s, _) => f s)

fun initRaise (name, layout) =
   initFun
   (fn s =>
    Error.bug
    (let open Layout
     in toString (seq [layout s, str " has no ", str name, str " property"])
     end))

fun ('sym, 'val) nondestructable (plist: 'sym -> Plist.t,
                                  init: ('sym, 'val) init) =
   let
      val {add, peek, remove, ...} = Plist.newProperty ()
      fun get (s: 'sym) =
         let
            val p = plist s
         in
            case peek p of
            NONE => (case init of
                        Const c => c
                      | Fun f =>
                           let val v = f (s, get)
                           in add (p, v); v
                           end)
          | SOME v => v
         end
      fun set (s: 'sym, none: unit -> 'val, some: 'val -> unit): unit =
         let val p = plist s
         in case peek p of
            NONE => add (p, none ())
          | SOME v => some v
         end
   in {get = get, rem = remove o plist, remove = remove, set = set}
   end

fun ('sym, 'val) destructable (plist, init) =
   let
      val plists = ref []
      fun add s = List.push (plists, plist s)
      val {get, remove, set, ...} =
         nondestructable (plist,
                          case init of
                             Const _ => init
                           | Fun f => Fun (fn z as (s, _) => (add s; f z)))
      val set: 'sym * (unit -> 'val) * ('val -> unit) -> unit =
         fn (s, none, some) => set (s, fn () => (add s; none ()), some)
      fun destroy () =
         (List.foreach (!plists, remove)
          ; plists := [])
   in {destroy = destroy, get = get, set = set}
   end

fun setToSetOnce set (s, v) =
   set (s, fn _ => v, fn _ => Error.bug "Property.setOnce: set used twice")

fun destGetSetOnce z =
   let val {destroy, get, set} = destructable z
   in {destroy = destroy, get = get, set = setToSetOnce set}
   end

fun destGet z =
   let val {destroy, get, ...} = destGetSetOnce z
   in {destroy = destroy, get = get}
   end

fun getSetOnce z =
   let
      val {get, rem, set, ...} = nondestructable z
   in {get = get, rem = rem, set = setToSetOnce set}
   end

fun get z =
   let val {get, rem, ...} = getSetOnce z
   in {get = get, rem = rem}
   end

fun setInit (plist, init) =
   (plist,
    case init of
       Const c => Fun (fn _ => ref c)
     | Fun f => Fun (fn (s, get) => ref (f (s, ! o get))))

fun destGetSet z =
   let
      val {destroy, get, set} = destructable (setInit z)
      val set = fn (s, v) => set (s, fn () => ref v, fn r => r := v)
   in
      {destroy = destroy, get = ! o get, set = set}
   end

fun getSet z =
   let val {get, rem, set, ...} = nondestructable (setInit z)
      val set = fn (s, v) => set (s, fn () => ref v, fn r => r := v)
   in {get = ! o get, rem = rem, set = set}
   end

end

structure HetContainer = ExnHetContainer ()
structure PropertyList = PropertyList (ExnHetContainer ())
structure Property = Property (PropertyList)
