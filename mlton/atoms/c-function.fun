(* Copyright (C) 2015 Matthew Fluet.
 * Copyright (C) 2003-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CFunction (S: C_FUNCTION_STRUCTS): C_FUNCTION = 
struct

open S

structure Convention =
   struct
      datatype t =
         Cdecl
       | Stdcall

      val toString =
         fn Cdecl => "cdecl"
          | Stdcall => "stdcall"

      val layout = Layout.str o toString
   end

structure Kind =
   struct
      datatype t =
         Impure
       | Pure
       | Runtime of {bytesNeeded: int option,
                     ensuresBytesFree: bool,
                     mayGC: bool,
                     maySwitchThreads: bool,
                     modifiesFrontier: bool,
                     readsStackTop: bool,
                     writesStackTop: bool}

      val runtimeDefault = Runtime {bytesNeeded = NONE,
                                    ensuresBytesFree = false,
                                    mayGC = true,
                                    maySwitchThreads = false,
                                    modifiesFrontier = true,
                                    readsStackTop = true,
                                    writesStackTop = true}
      val pure = Pure
      val impure = Impure
      val reentrant = runtimeDefault

      fun layout k =
         case k of
            Impure => Layout.str "Impure"
          | Pure => Layout.str "Pure"
          | Runtime {bytesNeeded, ensuresBytesFree, mayGC,
                     maySwitchThreads, modifiesFrontier,
                     readsStackTop, writesStackTop} =>
               Layout.namedRecord
               ("Runtime",
                [("bytesNeeded", Option.layout Int.layout bytesNeeded),
                 ("ensuresBytesFree", Bool.layout ensuresBytesFree),
                 ("mayGC", Bool.layout mayGC),
                 ("maySwitchThreads", Bool.layout maySwitchThreads),
                 ("modifiesFrontier", Bool.layout modifiesFrontier),
                 ("readsStackTop", Bool.layout readsStackTop),
                 ("writesStackTop", Bool.layout writesStackTop)])

      val toString = Layout.toString o layout

      local
         fun make (sel, default) k =
            case k of
               Impure => default
             | Pure => default
             | Runtime r => sel r
         fun makeBool sel = make (sel, false)
         fun makeOpt sel = make (sel, NONE)
      in
         val bytesNeeded = makeOpt #bytesNeeded
         val ensuresBytesFree = makeBool #ensuresBytesFree
         val mayGC = makeBool #mayGC
         val maySwitchThreads = makeBool #maySwitchThreads
         val modifiesFrontier = makeBool #modifiesFrontier
         val readsStackTop = makeBool #readsStackTop
         val writesStackTop = makeBool #writesStackTop
      end
   end

structure SymbolScope =
   struct
      datatype t =
         External
       | Private
       | Public

      val toString =
         fn External => "external"
          | Private => "private"
          | Public => "public"

      val layout = Layout.str o toString
   end

structure Target =
   struct
      datatype t =
         Direct of string
       | Indirect

      val toString =
         fn Direct name => name
          | Indirect => "<*>"

      val layout = Layout.str o toString

      val equals =
         fn (Direct name, Direct name') => name = name'
          | (Indirect, Indirect) => true
          | _ => false
   end
datatype z = datatype Target.t

datatype 'a t = T of {args: 'a vector,
                      convention: Convention.t,
                      kind: Kind.t,
                      prototype: CType.t vector * CType.t option,
                      return: 'a,
                      symbolScope: SymbolScope.t,
                      target: Target.t}

fun layout (T {args, convention, kind, prototype, return, symbolScope, target, ...},
            layoutType) =
   Layout.record
   [("args", Vector.layout layoutType args),
    ("convention", Convention.layout convention),
    ("kind", Kind.layout kind),
    ("prototype", (fn (args,ret) => 
                   Layout.record
                   [("args", Vector.layout CType.layout args),
                    ("res", Option.layout CType.layout ret)]) prototype),
    ("return", layoutType return),
    ("symbolScope", SymbolScope.layout symbolScope),
    ("target", Target.layout target)]

local
   fun make f (T r) = f r
   fun makeKind f (T r) = f (#kind r)
in
   fun args z = make #args z
   fun bytesNeeded z = makeKind Kind.bytesNeeded z
   fun convention z = make #convention z
   fun ensuresBytesFree z = makeKind Kind.ensuresBytesFree z
   fun mayGC z = makeKind Kind.mayGC z
   fun maySwitchThreads z = makeKind Kind.maySwitchThreads z
   fun modifiesFrontier z = makeKind Kind.modifiesFrontier z
   fun prototype z = make #prototype z
   fun readsStackTop z = makeKind Kind.readsStackTop z
   fun return z = make #return z
   fun symbolScope z = make #symbolScope z
   fun target z = make #target z
   fun writesStackTop z = makeKind Kind.writesStackTop z
end
(* quell unused warnings *)
val _ = (modifiesFrontier, readsStackTop, writesStackTop)

fun equals (f, f') = Target.equals (target f, target f')

fun map (T {args, convention, kind, prototype, return, symbolScope, target},
         f) =
   T {args = Vector.map (args, f),
      convention = convention,
      kind = kind,
      prototype = prototype,
      return = f return,
      symbolScope = symbolScope,
      target = target}

fun isOk (T {kind, return, ...},
          {isUnit}): bool =
   (if Kind.maySwitchThreads kind
       then Kind.mayGC kind andalso isUnit return
    else true)
   andalso (if Kind.ensuresBytesFree kind orelse Kind.maySwitchThreads kind
               then Kind.mayGC kind
            else true)
   andalso (if Kind.mayGC kind
               then (Kind.modifiesFrontier kind
                     andalso Kind.readsStackTop kind andalso Kind.writesStackTop kind)
            else true)
   andalso (not (Kind.writesStackTop kind) orelse Kind.readsStackTop kind)

fun vanilla {args, name, prototype, return} =
   T {args = args,
      convention = Convention.Cdecl,
      kind = Kind.Impure,
      prototype = prototype,
      return = return,
      symbolScope = SymbolScope.Private,
      target = Direct name}

fun cPrototype (T {convention, prototype = (args, return), symbolScope, target, 
                   ...}) =
   let
      val convention =
         if convention <> Convention.Cdecl
            then concat [" __attribute__ ((",
                         Convention.toString convention,
                         ")) "]
         else " "
      val symbolScope =
         case symbolScope of
            SymbolScope.External => "EXTERNAL "
          | SymbolScope.Private => "PRIVATE "
          | SymbolScope.Public => "PUBLIC "
      val name = 
         case target of
            Direct name => name
          | Indirect => Error.bug "CFunction.cPrototype: Indirect"
      val c = Counter.new 0
      fun arg t =
         concat [CType.toString t, " x", Int.toString (Counter.next c)]
      val return =
         case return of
            NONE => "void"
          | SOME t => CType.toString t
   in
      concat [symbolScope, return, convention, name,
              " (",
              concat (List.separate (Vector.toListMap (args, arg), ", ")),
              ")"]
   end

fun cPointerType (T {convention, prototype = (args, return), ...}) =
   let
      val attributes =
         if convention <> Convention.Cdecl
            then concat [" __attribute__ ((",
                         Convention.toString convention,
                         ")) "]
         else " "
      fun arg t = CType.toString t
      val return =
         case return of
            NONE => "void"
          | SOME t => CType.toString t
   in
      concat
      ["(", return, attributes, 
       "(*)(",       
       concat (List.separate (Vector.toListMap (args, arg), ", ")),
       "))"]
   end

end
