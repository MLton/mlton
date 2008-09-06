(* Copyright (C) 2003-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
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
                      bytesNeeded: int option,
                      convention: Convention.t,
                      ensuresBytesFree: bool,
                      mayGC: bool,
                      maySwitchThreads: bool,
                      modifiesFrontier: bool,
                      prototype: CType.t vector * CType.t option,
                      readsStackTop: bool,
                      return: 'a,
                      symbolScope: SymbolScope.t,
                      target: Target.t,
                      writesStackTop: bool}

fun layout (T {args, bytesNeeded, convention, ensuresBytesFree, mayGC,
               maySwitchThreads, modifiesFrontier, prototype, readsStackTop,
               return, symbolScope, target, writesStackTop, ...},
            layoutType) =
   Layout.record
   [("args", Vector.layout layoutType args),
    ("bytesNeeded", Option.layout Int.layout bytesNeeded),
    ("convention", Convention.layout convention),
    ("ensuresBytesFree", Bool.layout ensuresBytesFree),
    ("mayGC", Bool.layout mayGC),
    ("maySwitchThreads", Bool.layout maySwitchThreads),
    ("modifiesFrontier", Bool.layout modifiesFrontier),
    ("prototype", (fn (args,ret) => 
                   Layout.record
                   [("args", Vector.layout CType.layout args),
                    ("res", Option.layout CType.layout ret)]) prototype),
    ("readsStackTop", Bool.layout readsStackTop),
    ("return", layoutType return),
    ("symbolScope", SymbolScope.layout symbolScope),
    ("target", Target.layout target),
    ("writesStackTop", Bool.layout writesStackTop)]

local
   fun make f (T r) = f r
in
   fun args z = make #args z
   fun bytesNeeded z = make #bytesNeeded z
   fun convention z = make #convention z
   fun ensuresBytesFree z = make #ensuresBytesFree z
   fun mayGC z = make #mayGC z
   fun maySwitchThreads z = make #maySwitchThreads z
   fun modifiesFrontier z = make #modifiesFrontier z
   fun prototype z = make #prototype z
   fun readsStackTop z = make #readsStackTop z
   fun return z = make #return z
   fun symbolScope z = make #symbolScope z
   fun target z = make #target z
   fun writesStackTop z = make #writesStackTop z
end
(* quell unused warnings *)
val _ = (modifiesFrontier, readsStackTop, writesStackTop)

fun equals (f, f') = Target.equals (target f, target f')

fun map (T {args, bytesNeeded, convention, ensuresBytesFree, mayGC,
            maySwitchThreads, modifiesFrontier, prototype, readsStackTop, 
            return, symbolScope, target, writesStackTop},
         f) =
   T {args = Vector.map (args, f),
      bytesNeeded = bytesNeeded,
      convention = convention,
      ensuresBytesFree = ensuresBytesFree,
      mayGC = mayGC,
      maySwitchThreads = maySwitchThreads,
      modifiesFrontier = modifiesFrontier,
      prototype = prototype,
      readsStackTop = readsStackTop,
      return = f return,
      symbolScope = symbolScope,
      target = target,
      writesStackTop = writesStackTop}

fun isOk (T {ensuresBytesFree, mayGC, maySwitchThreads, modifiesFrontier,
             readsStackTop, return, writesStackTop, ...},
          {isUnit}): bool =
   (if maySwitchThreads
       then mayGC andalso isUnit return
    else true)
   andalso (if ensuresBytesFree orelse maySwitchThreads
               then mayGC
            else true)
   andalso (if mayGC
               then (modifiesFrontier
                     andalso readsStackTop andalso writesStackTop)
            else true)
   andalso (not writesStackTop orelse readsStackTop )

fun vanilla {args, name, prototype, return} =
   T {args = args,
      bytesNeeded = NONE,
      convention = Convention.Cdecl,
      ensuresBytesFree = false,
      mayGC = false,
      maySwitchThreads = false,
      modifiesFrontier = false,
      prototype = prototype,
      readsStackTop = false,
      return = return,
      symbolScope = SymbolScope.Private,
      target = Direct name,
      writesStackTop = false}

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
