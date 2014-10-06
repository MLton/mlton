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

structure Kind =
   struct
      datatype t = 
	       Functional
	     | ReadState 
	     | Impure
	     | Pure
	     | Runtime of {bytesNeeded: int option, 
			   ensuresBytesFree: bool,
			   mayGC: bool,
			   maySwitchThreads: bool,
			   modifiesFrontier: bool,
			   readsStackTop: bool,
			   writesStackTop: bool}

      val toString =
         fn Functional => "functional"
          | ReadState => "readstate"
          | Impure => "impure"
	  | Pure => "pure"
	  | Runtime _ => "runtime"
(* Vedant:
Merge the below text to the toString function *)  

(*
    ("bytesNeeded", Option.layout Int.layout kind.bytesNeeded),
    ("ensuresBytesFree", Bool.layout kind.ensuresBytesFree),
    ("mayGC", Bool.layout kind.mayGC),
    ("maySwitchThreads", Bool.layout kind.maySwitchThreads),
    ("modifiesFrontier", Bool.layout kind.modifiesFrontier),
    ("readsStackTop", Bool.layout kind.readsStackTop),
    ("writesStackTop", Bool.layout kind.writesStackTop)
*)

      val layout = Layout.str o toString

      val bytesNeeded =
	 fn Runtime { bytesNeeded, ... } => bytesNeeded
	  | _ => NONE

      val ensuresBytesFree =
	 fn Runtime { ensuresBytesFree, ... } => ensuresBytesFree
	  | _ => false

      val mayGC =
	 fn Runtime { mayGC, ... } => mayGC
	  | _ => false

      val maySwitchThreads =
	 fn Runtime { maySwitchThreads, ... } => maySwitchThreads
	  | _ => false

      val modifiesFrontier =
	 fn Runtime { modifiesFrontier, ... } => modifiesFrontier
	  | _ => false
      
      val readsStackTop =
	 fn Runtime { readsStackTop, ... } => readsStackTop
	  | _ => false
      
      val writesStackTop = 
	 fn Runtime { writesStackTop, ... } => writesStackTop
	  | _ => false
   end

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
(* Vedant : *)
(*    ("kind"), Kind.layout kind,*)
    ("prototype", (fn (args,ret) => 
                   Layout.record
                   [("args", Vector.layout CType.layout args),
                    ("res", Option.layout CType.layout ret)]) prototype),
    ("return", layoutType return),
    ("symbolScope", SymbolScope.layout symbolScope),
    ("target", Target.layout target)]

local
   fun make f (T r) = f r
in
(* Vedant:
*)
   fun args z = make #args z
   fun bytesNeeded z = Kind.bytesNeeded ( make #kind z )
   fun convention z = make #convention z
   fun ensuresBytesFree z = Kind.ensuresBytesFree ( make #kind z )
   fun mayGC z = Kind.mayGC ( make #kind z )
   fun maySwitchThreads z = Kind.maySwitchThreads ( make #kind z )
   fun modifiesFrontier z = Kind.modifiesFrontier ( make #kind z )
   fun prototype z = make #prototype z
   fun readsStackTop z = Kind.readsStackTop ( make #kind z )
   fun return z = make #return z
   fun symbolScope z = make #symbolScope z
   fun target z = make #target z
   fun writesStackTop z = Kind.writesStackTop ( make #kind z )
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
      kind = Kind.Functional,
(* Vedant:
*)
(*      kind = Kind.Runtime {bytesNeeded = NONE,
			      ensuresBytesFree = false,
			      mayGC = false,
			      maySwitchThreads = false,
			      modifiesFrontier = false,
			      readsStackTop = false,
			      writesStackTop = false}
*)
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
