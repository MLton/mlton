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

datatype t = T of {args: CType.t vector,
		   bytesNeeded: int option,
		   convention: Convention.t,
		   ensuresBytesFree: bool,
		   mayGC: bool,
		   maySwitchThreads: bool,
		   modifiesFrontier: bool,
		   modifiesStackTop: bool,
		   name: string,
		   return: CType.t option}
   
fun layout (T {args, bytesNeeded, convention, ensuresBytesFree, mayGC,
	       maySwitchThreads, modifiesFrontier, modifiesStackTop, name,
	       return}) =
   Layout.record
   [("args", Vector.layout CType.layout args),
    ("bytesNeeded", Option.layout Int.layout bytesNeeded),
    ("convention", Convention.layout convention),
    ("ensuresBytesFree", Bool.layout ensuresBytesFree),
    ("mayGC", Bool.layout mayGC),
    ("maySwitchThreads", Bool.layout maySwitchThreads),
    ("modifiesFrontier", Bool.layout modifiesFrontier),
    ("modifiesStackTop", Bool.layout modifiesStackTop),
    ("name", String.layout name),
    ("return", Option.layout CType.layout return)]

local
   fun make f (T r) = f r
in
   val args = make #args
   val bytesNeeded = make #bytesNeeded
   val ensuresBytesFree = make #ensuresBytesFree
   val mayGC = make #mayGC
   val maySwitchThreads = make #maySwitchThreads
   val modifiesFrontier = make #modifiesFrontier
   val modifiesStackTop = make #modifiesStackTop
   val name = make #name
   val return = make #return
end

fun equals (f, f') = name f = name f'

fun isOk (T {ensuresBytesFree, mayGC, maySwitchThreads, modifiesFrontier,
	     modifiesStackTop, return, ...}): bool =
   (if maySwitchThreads
       then mayGC andalso Option.isNone return
    else true)
       andalso
       (if ensuresBytesFree orelse maySwitchThreads
	   then mayGC
	else true)
	   andalso 
	   (if mayGC
	       then (modifiesFrontier andalso modifiesStackTop)
	    else true)
	     
val isOk = Trace.trace ("CFunction.isOk", layout, Bool.layout) isOk

val equals =
   Trace.trace2 ("CFunction.equals", layout, layout, Bool.layout) equals

datatype z = datatype CType.t
datatype z = datatype Convention.t
local
   open CType
in
   datatype z = datatype WordSize.t
   val Int32 = Int (IntSize.I 32)
   val Word32 = Word W32
end
	 
local
   fun make b =
      T {args = let
		   open CType
		in
		   Vector.new5 (Pointer, Word32, Int32, Pointer, Int32)
		end,
	     bytesNeeded = NONE,
	     convention = Cdecl,
	     ensuresBytesFree = true,
	     mayGC = true,
	     maySwitchThreads = b,
	     modifiesFrontier = true,
	     modifiesStackTop = true,
	     name = "GC_gc",
	     return = NONE}
   val t = make true
   val f = make false
in
   fun gc {maySwitchThreads = b} = if b then t else f
end

fun vanilla {args, name, return} =
   T {args = args,
      bytesNeeded = NONE,
      convention = Cdecl,
      ensuresBytesFree = false,
      mayGC = false,
      maySwitchThreads = false,
      modifiesFrontier = false,
      modifiesStackTop = false,
      name = name,
      return = return}

val allocTooLarge =
   vanilla {args = Vector.new0 (),
	    name = "MLton_allocTooLarge",
	    return = NONE}
   
val bug = vanilla {args = Vector.new1 Pointer,
		   name = "MLton_bug",
		   return = NONE}
	 
val profileEnter =
   vanilla {args = Vector.new1 Pointer,
	    name = "GC_profileEnter",
	    return = NONE}
val profileInc =
   vanilla {args = Vector.new2 (Pointer, Word32),
	    name = "GC_profileInc",
	    return = NONE}
	 
val profileLeave =
   vanilla {args = Vector.new1 Pointer,
	    name = "GC_profileLeave",
	    return = NONE}

val size =
   vanilla {args = Vector.new1 Pointer,
	    name = "MLton_size",
	    return = SOME CType.defaultInt}

val returnToC =
   T {args = Vector.new0 (),
      bytesNeeded = NONE,
      convention = Cdecl,
      ensuresBytesFree = false,
      modifiesFrontier = true,
      modifiesStackTop = true,
      mayGC = true,
      maySwitchThreads = true,
      name = "Thread_returnToC",
      return = NONE}

fun prototype (T {args, convention, name, return, ...}) =
   let
      val c = Counter.new 0
      fun arg t = concat [CType.toString t, " x", Int.toString (Counter.next c)]
   in
      concat [case return of
		 NONE => "void"
	       | SOME t => CType.toString t,
	      if convention <> Convention.Cdecl
		 then concat [" __attribute__ ((",
			      Convention.toString convention,
			      ")) "]
	      else " ",
	      name, " (",
	      concat (List.separate (Vector.toListMap (args, arg), ", ")),
	      ")"]
   end

end
