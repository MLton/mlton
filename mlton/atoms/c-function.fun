functor CFunction (S: C_FUNCTION_STRUCTS): C_FUNCTION = 
struct

open S

structure Type = RepType
structure CType = Type.CType

local
   open Type
in
   structure IntSize = IntSize
   structure RealSize = RealSize
   structure WordSize = WordSize
end

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

datatype t = T of {args: Type.t vector,
		   bytesNeeded: int option,
		   convention: Convention.t,
		   ensuresBytesFree: bool,
		   mayGC: bool,
		   maySwitchThreads: bool,
		   modifiesFrontier: bool,
		   modifiesStackTop: bool,
		   name: string,
		   return: Type.t}
   
fun layout (T {args, bytesNeeded, convention, ensuresBytesFree, mayGC,
	       maySwitchThreads, modifiesFrontier, modifiesStackTop, name,
	       return, ...}) =
   Layout.record
   [("args", Vector.layout Type.layout args),
    ("bytesNeeded", Option.layout Int.layout bytesNeeded),
    ("convention", Convention.layout convention),
    ("ensuresBytesFree", Bool.layout ensuresBytesFree),
    ("mayGC", Bool.layout mayGC),
    ("maySwitchThreads", Bool.layout maySwitchThreads),
    ("modifiesFrontier", Bool.layout modifiesFrontier),
    ("modifiesStackTop", Bool.layout modifiesStackTop),
    ("name", String.layout name),
    ("return", Type.layout return)]
   
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
       then mayGC andalso RepType.isUnit return
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

datatype z = datatype Convention.t

local
   open Type
in
   val Int32 = int (IntSize.I (Bits.fromInt 32))
   val Word32 = word (Bits.fromInt 32)
   val bool = bool
   val cPointer = cPointer
   val gcState = gcState
   val string = word8Vector
   val unit = unit
end
   
local
   fun make b =
      T {args = let
		   open Type
		in
		   Vector.new5 (gcState, Word32, bool, cPointer (), Int32)
		end,
	  bytesNeeded = NONE,
	     convention = Cdecl,
	     ensuresBytesFree = true,
	     mayGC = true,
	     maySwitchThreads = b,
	     modifiesFrontier = true,
	     modifiesStackTop = true,
	     name = "GC_gc",
	     return = unit}
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
	    return = unit}
   
val bug = vanilla {args = Vector.new1 string,
		   name = "MLton_bug",
		   return = unit}

val profileEnter =
   vanilla {args = Vector.new1 gcState,
	    name = "GC_profileEnter",
	    return = unit}

val profileInc =
   vanilla {args = Vector.new2 (gcState, Word32),
	    name = "GC_profileInc",
	    return = unit}
	 
val profileLeave =
   vanilla {args = Vector.new1 gcState,
	    name = "GC_profileLeave",
	    return = unit}

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
      return = unit}

fun prototype (T {args, convention, name, return, ...}) =
   let
      val c = Counter.new 0
      fun arg t = concat [CType.toString (Type.toCType t),
			  " x", Int.toString (Counter.next c)]
   in
      concat [if Type.isUnit return
		 then "void"
	      else CType.toString (Type.toCType return),
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
