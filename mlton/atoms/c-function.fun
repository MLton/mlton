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

datatype 'a t = T of {args: 'a vector,
		      bytesNeeded: int option,
		      convention: Convention.t,
		      ensuresBytesFree: bool,
		      mayGC: bool,
		      maySwitchThreads: bool,
		      modifiesFrontier: bool,
		      name: string,
		      readsStackTop: bool,
		      return: 'a,
		      writesStackTop: bool}
   
fun layout (T {args, bytesNeeded, convention, ensuresBytesFree, mayGC,
	       maySwitchThreads, modifiesFrontier, name, readsStackTop,
	       return, writesStackTop, ...},
	    layoutType) =
   Layout.record
   [("args", Vector.layout layoutType args),
    ("bytesNeeded", Option.layout Int.layout bytesNeeded),
    ("convention", Convention.layout convention),
    ("ensuresBytesFree", Bool.layout ensuresBytesFree),
    ("mayGC", Bool.layout mayGC),
    ("maySwitchThreads", Bool.layout maySwitchThreads),
    ("modifiesFrontier", Bool.layout modifiesFrontier),
    ("name", String.layout name),
    ("readsStackTop", Bool.layout readsStackTop),
    ("return", layoutType return),
    ("writesStackTop", Bool.layout writesStackTop)]
   
local
   fun make f (T r) = f r
in
   fun args z = make #args z
   fun bytesNeeded z = make #bytesNeeded z
   fun ensuresBytesFree z = make #ensuresBytesFree z
   fun mayGC z = make #mayGC z
   fun maySwitchThreads z = make #maySwitchThreads z
   fun modifiesFrontier z = make #modifiesFrontier z
   fun name z = make #name z
   fun readsStackTop z = make #readsStackTop z
   fun return z = make #return z
   fun writesStackTop z = make #writesStackTop z
end

fun equals (f, f') = name f = name f'

fun map (T {args, bytesNeeded, convention, ensuresBytesFree, mayGC,
	    maySwitchThreads, modifiesFrontier, name, readsStackTop, return,
	    writesStackTop},
	 f) =
   T {args = Vector.map (args, f),
      bytesNeeded = bytesNeeded,
      convention = convention,
      ensuresBytesFree = ensuresBytesFree,
      mayGC = mayGC,
      maySwitchThreads = maySwitchThreads,
      modifiesFrontier = modifiesFrontier,
      name = name,
      readsStackTop = readsStackTop,
      return = f return,
      writesStackTop = writesStackTop}
   
fun isOk (T {ensuresBytesFree, mayGC, maySwitchThreads, modifiesFrontier,
	     readsStackTop, return, writesStackTop, ...},
	  {isUnit}): bool =
   (if maySwitchThreads
       then mayGC andalso isUnit return
    else true)
       andalso
       (if ensuresBytesFree orelse maySwitchThreads
	   then mayGC
	else true)
	   andalso 
	   (if mayGC
	       then (modifiesFrontier
		     andalso readsStackTop andalso writesStackTop)
	    else true)

fun vanilla {args, name, return} =
   T {args = args,
      bytesNeeded = NONE,
      convention = Convention.Cdecl,
      ensuresBytesFree = false,
      mayGC = false,
      maySwitchThreads = false,
      modifiesFrontier = false,
      name = name,
      readsStackTop = false,
      return = return,
      writesStackTop = false}

end
