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
		      modifiesStackTop: bool,
		      name: string,
		      return: 'a}
   
fun layout (T {args, bytesNeeded, convention, ensuresBytesFree, mayGC,
	       maySwitchThreads, modifiesFrontier, modifiesStackTop, name,
	       return, ...},
	    layoutType) =
   Layout.record
   [("args", Vector.layout layoutType args),
    ("bytesNeeded", Option.layout Int.layout bytesNeeded),
    ("convention", Convention.layout convention),
    ("ensuresBytesFree", Bool.layout ensuresBytesFree),
    ("mayGC", Bool.layout mayGC),
    ("maySwitchThreads", Bool.layout maySwitchThreads),
    ("modifiesFrontier", Bool.layout modifiesFrontier),
    ("modifiesStackTop", Bool.layout modifiesStackTop),
    ("name", String.layout name),
    ("return", layoutType return)]
   
local
   fun make f (T r) = f r
in
   fun args z = make #args z
   fun bytesNeeded z = make #bytesNeeded z
   fun ensuresBytesFree z = make #ensuresBytesFree z
   fun mayGC z = make #mayGC z
   fun maySwitchThreads z = make #maySwitchThreads z
   fun modifiesFrontier z = make #modifiesFrontier z
   fun modifiesStackTop z = make #modifiesStackTop z
   fun name z = make #name z
   fun return z = make #return z
end

fun equals (f, f') = name f = name f'

fun map (T {args, bytesNeeded, convention, ensuresBytesFree, mayGC,
	    maySwitchThreads, modifiesFrontier, modifiesStackTop, name,
	    return},
	 f) =
   T {args = Vector.map (args, f),
      bytesNeeded = bytesNeeded,
      convention = convention,
      ensuresBytesFree = ensuresBytesFree,
      mayGC = mayGC,
      maySwitchThreads = maySwitchThreads,
      modifiesFrontier = modifiesFrontier,
      modifiesStackTop = modifiesStackTop,
      name = name,
      return = f return}
   
fun isOk (T {ensuresBytesFree, mayGC, maySwitchThreads, modifiesFrontier,
	     modifiesStackTop, return, ...},
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
	       then (modifiesFrontier andalso modifiesStackTop)
	    else true)

fun vanilla {args, name, return} =
   T {args = args,
      bytesNeeded = NONE,
      convention = Convention.Cdecl,
      ensuresBytesFree = false,
      mayGC = false,
      maySwitchThreads = false,
      modifiesFrontier = false,
      modifiesStackTop = false,
      name = name,
      return = return}

end
