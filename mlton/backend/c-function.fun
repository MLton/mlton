(* Copyright (C) 2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor CFunction (S: C_FUNCTION_STRUCTS): C_FUNCTION =
struct

open S

datatype t = T of {bytesNeeded: int option,
		   ensuresBytesFree: bool,
		   mayGC: bool,
		   maySwitchThreads: bool,
		   modifiesFrontier: bool,
		   modifiesStackTop: bool,
		   name: string,
		   needsArrayInit: bool,
		   returnTy: Type.t option}
   
fun layout (T {bytesNeeded, ensuresBytesFree, mayGC, maySwitchThreads,
	       modifiesFrontier, modifiesStackTop, name, needsArrayInit,
	       returnTy}) =
   Layout.record
   [("bytesNeeded", Option.layout Int.layout bytesNeeded),
    ("ensuresBytesFree", Bool.layout ensuresBytesFree),
    ("mayGC", Bool.layout mayGC),
    ("maySwitchThreads", Bool.layout maySwitchThreads),
    ("modifiesFrontier", Bool.layout modifiesFrontier),
    ("modifiesStackTop", Bool.layout modifiesStackTop),
    ("name", String.layout name),
    ("needsArrayInit", Bool.layout needsArrayInit),
    ("returnTy", Option.layout Type.layout returnTy)]

local
   fun make f (T r) = f r
in
   val bytesNeeded = make #bytesNeeded
   val ensuresBytesFree = make #ensuresBytesFree
   val mayGC = make #mayGC
   val maySwitchThreads = make #maySwitchThreads
   val modifiesFrontier = make #modifiesFrontier
   val modifiesStackTop = make #modifiesStackTop
   val name = make #name
   val needsArrayInit = make #needsArrayInit
   val returnTy = make #returnTy
end

fun isOk (T {ensuresBytesFree, mayGC, maySwitchThreads, modifiesFrontier,
	     modifiesStackTop, needsArrayInit, returnTy, ...}): bool =
   (if ensuresBytesFree orelse maySwitchThreads
       then mayGC
    else true)
       andalso (if mayGC
		   then modifiesFrontier andalso modifiesStackTop
		else true)
       andalso (if needsArrayInit
		   then (case returnTy of
			    NONE => false
			  | SOME t => Type.equals (t, Type.pointer))
		else true)

val isOk = Trace.trace ("CFunction.isOk", layout, Bool.layout) isOk

fun equals (T {bytesNeeded = b,
	       ensuresBytesFree = e,
	       mayGC = g,
	       maySwitchThreads = s,
	       modifiesFrontier = f,
	       modifiesStackTop = t,
	       name = n,
	       needsArrayInit = nai,
	       returnTy = r},
	    T {bytesNeeded = b',
	       ensuresBytesFree = e',
	       mayGC = g',
	       maySwitchThreads = s',
	       modifiesFrontier = f',
	       modifiesStackTop = t',
	       name = n',
	       needsArrayInit = nai',
	       returnTy = r'}) =
   b = b' andalso e = e' andalso g = g' andalso s = s' andalso f = f'
   andalso t = t' andalso n = n' andalso nai = nai'
   andalso Option.equals (r, r', Type.equals)

val equals =
   Trace.trace2 ("CFunction.equals", layout, layout, Bool.layout) equals
	 
local
   fun make b =
      T {bytesNeeded = NONE,
	 ensuresBytesFree = true,
	 mayGC = true,
	 maySwitchThreads = b,
	 modifiesFrontier = true,
	 modifiesStackTop = true,
	 name = "GC_gc",
	 needsArrayInit = false,
	 returnTy = NONE}
   val t = make true
   val f = make false
in
   fun gc {maySwitchThreads = b} = if b then t else f
end

fun vanilla {name, returnTy} =
   T {bytesNeeded = NONE,
      ensuresBytesFree = false,
      mayGC = false,
      maySwitchThreads = false,
      modifiesFrontier = false,
      modifiesStackTop = false,
      name = name,
      needsArrayInit = false,
      returnTy = returnTy}

val bug = vanilla {name = "MLton_bug",
		   returnTy = NONE}

val size = vanilla {name = "MLton_size",
		    returnTy = SOME Type.int}

val stringEqual = vanilla {name = "String_equal",
			   returnTy = SOME Type.bool}

end
