(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure File:> FILE =
struct

structure FS = OS.FileSys
   
type t = string
type dir = string

fun toString f = f
val layout = Layout.str o toString

val size = FS.fileSize
val modTime = FS.modTime

fun isNewer (f1, f2) = Time.>= (modTime f1, modTime f2)

fun withh (file, p, openn, close) =
   let
      val stream =
	 (openn file) handle IO.Io _ => Error.bug (concat ["cannot open ", file])
   in DynamicWind.wind (fn () => p stream,
			fn () =>
			close stream
			handle e =>
			   Error.bug (concat ["cannot close ", file,
					      " due to ",
					      Layout.toString (Exn.layout e)]))
   end 

fun withOut (f, p) = withh (f, p, Out.openOut, Out.close)
fun withAppend (f, p) = withh (f, p, Out.openAppend, Out.close)
fun withIn (f, p) = withh (f, p, In.openIn, In.close)

fun appendTo (f, s) = withAppend (f, fn out => Out.output (out, s))

fun foldLines (f, ac, trans) =
   withIn (f, fn ins => In.foldLines (ins, ac, trans))

local
   fun can a f = FS.access (f, a)
in
   val canWrite = can [FS.A_WRITE]
   val canRead = can [FS.A_READ]
   val doesExist = can []
end

fun remove f =
   if doesExist f
      then (FS.remove f
	    handle e => Error.bug (concat ["cannot remove ", f,
					   " due to ",
					   Layout.toString (Exn.layout e)]))
   else ()

local
   fun ensure (pred, msg) f =
      if pred f then ()
      else Error.bug (concat ["can not ", msg, " ", f])
in
   val ensureWrite = ensure (canWrite, "write")
   val ensureRead = ensure (canRead, "read")
end

fun sameContents (f1, f2) =
   size f1 = size f2
   andalso withIn (f1, fn in1 =>
		   withIn (f2, fn in2 =>
			   In.sameContents (in1, in2)))

fun output (file, out) = Out.output (out, file)
   
fun outputContents (file, out) =
   withIn (file, fn ins => In.outputAll (ins, out))

fun lines f = withIn (f, In.lines)
   
fun contents file = withIn (file, In.inputAll)

fun move {from, to} = FS.rename {old = from, new = to}
   
fun copy (source, dest) =
   withOut (dest, fn out => outputContents (source, out))
   
fun concat (sources, dest) =
   withOut (dest, fn out =>
	   List.foreach (sources, fn f => outputContents (f, out)))

val temp = MLton.TextIO.mkstemps

fun tempName z =
   let
      val (f, out) = temp z
      val _ = Out.close out
   in
      f
   end

fun withTemp f =
   let
      val name = tempName {prefix = "/tmp/file", suffix = ""}
   in
      DynamicWind.wind (fn () => f name, fn () => remove name)
   end
   
fun withTempOut' (z, f, g) =
   let
      val (name, out) = temp z
   in
      DynamicWind.wind (fn () =>
			(DynamicWind.wind (fn () => f out,
					   fn () => Out.close out)
			 ; g name),
			fn () => remove name)
   end

fun withTempOut (f, g) =
   withTempOut' ({prefix = "/tmp/file", suffix = ""}, f, g)

fun withString (s, f) =
   withTempOut (fn out => Out.output (out, s), f)
   
fun withOutIn (fout, fin) =
   withTempOut (fout, fn tmp => withIn (tmp, fin))

fun withStringIn (s, fin) =
   withOutIn (fn out => Out.output (out, s),
	      fin)

fun create f = withOut (f, fn _ => ())

val suffix = #ext o OS.Path.splitBaseExt

local open OS.Path
in 
   val base = base
   val dirOf = dir
   val extension = ext
end

end
