(* cpif-dev.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(* cpif-dev.sml
 *    A simple pretty-printing device that eventually writes to a
 *    text file unless the current contents of that file coincides
 *    with what's being written.
 *
 * (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure CPIFDev : sig

    include PP_DEVICE

    val openOut : string * int -> device
    val closeOut : device -> unit

end = struct

    datatype device =
        DEV of { filename: string,
                 buffer : string list ref,
                 wid : int option ref }

    (* no style support *)
    type style = unit
    fun sameStyle _ = true
    fun pushStyle _ = ()
    fun popStyle _ = ()
    fun defaultStyle _ = ()

    (* Allocate an empty buffer and remember the file name. *)
    fun openOut (f, w) = DEV { filename = f, buffer = ref [], wid = ref (SOME w) }

    (* Calculate the final output and compare it with the current
     * contents of the file.  If they do not coincide, write the file. *)
    fun closeOut (DEV { buffer = ref l, filename, ... }) = let
        val s = concat (rev l)
        fun write () = let
            val f = TextIO.openOut filename
        in
            TextIO.output (f, s);
            TextIO.closeOut f
        end
    in
        let val f = TextIO.openIn filename
            val s' = TextIO.inputAll f
        in
            TextIO.closeIn f;
            if s = s' then () else write ()
        end handle _ => write ()
    end

    (* placeholders for the unsupported property functions *)
    fun maxDepth _ = NONE
    fun depth _ = NONE (* DEPRECATED *)
    fun setMaxDepth _ = ()
    fun ellipses _ = ("", 0)
    fun setEllipses _ = ()
    fun setEllipsesWithSz _ = ()
    fun maxIndent _ = NONE
    fun setMaxIndent _ = ()
    fun textWidth _ = NONE
    fun setTextWidth _ = ()

    (* the width of the device *)
    fun lineWidth (DEV{wid, ...}) = !wid
    fun setLineWidth (DEV{wid, ...}, w) = wid := w

    (* output a string/character in the current style to the device *)
    fun string (DEV { buffer, ... }, s) = buffer := s :: !buffer
    (* output some number of spaces to the device *)
    fun space (d, n) = string (d, StringCvt.padLeft #" " n "")
    val indent = space
    (* output a new-line to the device *)
    fun newline d = string (d, "\n")
    fun char (d, c) = string (d, String.str c)
    (* nothing to flush *)
    fun flush d = ()
end
