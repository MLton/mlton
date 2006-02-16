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
                 wid : int }

    (* no style support *)
    type style = unit
    fun sameStyle _ = true
    fun pushStyle _ = ()
    fun popStyle _ = ()
    fun defaultStyle _ = ()

    (* Allocate an empty buffer and remember the file name. *)
    fun openOut (f, w) = DEV { filename = f, buffer = ref [], wid = w }

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

    (* maximum printing depth (in terms of boxes) *)
    fun depth _ = NONE

    (* the width of the device *)
    fun lineWidth (DEV{wid, ...}) = SOME wid
    (* the suggested maximum width of text on a line *)
    fun textWidth _ = NONE

    (* output a string/character in the current style to the device *)
    fun string (DEV { buffer, ... }, s) = buffer := s :: !buffer

    fun char (d, c) = string (d, String.str c)
    fun space (d, n) = string (d, StringCvt.padLeft #" " n "")
    fun newline d = string (d, "\n")

    fun flush d = ()
end
