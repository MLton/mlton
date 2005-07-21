(* export-lex.sml
 *
 * Revision 1.2  2000/03/07 04:01:05  blume
 * - build script now use new ml-build mechanism
 *)
structure ExportLexGen : sig
    val lexGen : (string * string list) -> OS.Process.status
end = struct

    exception Interrupt

  (* This function applies operation to ().  If it handles an interrupt
   * signal (Control-C), it raises the exception Interrupt.  Example:
   * (handleInterrupt foo) handle Interrupt => print "Bang!\n"
   *)
    fun handleInterrupt (operation : unit -> unit) =
      let exception Done
          val old'handler = Signals.inqHandler(Signals.sigINT)
          fun reset'handler () =
            Signals.setHandler(Signals.sigINT, old'handler)
      in (SMLofNJ.Cont.callcc (fn k =>
             (Signals.setHandler(Signals.sigINT, Signals.HANDLER(fn _ => k));
               operation ();
               raise Done));
           raise Interrupt)
          handle Done => (reset'handler ())
               | exn  => (reset'handler (); raise exn)
      end

    fun err msg = TextIO.output(TextIO.stdErr, String.concat msg)

    fun lexGen (name, args) = let
	fun lex_gen () =
	    case args of
		[] => (err [name, ": missing filename\n"];
		       OS.Process.exit OS.Process.failure)
	      | files => List.app LexGen.lexGen files
    in
	(handleInterrupt lex_gen; OS.Process.success)
	handle Interrupt => (err [name, ": Interrupt\n"]; OS.Process.failure)
	     | any => (err [name, ": uncaught exception ",
			    General.exnMessage any, "\n"];
		       OS.Process.failure)
    end
end
