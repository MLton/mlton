(* export-yacc.sml
 *
 * ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi
 *)
structure ExportParseGen : sig
    val parseGen : (string * string list) -> OS.Process.status
end = struct
    fun err msg = TextIO.output (TextIO.stdErr, msg)

    exception Interrupt;

    (* This function applies operation to ().  If it handles an interrupt
       signal (Control-C), it raises the exception Interrupt. Example:
       (handleInterrupt foo) handle Interrupt => print "Bang!\n" *)

    fun handleInterrupt (operation : unit -> unit) =
      let exception Done
          val old'handler = Signals.inqHandler(Signals.sigINT)
          fun reset'handler () =
            Signals.setHandler(Signals.sigINT, old'handler)
      in (SMLofNJ.Cont.callcc (fn k =>
             (Signals.setHandler(Signals.sigINT, Signals.HANDLER(fn _ => k));
               operation ();
               raise Done));
           err ("\n--- Interrupt ml-yacc ---\n");
           raise Interrupt)
          handle Done => (reset'handler ())
               | exn  => (reset'handler (); raise exn)
      end

    val exit = OS.Process.exit

    fun parseGen (_, argv) = let
	fun parse_gen () =
	    case argv of
		[file] => (ParseGen.parseGen file; exit OS.Process.success)
	      | _ => (err("Usage: ml-yacc filename\n");
		      exit OS.Process.failure)
    in
	(handleInterrupt parse_gen; OS.Process.success)
	handle Interrupt => OS.Process.failure
	     | ex => (err (String.concat ["? ml-yacc: uncaught exception ",
					  General.exnMessage ex, "\n"]);
		      OS.Process.failure)
    end
end
