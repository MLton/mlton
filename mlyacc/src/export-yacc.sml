(* export-yacc.sml
 *
 * ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi
 *
 * $Log: export-yacc.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:40:16  george
 * Version 110.5
 *
 * Revision 1.2  1997/03/03 17:10:37  george
 * moved callcc related functions to SMLofNJ.Cont
 *
# Revision 1.1.1.1  1997/01/14  01:38:05  george
#   Version 109.24
#
 * Revision 1.3  1996/02/26  16:55:22  jhr
 * Moved exportFn/exportML to SMLofNJ structure.
 *
 * Revision 1.2  1996/02/26  15:02:32  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:45  george
 * Version 109
 * 
 *)

structure ExportParseGen : sig

    val parseGen : (string * string list) -> OS.Process.status
    val export : string -> unit

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
	  fun parse_gen () = (case argv
		 of [file] => (ParseGen.parseGen file; exit OS.Process.success)
		  | _ => (err("Usage: ml-yacc filename\n"); exit OS.Process.failure)
		(* end case *))
	  in
	    (handleInterrupt parse_gen; OS.Process.success)
	      handle Interrupt => OS.Process.failure
		   | ex => (
			err (String.concat[
			    "? ml-yacc: uncaught exception ", exnMessage ex, "\n"
			  ]);
			OS.Process.failure)
	  end

fun export heap = SMLofNJ.exportFn(heap, parseGen);

  end;


