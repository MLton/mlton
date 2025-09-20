structure Main =
struct
  local
    val (TextPrimIO.RD reader, _) =
      TextIO.StreamIO.getReader (TextIO.getInstream TextIO.stdIn)
    val iodesc = Option.valOf (#ioDesc reader)
    val polldesc = OS.IO.pollIn (Option.valOf (OS.IO.pollDesc iodesc))
  in
    val readVecNB = Option.valOf (#readVecNB reader)
    val readStdInEvt = IOManager.ioEvt polldesc
  end

  fun echoOrTimeoutLoop () =
    let
      val () = TextIO.print "Start of loop:\n"
    in
      CML.select
        [ CML.wrap (readStdInEvt, fn (_: IOManager.poll_info) =>
            ( TextIO.print "Received stdin event\n"
            ; case readVecNB 1000 of
                SOME s => TextIO.print ("Read string: " ^ s ^ "\n")
              | NONE => TextIO.print "Read will block\n"
            ))
        , CML.wrap (CML.timeOutEvt (Time.fromSeconds 3), fn () =>
            TextIO.print "Timed out\n")
        ];
      echoOrTimeoutLoop ()
    end

  fun cmlMain () =
    echoOrTimeoutLoop ()
    handle e =>
      TextIO.print
        ("Exception name: " ^ exnName e ^ " message: " ^ exnMessage e ^ "\n")

  fun doit _ =
    let
      val status = RunCML.doit (cmlMain, NONE)
    in
      if OS.Process.isSuccess status then print "Exit success\n"
      else print "Exit failure\n"
    end
end
