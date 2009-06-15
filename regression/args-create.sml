val tests = [
  "\"hello\\\"",
  "c:\\foo.bah",
  "",
  "hi\\",
  "hi\"",
  "evil\narg",
  "evil\targ",
  "evil arg",
  "evil\rarg",
  "evil\farg",
  "\"bar\\",
  "\\bah",
  "bah \\bar",
  "bah\\bar",
  "bah\\\\",
  "ba h\\\\",
  "holy\"smoke",
  "holy \"smoke" ]

val cmd = CommandLine.name ()
val args = CommandLine.arguments ()

fun loop ([], []) = print "OK!\n"
  | loop (x::r, y::s) = 
      (if x <> y then print ("FAIL: "^x^":"^y^"\n") else (); loop (r, s))
  | loop (_, _) = print "Wrong argument count\n"

open MLton.Process
val () =
  if List.length args = 0
  then let
          val pid =
             create {args = (*cmd::*)tests,
                     env = NONE,
                     path = cmd,
                     stderr = Param.self,
                     stdin = Param.self,
                     stdout = Param.self}
          val status = reap pid
       in
          ()
       end
  else loop (tests, args)
