(* ------------------------------------------------------------------- *
 *   testdyn2, 13/02/1995 20:53, Martin Elsman                         *
 *             15/04/1997, Niels Hallenberg                            *
 *   Dynamic test of input/output primitives...                        *
 *   Run testdyn1 prior to this test...                                *
 * ------------------------------------------------------------------- *)

open SML90
   
val _ =
let 
  fun digit n = chr(ord "0" + n)
  fun digits(n,acc) =
    if n >=0 andalso n<=9 then digit n:: acc
    else digits (n div 10, digit(n mod 10) :: acc)

  fun int_to_string(n) = if n >= 0 then implode(digits(n,[]))
			 else "~" ^ int_to_string(~n)

  fun print s = output (std_out, s)
  fun print_int_tuple (a,b) = 
    print ("("^ int_to_string a ^ "," ^ int_to_string b ^ ")\n")
  fun error b s = if b then () else print ("Error - " ^ s ^ "...\n")
  exception Fail of string
  fun fail s = (print s; raise Fail s)

  (*---------------*)
  (* testing stuff *)
  (*---------------*)
  val _ =
    let
      val _ = print "Testing [open_out, output, close_out]\n"
      val os = open_out "test.txt";
    in
      ((error (output (os, "hello\n") = ()) "output1")
       handle Io s => print ("Error - output raised Io \"" ^ s ^ "\".\n")
	    | _ => print "Error - output raised some unknown exception!!\n");
      ((error (close_out os = ()) "close_out")
       handle Io s => print ("Error - close_out raised Io \"" ^ s ^ "\".\n")
	    | _ => print "Error - close_out raised some unknown exception!!\n");
      ((error ((output (os, "wow"); false) handle (Io s) =>
	       s = "Output stream is closed") "output2")
       handle Io s => print ("Error - output raised the wrong Io exception:\n\
			     \it raised: Io \"" ^ s ^ "\".\n")
	    | _ => print "Error - output raised some unknown exception!!\n")
    end;

  val is =
    let
      val _ = print "Testing [open_in, input, lookahead, end_of_stream, close_in]\n"
      val is = ((open_in "test.txt")
		handle Io s => fail ("Error - open_in (1) raised Io \"" ^ s ^ "\".\n")
		     | _ => fail "Error - open_in (1) raised some unknown exception!!\n");
      val k = ((input(is,3))
	       handle Io s => fail ("Error - input (1) raised Io \"" ^ s ^ "\".\n")
		    | _ => fail "Error - input (1) raised some unknown exception!!\n")
    in
      error (k = "hel") ("input1, k=[" ^ k ^ "]");
      ((error (not (end_of_stream is)) "end_of_stream1")
       handle Io s => print ("Error - end_of_stream (1) raised Io \"" ^ s ^ "\".\n")
	    | _ => print "Error - end_of_stream (1) raised some unknown exception!!\n");
      ((error (lookahead is = "l") "lookahead1")
       handle Io s => print ("Error - lookahead (1) raised Io \"" ^ s ^ "\".\n")
	    | _ => print "Error - lookahead (1) raised some unknown exception!!\n");
      is
    end

  val _ =
    let
      val j = ((input(is,3))
	       handle Io s => fail ("Error - input (2) raised Io \"" ^ s ^ "\".\n")
		    | _ => fail "Error - input (2) raised some unknown exception!!\n")
    in
      error (j = "lo\n") ("input2, j=[" ^ j ^ "]");
      ((error (end_of_stream is) "end_of_stream2")
       handle Io s => print ("Error - end_of_stream (2) raised Io \"" ^ s ^ "\".\n")
	    | _ => print "Error - end_of_stream (2) raised some unknown exception!!\n");
      ((error ((lookahead is) = "") "lookahead2")
       handle Io s => print ("Error - lookahead (2) raised Io \"" ^ s ^ "\".\n")
	    | _ => print "Error - lookahead (2) raised some unknown exception!!\n");
      ((error (close_in is = ()) "close_in") 
       handle Io s => print ("Error - close_in (1) raised Io \"" ^ s ^ "\".\n")
	    | _ => print "Error - close_in (1) raised some unknown exception!!\n");
      ((error ((open_in("I_do_not_exist");false) handle (Io s) =>
	       s = "Cannot open I_do_not_exist") "open_in")
	handle Io s => print ("Error - open_in (2) raised Io \"" ^ s ^ "\".\n")
	     | _ => print "Error - open_in (2) raised some unknown exception!!\n")
    end

(*   val _ =
 *     let
 *       val _ = print "Testing [std_in, input] (interactively)\n"
 *       val _ = print "\nWrite the word 'Potato' (and return)\n\n"
 *       val s = ((input(std_in, 7))
 * 	       handle Io s => fail ("Error - input (3) raised Io \"" ^ s ^ "\".\n")
 * 		    | _ => fail "Error - input (3) raised some unknown exception!!\n")
 *       val _ = print "\nNow, write the word 'hello' (and return)\n\n"
 *       val s2 = ((input(std_in, 6))
 * 		handle Io s => fail ("Error - input (3) raised Io \"" ^ s ^ "\".\n")
 * 		     | _ => fail "Error - input (3) raised some unknown exception!!\n")
 *     in
 *       error (s = "Potato\n") "std_in, input (Potato)";
 *       error (s2 = "hello\n") "std_in, input (hello)"
 *     end
 *)

in
  print "End of test.\n"
end



