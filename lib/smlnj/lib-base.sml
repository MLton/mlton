(* lib-base.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

structure LibBase: LIB_BASE =
  struct

  (* raised to report unimplemented features *)
    exception Unimplemented of string

  (* raised to report internal errors *)
    exception Impossible of string

  (* raised by searching operations *)
    exception NotFound

  (* raise the exception Fail with a standard format message. *)
    fun failure {module, func, msg} =
	  raise (Fail(concat[module, ".", func, ": ", msg]))

    val version = {
	    date = "June 1, 1996", 
	    system = "SML/NJ Library",
	    version_id = [1, 0]: int list
	  }

    fun f ([], l) = l
      | f ([x: int], l) = (Int.toString x) :: l
      | f (x :: r, l) = (Int.toString x) :: "." :: f(r, l)

    val banner = concat (
	    #system version :: ", Version " ::
	    f (#version_id version, [", ", #date version]))

  end (* LibBase *)

