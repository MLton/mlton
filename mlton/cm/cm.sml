(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure CM: CM =
struct

fun fail s = raise Fail s

val maxAliasNesting: int = 32

fun cm {cmfile: File.t} =
   let
      val files = ref []
      (* The files in seen are absolute. *)
      val seen = String.memoize (fn _ => ref false)
      fun loop (cmfile: File.t,
		nesting: int,
		relativize: Dir.t option): unit =
	 let
	    val relativize =
	       case relativize of
		  NONE => NONE
		| _ => if OS.Path.isAbsolute cmfile
			  then NONE
		       else relativize
	    val {dir, file} = OS.Path.splitDirFile cmfile
	 in
	    if not (File.doesExist cmfile)
	       then fail (concat [cmfile, " does not exist"])
	    else 
	       Dir.inDir
	       (if dir = "" then "." else dir, fn () =>
		   let
		      val cwd = Dir.current ()
		      fun abs f = OS.Path.mkAbsolute (f, cwd)
		      fun finalize f =
			 case relativize of
			    NONE => abs f
			  | SOME d =>
			       OS.Path.mkRelative (f, d)
		      datatype z = datatype Parse.result
		   in case Parse.parse {cmfile = file} of
		      Alias f =>
			 if nesting > maxAliasNesting
			    then fail (concat [finalize cmfile,
					       ": alias nesting too deep."])
			       
			 else loop (f, nesting + 1, relativize)
		    | Bad s =>
			 fail (concat [finalize cmfile, ": bad CM file: ", s])
		    | CanNotRead =>
			 fail (concat [finalize cmfile, ": can not read"])
		    | Members members =>
			 List.foreach
			 (members, fn m =>
			  let
			     val m' = abs m
			     val seen = seen m'
			  in
			     if !seen
				then ()
			     else let
				     val _ = seen := true
				     fun sml () = List.push (files, finalize m')
				  in case File.suffix m of
				     SOME "cm" => loop (m, 0, relativize)
				   | SOME "sml" => sml ()
				   | SOME "sig" => sml ()
				   | SOME "fun" => sml ()
				   | SOME "ML" => sml ()
				   | _ =>
					fail (concat
					      [finalize file,
					       ": MLton can't process ", m])
				  end
			  end)
		   end)
	 end 
      val d = Dir.current ()
      val _ = loop (cmfile, 0, SOME d)
      val files = rev (!files)
   in files
   end

end
