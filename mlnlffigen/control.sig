(* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature CONTROL =
   sig
      (* set all flags to their default values *)
      val defaults: unit -> unit

      (*------------------------------------*)
      (*            Begin Flags             *)
      (*------------------------------------*)
      val debug : bool ref

      val allSU : bool ref

      val collect_enums : bool ref

      val cppopts : string list ref

      val dir : string ref

      val enum_cons : bool ref

      val extramembers : string list ref

      val gensym : string ref

      val libhandle : string ref

      structure Linkage :
	 sig
	    datatype t = Dynamic | Static
	 end
      val linkage : Linkage.t ref

      val match : (string -> bool) ref

      val mlbfile : string ref

      val namedargs : bool ref

      val prefix : string ref

      structure Target :
	 sig
	    type t
	    val fromString : string -> t option
	    val make: t -> {name: string, sizes: Sizes.sizes,
			    endianShift: Endian.shift} option
	 end
      val target: {name: string, sizes: Sizes.sizes,
		   endianShift: Endian.shift} option ref

      val weight: {heavy: bool, light: bool} ref

      val width : int ref
   end
