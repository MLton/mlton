(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_RANDOM =
   sig
      (* Return a random alphanumeric character. *)
      val alphaNumChar: unit -> char

      (* Return a string of random alphanumeric characters of specified
       * length.
       *)
      val alphaNumString: int -> string

      (* Get the next pseudrandom. *)
      val rand: unit -> word

      (* Use /dev/random to get a word.  Useful as an arg to srand.
       * Return NONE if /dev/random can't be read.
       *)
      val seed: unit -> word option

      (* Set the seed used by rand. *)
      val srand: word -> unit

      (* Use /dev/urandom to get a word.  Useful as an arg to srand.
       * Return NONE if /dev/urandom can't be read.
       *)
      val useed: unit -> word option
   end
