(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature MACHINE_CASES_STRUCTS = 
   sig
      structure Label: ID
   end

signature MACHINE_CASES = 
   sig
      include MACHINE_CASES_STRUCTS
      
      datatype t =
	 Char of (char * Label.t) list
       | Int of (int * Label.t) list
       | Word of (word * Label.t) list

      val fold: t * 'a * (Label.t * 'a -> 'a) -> 'a
      val forall: t * (Label.t -> bool) -> bool
      val foreach: t * (Label.t -> unit) -> unit
      val layout: t -> Layout.t
      val length: t -> int
   end


functor TestMachineCases (S: MACHINE_CASES) = 
struct

open S

val _ = Assert.assert("MachineCases", fn () => true)

end

