(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature DECS_STRUCTS =
   sig
      structure CoreML: CORE_ML
   end

signature DECS =
   sig
      include DECS_STRUCTS
      
      type dec = CoreML.Dec.t
	 
      type t

      val add: t * dec -> t      (* add a dec to the end of the list *)
      val append: t * t -> t
      val appends: t list -> t
      val appendsV: t vector -> t
      val cons: dec * t -> t
      val empty: t
      val fold: t * 'a * (dec * 'a -> 'a) -> 'a
      val foreach: t * (dec -> unit) -> unit
      val fromList: dec list -> t
      val fromVector: dec vector -> t
      val layout: t -> Layout.t
      val map: t * (dec -> dec) -> t
      val single: dec -> t
      val toList: t -> dec list
      val toVector: t -> dec vector
   end

