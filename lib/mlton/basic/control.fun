(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Control(): CONTROL =
struct

val defaults: (unit -> unit) list ref = ref []

val settings: {name: string,
	       value: (unit -> string)} list ref = ref []

fun setDefaults() = List.foreach(!defaults, fn f => f())

fun control{name, default, toString} =
   let val r = ref default
   in List.push(settings, {name = name, 
			   value = fn () => toString(!r)})
      ; List.push(defaults, fn () => r := default)
      ; r
   end

fun all() =
   List.fold(!settings, [], fn ({name, value}, ac) =>
	     {name = name, value = value()} :: ac)

end
