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
