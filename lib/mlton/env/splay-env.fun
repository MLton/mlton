signature ORDER_ENV_STRUCTS =
   sig
      structure Domain: ORDER
      structure Range: T
   end


functor SplayMonoEnv(S:ORDER_ENV_STRUCTS): MONO_ENV =
BasicEnvToEnv
(open S

 structure E = SplayMapFn(type ord_key = Domain.t
			  val compare = Domain.compare)
    
 type t = Range.t E.map
 fun extend(env, d, r) = E.insert(env, d, r)
 fun fromList l = List.fold(l, E.empty, fn ((d, r), env) =>
			    extend(env, d, r))
 val toList = E.listItemsi
 val peek = E.find)
