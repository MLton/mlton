(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor TypeEnv (S: TYPE_ENV_STRUCTS): TYPE_ENV =
struct

open S

structure InferType = InferType (S)
   
structure InferScheme = InferScheme (structure Type = InferType)
   
structure Frees = InferType.Frees

structure VarRange =
   struct
      datatype kind =
	 Normal
       | Delayed
       | Recursive of Tyvar.t vector ref
       | Overload of (Var.t * InferType.t) vector

      datatype t = T of {scheme: InferScheme.t,
			 kind: kind}

      fun scheme (T {scheme, ...}) = scheme

      fun layout (T {scheme, ...}) = InferScheme.layout scheme
   end

structure Opens (* :
		 sig
		    type t
val empty: t
val maybeAdd: t * InferScheme.t -> t
val append: t * t -> t
val frees: t -> Frees.t
val layout: t -> Layout.t
end*) =
   struct
      datatype list =
	 Empty
       | Cons of InferScheme.t * t
      withtype t = list ref

      fun toList opens =
	 case !opens of
	    Empty => []
	  | Cons (s, opens) => s :: (toList opens)

      local open Layout
      in fun layout opens = list (List.map (toList opens, InferScheme.layout))
      end

      fun append (opens, opens') =
	 let
	    fun loop opens =
	       case !opens of
		  Empty => opens'
		| Cons (s, opens) => ref (Cons (s, loop opens))
	 in loop opens 
	 end

      val empty = ref Empty

      fun maybeAdd (opens, s) =
	 if Frees.isEmpty (InferScheme.frees s)
	    then opens
	 else ref (Cons (s, opens))
	    
      (*       val maybeAdd =
       * 	 Trace.trace2 ("maybeAdd", layout, InferScheme.layout, layout)
       * 	 maybeAdd
       *)

      fun frees opens =
	 let
	    fun loop (r, r', fs) =
	       case !r' of
		  Empty => (r := Empty ; fs)
		| pair as Cons (s, r') =>
		     let val fs' = InferScheme.frees s
		     in if Frees.isEmpty fs'
			   then loop (r, r', fs)
			else (r := pair ; loop (r', r', Frees.+ (fs', fs)))
		     end
	 in loop (opens, opens, Frees.empty)
	 end
   end

datatype t = T of {opens: Opens.t}

fun layout (T {opens}) =
   let open Layout
   in record [("opens", Opens.layout opens)]
   end	     

val empty = T {opens = Opens.empty}

(* This property is destroyed after type inference is done by the call to
 * Xml.Program.clear in infer.fun.
 * The property can not be getSetOnce, because it is set twice for fun
 * declarations.
 *)
val {get = getVarRange: Var.t -> VarRange.t, set = setVarRange} =
   Property.getSet (Var.plist, Property.initRaise ("range", Var.layout))

val setVarRange =
   Trace.trace2 ("setVarRange", Var.layout, VarRange.layout, Unit.layout)
   setVarRange
   
fun extendVarRange (T {opens}, x, r) =
   (setVarRange (x, r)
    ; T {opens = Opens.maybeAdd (opens, VarRange.scheme r)})
fun lookupVarRange (_, x) = getVarRange x
fun lookupVar (e, x) = VarRange.scheme (lookupVarRange (e, x))
fun extendVar (e, x, s) =
   extendVarRange (e, x, VarRange.T {scheme = s,
				     kind = VarRange.Normal})
fun frees (T {opens, ...}) = Opens.frees opens

(*val frees = Trace.trace ("frees", layout, Frees.layout) frees *)
   
end
