(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(*
 * Invariant: Created globals only refer to other globals.
 *            Hence, the newly created globals may appear at the
 *            beginning of the program.
 *
 * Circular abstract values can arise as a result of programs like:
 *   datatype t = T of t
 *   fun f () = T (f ())
 *   val _ = f ()
 * There is special code in printing abstract values and in determining whether
 * they are global in order to avoid infinite loops.
 *)
functor ConstantPropagation (S: CONSTANT_PROPAGATION_STRUCTS) : CONSTANT_PROPAGATION = 
struct

open S

structure Once = Once (S)
structure Global = Global (S)

structure Type =
   struct
      open Type

      fun isSmall t =
	 case dest t of
	    Array _ => false
	  | Vector _ => false
	  | Datatype _ => false
	  | Ref t => isSmall t
	  | Tuple ts => Vector.forall (ts, isSmall)
	  | _ => true
   end

open Exp Transfer

structure Value =
   struct
      datatype global =
	 NotComputed
       | No
       | Yes of Var.t

      structure Const =
	 struct
	    datatype t = T of {const: const ref,
			       coercedTo: t list ref}
	    and const =
	       Undefined (* no possible value *)
	     | Const of Const.t
	     | Unknown (* many possible values *)

	    fun new c = T {const = ref c,
			   coercedTo = ref []}

	    fun equals (T {const = r, ...}, T {const = r', ...}) = r = r'

	    val const = new o Const

	    fun undefined () = new Undefined
	    fun unknown () = new Unknown

	    fun layout (T {const, ...}) =
	       let open Layout
	       in case !const of
		  Undefined => str "undefined constant"
		| Const c => Const.layout c
		| Unknown => str "unknown constant"
	       end
	    
	    fun makeUnknown (T {const, coercedTo}): unit =
	       case !const of
		  Unknown => ()
		| _ => (const := Unknown
			; List.foreach (!coercedTo, makeUnknown)
			; coercedTo := [])

	    fun send (c: t, c': const): unit =
	       let
		  fun loop (c as T {const, coercedTo}) =
		     case (c', !const) of
			(_, Unknown) => ()
		      | (_, Undefined) => (const := c'
					   ; List.foreach (!coercedTo, loop))
		      | (Const c', Const c'') =>
			   if Const.equals (c', c'')
			      then ()
			   else makeUnknown c
		      | _ => makeUnknown c
	       in loop c
	       end

	    fun coerce {from = from as T {const, coercedTo}, to: t}: unit =
	       if equals (from, to)
		  then ()
	       else
		  let fun push () = List.push (coercedTo, to)
		  in case !const of
		     Unknown => makeUnknown to
		   | Undefined => push ()
		   | c as Const _ => (push (); send (to, c))
		  end

	    fun unify (c, c') =
	       (coerce {from = c, to = c'}
		; coerce {from = c', to = c})
	 end

      structure One =
	 struct
	    datatype 'a t = T of {global: Var.t option ref,
				  extra: 'a}

	    fun new (a: 'a): 'a t = T {global = ref NONE,
				       extra = a}

	    fun global (T {global = g, ...}) = g
	       
	    fun equals (n, n') = global n = global n'
	 end
      
      structure Place =
	 struct
	    datatype 'a t =
	       Undefined
	     | One of 'a One.t
	     | Unknown

	    val toString =
	       fn Undefined => "Undefined"
		| One _ => "One"
		| Unknown => "Unknown"

	    fun layout b = Layout.str (toString b)
	 end

      structure Birth =
	 struct
	    datatype 'a t = T of {place: 'a Place.t ref,
				  coercedTo: 'a t list ref}

	    fun layout (T {place, ...}) = Place.layout (!place)

	    fun equals (T {place = r, ...}, T {place = r', ...}) = r = r'
	    fun new p = T {place = ref p,
			   coercedTo = ref []}

	    fun undefined (): 'a t = new Place.Undefined
	    fun unknown (): 'a t = new Place.Unknown
	    fun here (a: 'a): 'a t = new (Place.One (One.new a))

	    fun makeUnknown (T {place, coercedTo, ...}) =
	       case !place of
		  Place.Unknown => ()
		| _ => (place := Place.Unknown
			; List.foreach (!coercedTo, makeUnknown)
			; coercedTo := [])

	    fun send (b: 'a t, one): unit =
	       let
		  fun loop (b as T {place, coercedTo, ...}) =
		     case !place of
			Place.Undefined => (place := Place.One one
					    ; List.foreach (!coercedTo, loop))
		      | Place.One one' => if One.equals (one, one')
					     then ()
					  else makeUnknown b
		      | Place.Unknown => ()
	       in loop b
	       end

	    fun coerce {from = from as T {place, coercedTo, ...}, to} =
	       if equals (from, to)
		  then ()
	       else
		  let fun push () = List.push (coercedTo, to)
		  in case !place of
		     Place.Unknown => makeUnknown to
		   | Place.One one => (push (); send (to, one))
		   | Place.Undefined => push ()
		  end

	    fun unify (c, c') =
	       (coerce {from = c, to = c'}
		; coerce {from = c', to = c})
	 end

      structure Set = DisjointSet
      structure Unique = UniqueId ()
	 
      datatype t =
	 T of {
	       value: value,
	       ty: Type.t,
	       global: global ref
	       } Set.t
      and value =
	 Const of Const.t
       | Datatype of data
       | Vector of {length: t,
		    elt: t}
       | Ref of {birth: {init: t} Birth.t,
		 arg: t}
       | Array of {birth: unit Birth.t,
		   length: t,
		   elt: t}
       | Tuple of t vector
      and data =
	 Data of {
		  value: dataVal ref,
		  coercedTo: data list ref,
		  filters: {
			    con: Con.t,
			    args: t vector
			    } list ref
		  }
      and dataVal =
	 Undefined
       | ConApp of {con: Con.t,
		    args: t vector,
		    uniq: Unique.t}
       | Unknown

      local
	 fun make sel (T s) = sel (Set.value s)
      in
	 val value = make #value
	 val global = make #global
	 val ty = make #ty
      end

      fun equals (T s, T s') = Set.equals (s, s')

      local
	 open Layout
      in
	 fun layout v =
	    case value v of
	       Const c => Const.layout c
	     | Datatype d => layoutData d
	     | Ref {birth, arg, ...} =>
		  seq [str "ref", tuple [Birth.layout birth, layout arg]]
	     | Array {birth, length, elt, ...} =>
		  seq [str "array", tuple [Birth.layout birth,
					   layout length,
					   layout elt]]
	     | Vector {length, elt, ...} => seq [str "vector",
						 tuple [layout length,
							layout elt]]
	     | Tuple vs => Vector.layout layout vs
	 and layoutData (Data {value, ...}) =
	    case !value of
	       Undefined => str "undefined datatype"
	     | ConApp {con, args, uniq} =>
		  record [("con", Con.layout con),
			  ("uniq", Unique.layout uniq)]
	          (* Can't layout the args because there may be a circularity *)
	     | Unknown => str "unknown datatype"
      end

      val globalsInfo = Trace.info "globals"
      val globalInfo = Trace.info "global"
	 
      fun globals arg: (Var.t * Type.t) vector option =
	 Trace.traceInfo
	 (globalsInfo,
	  (Vector.layout layout) o #1,
	  Option.layout (Vector.layout (Layout.tuple2 (Var.layout, Type.layout))),
	  Trace.assertTrue)
	 (fn (vs: t vector, newGlobal) =>
	  DynamicWind.withEscape
	  (fn escape =>
	   SOME (Vector.map
		 (vs, fn v =>
		  case global (v, newGlobal) of
		     NONE => escape NONE
		   | SOME g => g)))) arg
      and global arg: (Var.t * Type.t) option =
	 Trace.traceInfo (globalInfo,
			  layout o #1,
			  Option.layout (Var.layout o #1),
			  Trace.assertTrue)
	 (fn (v as T s, newGlobal) =>
	  let val {global = r, ty, value} = Set.value s
	  in case !r of
	        No => NONE
	      | Yes g => SOME (g, ty)
	      | NotComputed =>
		   let
		      (* avoid globalizing circular abstract values *)
		      val _ = r := No
		      fun yes e = Yes (newGlobal (ty, e))
		      fun unary (Birth.T {place, ...},
				 makeInit: 'a -> t,
				 prim: Prim.t,
				 targ: Type.t) =
			 case !place of
			    Place.One (One.T {global = glob, extra}) =>
			       let
				  val init = makeInit extra
			       in
				  case global (init, newGlobal) of
				     SOME (x, _) =>
				        Yes (case !glob of
					        NONE => 
						   let
						      val exp =
							  Exp.PrimApp
							  {prim = prim,
							   targs = Vector.new1 targ,
							   args = Vector.new1 x}
						      val g = newGlobal (ty, exp)
						   in glob := SOME g; g
						   end
					      | SOME g => g)
				   | _ => No
			       end
			  | _ => No
		      val g =
			 case value of
			    Const (Const.T {const, ...}) =>
			       (case !const of
				   Const.Const c => yes (Exp.Const c)
				 | _ => No)
			  | Datatype (Data {value, ...}) =>
			       (case !value of
				   ConApp {con, args, ...} =>
				      (case globals (args, newGlobal) of
					  NONE => No
					| SOME args =>
					     yes (Exp.ConApp
						  {con = con,
						   args = Vector.map (args, #1)}))
				 | _ => No)
			  | Ref {birth, ...} =>
			       unary (birth, fn {init} => init,
				      Prim.reff, Type.deref ty)
			  | Array {birth, length, ...} =>
			       unary (birth, fn _ => length,
				      Prim.array, Type.dearray ty)
			  | Vector _ => No
			  | Tuple vs =>
			       (case globals (vs, newGlobal) of
				   NONE => No
				 | SOME xts =>
				      yes (Exp.Tuple (Vector.map (xts, #1))))
		   in r := g; global (v, newGlobal)
		   end
	  end) arg
	 
      fun new (v: value, ty: Type.t): t =
	 T (Set.singleton {value = v,
			   ty = ty,
			   global = ref NotComputed})


      fun tuple vs =
	 new (Tuple vs, Type.tuple (Vector.map (vs, ty)))

      fun const c = new (Const (Const.const c), Type.ofConst c)

      val zero = const (S.Const.fromInt 0)

      fun deconst v =
	 case value v of
	    Const c => c
	  | _ => Error.bug "deconst"
	       
      local
	 fun make (err, sel) v =
	    case value v of
	       Vector fs => sel fs
	     | _ => Error.bug err
      in val devector = make ("devector", #elt)
	 val vectorLength = make ("vectorLength", #length)
      end

      local
	 fun make (err, sel) v =
	    case value v of
	       Array fs => sel fs
	     | _ => Error.bug err
      in val dearray = make ("dearray", #elt)
	 val arrayLength = make ("arrayLength", #length)
	 val arrayBirth = make ("arrayBirth", #birth)
      end

      fun vectorFromArray (T s: t): t =
	 let
	    val {value, ty, ...} = Set.value s
	 in case value of
	    Array {elt, length, ...} =>
	       new (Vector {elt = elt, length = length}, ty)
	  | _ => Error.bug "Value.vectorFromArray"
	 end

      local
	 fun make (err, sel) v =
	    case value v of
	       Ref fs => sel fs
	     | _ => Error.bug err
      in val deref = make ("deref", #arg)
	 val refBirth = make ("refBirth", #birth)
      end

      structure Data =
	 struct
	    datatype t = datatype data

	    val layout = layoutData

	    local
	       fun make v () = Data {value = ref v,
				     coercedTo = ref [],
				     filters = ref []}
	    in val undefined = make Undefined
	       val unknown = make Unknown
	    end
	 end
      
      local
	 (* The extra birth is because of let-style polymorphism.
	  * arrayBirth is really the same as refBirth.
	  *)
	 fun make (const, data, refBirth, arrayBirth) =
	    let
	       fun loop (t: Type.t): t =
		  new
		  (case Type.dest t of
		      Type.Datatype _ => Datatype (data ())
		    | Type.Ref t => Ref {birth = refBirth (),
					 arg = loop t}
		    | Type.Array t => Array {birth = arrayBirth (),
					     length = loop Type.int,
					     elt = loop t}
		    | Type.Vector t => Vector {length = loop Type.int,
					       elt = loop t}
		    | Type.Tuple ts => Tuple (Vector.map (ts, loop))
		    | _ => Const (const ()),
			 t)
	    in loop
	    end
      in
	 val fromType =
	    make (Const.undefined,
		  Data.undefined,
		  Birth.undefined,
		  Birth.undefined)
	 val unknown =
	    make (Const.unknown,
		  Data.unknown,
		  Birth.unknown,
		  Birth.unknown)
      end

      fun select {tuple, offset, resultType} =
	 case value tuple of
	    Tuple vs => Vector.sub (vs, offset)
	  | _ => Error.bug "select of non-tuple" 

      fun unit () = tuple (Vector.new0 ())
   end

val traceSendConApp =
   Trace.trace2
   ("sendConApp", Value.Data.layout,
    fn {con, args, uniq} =>
    Layout.record [("con", Con.layout con),
		   ("args", Vector.layout Value.layout args),
		   ("uniq", Value.Unique.layout uniq)],
    Unit.layout)

val traceSendConAppLoop =
   Trace.trace ("sendConAppLoop", Value.Data.layout, Unit.layout)

val traceMakeDataUnknown =
   Trace.trace ("makeDataUnknown", Value.Data.layout, Unit.layout)

(* ------------------------------------------------- *)
(*                     simplify                      *)
(* ------------------------------------------------- *)

fun simplify (program as Program.T {datatypes, globals, functions, main})
   : Program.t =
   let
      val once = Once.once program
      val {get = conInfo: Con.t -> {result: Type.t,
				    types: Type.t vector,
				    values: Value.t vector},
	   set = setConInfo, ...} =
	 Property.getSetOnce
	 (Con.plist, Property.initRaise ("conInfo", Con.layout))
      val conValues = #values o conInfo
      val conResult = #result o conInfo
      val _ =
	 Vector.foreach
	 (datatypes, fn Datatype.T {tycon, cons} =>
	  let
	     val result = Type.con (tycon, Vector.new0 ())
	  in
	     Vector.foreach
	     (cons, fn {con, args} =>
	      setConInfo (con,
			  {result = result,
			   types = args,
			   values = Vector.map (args, Value.fromType)}))
	  end)
	 
      local open Value
      in
 	 val traceCoerce =
 	    Trace.trace ("Value.coerce",
			 fn {from, to} => Layout.record [("from", layout from),
							 ("to", layout to)],
			 Unit.layout)
	    
	 fun makeDataUnknown arg: unit =
	    traceMakeDataUnknown
	    (fn Data {value, coercedTo, filters, ...} =>
	     let
		fun doit () =
		   (value := Unknown
		    ; List.foreach (!coercedTo, makeDataUnknown)
		    ; coercedTo := []
		    ; (List.foreach
		       (!filters, fn {con, args} =>
			coerces {froms = conValues con,
				 tos = args})))
	     in case !value of
		Unknown => ()
	      | Undefined => doit ()
	      | ConApp _ => doit ()
	     end) arg

	 and sendConApp arg: unit =
	    traceSendConApp
	    (fn (d: data, ca as {con, args, uniq}) =>
	     let
		val v = ConApp ca
		fun loop arg: unit =
		   traceSendConAppLoop
		   (fn Data {value, coercedTo, filters, ...} =>
		    case !value of
		       Unknown => ()
		     | Undefined =>
			  (value := v
			   ; List.foreach (!coercedTo, loop)
			   ; (List.foreach
			      (!filters, fn {con = con', args = args'} =>
			       if Con.equals (con, con')
				  then coerces {froms = args, tos = args'}
			       else ())))
		     | ConApp {con = con', uniq = uniq', ...} =>
			  if Unique.equals (uniq, uniq')
			     orelse (Con.equals (con, con')
				     andalso 0 = Vector.length args)
			     then ()
			  else makeDataUnknown d) arg
	     in loop d
	     end) arg
	 and coerces {froms: Value.t vector, tos: Value.t vector} =
	    Vector.foreach2 (froms, tos, fn (from, to) =>
			    coerce {from = from, to = to})
	 and coerce arg =
	    traceCoerce
	    (fn {from, to} =>
	     if equals (from, to)
		then ()
	     else
		case (value from, value to) of
		   (Const from, Const to) => Const.coerce {from = from, to = to}
		 | (Datatype from, Datatype to) =>
		      coerceData {from = from, to = to}
		 | (Ref {birth, arg}, Ref {birth = b', arg = a'}) =>
		      (Birth.coerce {from = birth, to = b'}
		       ; unify (arg, a'))
	         | (Array {birth = b, length = n, elt = x},
		    Array {birth = b', length = n', elt = x'}) =>
		      (Birth.coerce {from = b, to = b'}
		       ; coerce {from = n, to = n'}
		       ; unify (x, x'))
	         | (Vector {length = n, elt = x},
		    Vector {length = n', elt = x'}) =>
		      (coerce {from = n, to = n'}
		       ; coerce {from = x, to = x'})
		 | (Tuple vs, Tuple vs') => coerces {froms = vs, tos = vs'}
		 | _ => Error.bug "strange coerce") arg

	 and unify (T s: t, T s': t): unit =
	    if Set.equals (s, s')
	       then ()
	    else
	       let 
		  val {value, ...} = Set.value s
		  val {value = value', ...} = Set.value s'
	       in Set.union (s, s')
		  ; case (value, value') of
		       (Const c, Const c') => Const.unify (c, c')
		     | (Datatype d, Datatype d') => unifyData (d, d')
		     | (Ref {birth, arg}, Ref {birth = b', arg = a'}) =>
			  (Birth.unify (birth, b')
			   ; unify (arg, a'))
		     | (Array {birth = b, length = n, elt = x},
		        Array {birth = b', length = n', elt = x'}) =>
		          (Birth.unify (b, b')
			   ; unify (n, n')
			   ; unify (x, x'))
		     | (Vector {length = n, elt = x},
		        Vector {length = n', elt = x'}) =>
		          (unify (n, n')
			   ; unify (x, x'))
		     | (Tuple vs, Tuple vs') => Vector.foreach2 (vs, vs', unify)
		     | _ => Error.bug "strange unify"
	       end
	 and unifyData (d, d') =
	    (coerceData {from = d, to = d'}
	     ; coerceData {from = d', to = d})
	 and coerceData {from = Data {value, coercedTo, ...}, to} =
	    case !value of
	       Undefined => List.push (coercedTo, to)
	     | ConApp ca => (List.push (coercedTo, to)
			     ; sendConApp (to, ca))
	     | Unknown => makeDataUnknown to
	 fun conApp {con: Con.t, args: t vector}: t =
	    let
	       val {values = tos, result, ...} = conInfo con
	    in
	       coerces {froms = args, tos = tos}
	       ; new (Datatype
		      (Data {value = ref (ConApp {con = con, args = args,
						  uniq = Unique.new ()}),
			     coercedTo = ref [],
			     filters = ref []}),
		      result)
	    end
	 fun makeUnknown (v: t): unit =
	    case value v of
	       Const c => Const.makeUnknown c
	     | Datatype d => makeDataUnknown d
	     | Ref {arg, ...} => makeUnknown arg
	     | Array {length, elt, ...} => (makeUnknown length
					    ; makeUnknown elt)
	     | Vector {length, elt} => (makeUnknown length
					; makeUnknown elt)
	     | Tuple vs => Vector.foreach (vs, makeUnknown)
	 fun sideEffect (v: t): unit =
	    case value v of
	       Const _ => ()
	     | Datatype _ => ()
	     | Ref {arg, ...} => makeUnknown arg
	     | Array {elt, ...} => makeUnknown elt
	     | Vector {elt, ...} => makeUnknown elt
	     | Tuple vs => Vector.foreach (vs, sideEffect)
	 fun primApp {prim,
		      targs,
		      args: Value.t vector,
		      resultVar,
		      resultType}: t =
	    let
	       fun bear z =
		  case resultVar of
		     SOME resultVar => if once resultVar 
		                          andalso 
					  Type.isSmall resultType
					  then Birth.here z
				       else Birth.unknown ()
		   | _ => Error.bug "bear"
	       fun update (a, v) =
		  (coerce {from = v, to = dearray a}
		   ; unit ())
	       fun arg i = Vector.sub (args, i)
	       datatype z = datatype Prim.Name.t
	    in case Prim.name prim of
	       Array_array =>
		  let val a = fromType resultType
		  in coerce {from = arg 0, to = arrayLength a}
		     ; Birth.coerce {from = bear (), to = arrayBirth a}
		     ; a
		  end
	     | Array_array0 =>
		  let
		     val a = fromType resultType
		     val _ = coerce {from = zero, to = arrayLength a}
		     val _ = Birth.coerce {from = Birth.here (),
					   to = arrayBirth a}
		  in a
		  end
	     | Array_length => arrayLength (arg 0)
	     | Array_sub => dearray (arg 0)
	     | Array_update => update (arg 0, arg 2)
	     | Ref_assign =>
		  (coerce {from = arg 1, to = deref (arg 0)}; unit ())
	     | Ref_deref => deref (arg 0)
	     | Ref_ref =>
		  let
		     val v = arg 0
		     val r = fromType resultType
		  in coerce {from = v, to = deref r}
		     ; Birth.coerce {from = bear {init = v}, to = refBirth r}
		     ; r
		  end
	     | Vector_fromArray => vectorFromArray (arg 0)
	     | Vector_length => vectorLength (arg 0)
	     | Vector_sub => devector (arg 0)
	     | _ => (if Prim.maySideEffect prim
			then Vector.foreach (args, sideEffect)
		     else ()
			; unknown resultType)
	    end
	 fun filter (variant, con, args) =
	    case value variant of
	       Datatype (Data {value, filters, ...}) =>
		  let
		     fun save () = List.push (filters, {con = con, args = args})
		  in case !value of
		     Undefined => save ()
		   | Unknown => coerces {froms = conValues con, tos = args}
		   | ConApp {con = con', args = args', ...} =>
			((* The save () has to happen before the coerces because
			  * they may loop back and change the variant, which
			  * would need to then change this value.
			  *)
			 save ()
			 ; if Con.equals (con, con')
			      then coerces {froms = args', tos = args}
			   else ())
		  end
	     | _ => Error.bug "conSelect of non-datatype"
      end
      fun filterIgnore _ = ()
      val {value, ...} =
	 Control.trace (Control.Pass, "fixed point")
	 analyze {
		  coerce = coerce,
		  conApp = conApp,
		  const = Value.const,
		  copy = Value.fromType o Value.ty,
		  filter = filter,
		  filterChar = filterIgnore,
		  filterInt = filterIgnore,
		  filterWord = filterIgnore,
		  filterWord8 = filterIgnore,
		  fromType = Value.fromType,
		  layout = Value.layout,
		  primApp = primApp,
		  program = program,
		  select = Value.select,
		  tuple = Value.tuple,
		  useFromTypeOnBinds = false
		  }
      val _ =
	 Control.diagnostics
	 (fn display =>
	  let open Layout
	  in 
	     display (str "\n\nConstructors:")
	     ; (Vector.foreach
		(datatypes, fn Datatype.T {tycon, cons} =>
		 (display (seq [Tycon.layout tycon, str ": "])
		  ; Vector.foreach
		    (cons, fn {con, ...} =>
		     display
		     (seq [Con.layout con, str ": ",
			   Vector.layout Value.layout (conValues con)])))))
	     ; display (str "\n\nConstants:")
	     ; (Program.foreachVar
		(program, fn (x, _) => display (seq [Var.layout x,
						     str " ",
						     Value.layout (value x)])))
	  end)
      (* Walk through the program
       *  - removing declarations whose rhs is constant
       *  - replacing variables whose value is constant with globals
       *  - building up the global decs
       *)
      val {new = newGlobal, all = allGlobals} = Global.make ()
      fun replaceVar x =
	 case Value.global (value x, newGlobal) of
	    NONE => x
	  | SOME (g, _) => g
      fun replaceVars xs = Vector.map (xs, replaceVar)

      fun doitStatement (Statement.T {var, ty, exp}) =
	 let
	    fun keep () =
	       SOME (Statement.T {var = var,
				  ty = ty,
				  exp = Exp.replaceVar (exp, replaceVar)})
	 in
	    case var of
	       NONE => keep ()
	     | SOME var => 
		  (case (Value.global (value var, newGlobal), exp) of
		      (NONE, _) => keep ()
		    | (SOME _, PrimApp {prim, ...}) =>
			 if Prim.maySideEffect prim
			    then keep ()
			 else NONE
		    | _ => NONE)
	 end
      fun doitTransfer transfer =
	 Transfer.replaceVar (transfer, replaceVar)
      fun doitBlock (Block.T {label, args, statements, transfer}) =
	 Block.T {label = label,
		  args = args,
		  statements = Vector.keepAllMap (statements, doitStatement),
		  transfer = doitTransfer transfer}
      fun doitFunction f =
	 let val {name, args, start, blocks, returns, mayRaise} = Function.dest f
	 in
	    Function.new {name = name,
			  args = args,
			  start = start,
			  blocks = Vector.map (blocks, doitBlock),
			  returns = returns,
			  mayRaise = mayRaise}
	 end
      val functions = List.revMap (functions, doitFunction)
      val globals = Vector.keepAllMap (globals, doitStatement)
      val globals = Vector.concat [allGlobals (), globals]
      val shrink = shrinkFunction globals
      val program = Program.T {datatypes = datatypes,
			       globals = globals,
			       functions = List.revMap (functions, shrink),
			       main = main}
      val _ = Program.clearTop program
   in
      program
   end

end
