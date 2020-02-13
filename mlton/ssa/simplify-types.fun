(* Copyright (C) 2009,2018,2020 Matthew Fluet.
 * Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* This pass must happen before polymorphic equality is implemented because
 * 1. it will make polymorphic equality faster because some types are simpler
 * 2. it removes uses of polymorphic equality that must return true
 *
 * This pass computes a "cardinality" of each datatype, which is an
 * abstraction of the number of values of the datatype.
 *   Zero means the datatype has no values (except for bottom).
 *   One means the datatype has one value (except for bottom).
 *   Many means the datatype has many values.
 *
 * This pass removes all datatypes whose cardinality is Zero or One
 * and removes
 *   components of tuples
 *   function args
 *   constructor args
 * which are such datatypes.
 *
 * This pass marks constructors as one of
 *   Useless: it never appears in a ConApp.
 *   Transparent: it is the only variant in its datatype
 *     and its argument type does not contain any uses of
 *     Tycon.array or Tycon.vector.
 *   Useful: otherwise
 * This pass also removes Useless and Transparent constructors.
 *
 * We must keep track of Transparent constructors whose argument type
 * uses Tycon.array because of datatypes like the following:
 *   datatype t = T of t array
 * Such a datatype has Cardinality.Many, but we cannot eliminate
 * the datatype and replace the lhs by the rhs, i.e. we must keep the
 * circularity around.
 * Must do similar things for vectors.
 *
 * Also, to eliminate as many Transparent constructors as possible, for
 * something like the following,
 *   datatype t = T of u array
 *   and u = U of t vector
 * we (arbitrarily) expand one of the datatypes first.
 * The result will be something like
 *   datatype u = U of u array array
 * where all uses of t are replaced by u array.
 *)

functor SimplifyTypes (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

open S
open Exp Transfer
structure Cardinality =
   struct
      structure L = ThreePointLattice(val bottom = "zero"
                                      val mid = "one"
                                      val top = "many")
      open L

      val isZero = isBottom
      val newZero = new
      val isOne = isMid
      val makeOne = makeMid
      val whenOne = whenMid
      val makeMany = makeTop
      val whenMany = whenTop

      local
         fun mkNew (make: t -> unit) () =
            let val c = newZero ()
            in make c; c end
      in
         val newOne = mkNew makeOne
         val newMany = mkNew makeMany
      end

      val one = newOne ()
      val many = newMany ()

      structure Card =
         struct
            datatype t = Zero | One | Many
            fun toCard c =
               if isZero c then Zero
               else if isOne c then One
               else Many
            fun sum esc (c1, c2) =
               case (toCard c1, c2) of
                  (Zero, c2) => c2
                | (c1, Zero) => c1
                | _ => esc Many
            fun prod esc (c1, c2) =
               case (toCard c1, c2) of
                  (Zero, _) => esc Zero
                | (_, Zero) => esc Zero
                | (One, One) => One
                | _ => Many
         end

      local
         fun make (f, id) cs =
            let
               val c' = newZero ()
               fun doit () =
                  case (Exn.withEscape
                        (fn escape =>
                         Vector.fold (cs, id, f escape))) of
                     Card.Zero => ()
                   | Card.One => makeOne c'
                   | Card.Many => makeMany c'
               val () =
                  Vector.foreach
                  (cs, fn c =>
                   (whenOne (c, doit); whenMany (c, doit)))
               val () = doit ()
            in
               c'
            end
      in
         val sum = make (Card.sum, Card.Zero)
         val prod = make (Card.prod, Card.One)
      end
   end

structure ConRep =
   struct
      datatype t =
         Useless
       | Transparent
       | Useful

      val isUseful =
         fn Useful => true
          | _ => false

      val isUseless =
         fn Useless => true
          | _ => false

      val toString =
         fn Useless => "useless"
          | Transparent => "transparent"
          | Useful => "useful"

      val layout = Layout.str o toString
   end

structure Result =
   struct
      datatype 'a t =
         Bugg
       | Delete
       | Keep of 'a

      fun layout layoutX =
         let
            open Layout
         in
            fn Bugg => str "Bug"
             | Delete => str "Delete"
             | Keep x => seq [str "Keep ", layoutX x]
         end
   end

fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = conInfo: Con.t -> {args: Type.t vector,
                                    cardinality: Cardinality.t,
                                    rep: ConRep.t ref},
           set = setConInfo, ...} =
         Property.getSetOnce
         (Con.plist, Property.initRaise ("SimplifyTypes.conInfo", Con.layout))
      val conInfo =
         Trace.trace ("SimplifyTypes.conInfo",
                      Con.layout,
                      fn {args, cardinality, rep} =>
                      Layout.record [("args", Vector.layout Type.layout args),
                                     ("cardinality", Cardinality.layout cardinality),
                                     ("rep", ConRep.layout (!rep))])
         conInfo
      local
         fun make sel = sel o conInfo
         fun make' sel = let val get = make sel
                         in (! o get, fn (c, x) => get c := x)
                         end
      in
         val conArgs = make #args
         val conCardinality = make #cardinality
         val (conRep, setConRep) = make' #rep
      end
      val setConRep =
         Trace.trace2
         ("SimplifyTypes.setConRep", Con.layout, ConRep.layout, Unit.layout)
         setConRep
      val conIsUseful = ConRep.isUseful o conRep
      val conIsUseful =
         Trace.trace
         ("SimplifyTypes.conIsUseful", Con.layout, Bool.layout)
         conIsUseful
      val {get = tyconInfo: Tycon.t -> {cardinality: Cardinality.t,
                                        numCons: int ref,
                                        replacement: Type.t option ref},
           set = setTyconInfo, ...} =
         Property.getSetOnce
         (Tycon.plist, Property.initRaise ("SimplifyTypes.tyconInfo", Tycon.layout))
      local
         fun make sel = sel o tyconInfo
         fun make' sel = let val get = make sel
                         in (! o get, fn (t, x) => get t := x)
                         end
      in
         val tyconCardinality = make #cardinality
         val (tyconNumCons, setTyconNumCons) = make' #numCons
         val (tyconReplacement, setTyconReplacement) = make' #replacement
      end
      (* Initialize conInfo and typeInfo *)
      val _ =
         Vector.foreach
         (datatypes, fn Datatype.T {tycon, cons} =>
          (setTyconInfo (tycon, {cardinality = Cardinality.newZero (),
                                 numCons = ref 0,
                                 replacement = ref NONE});
           Vector.foreach
           (cons, fn {con, args} =>
            setConInfo (con, {args = args,
                              cardinality = Cardinality.newZero (),
                              rep = ref ConRep.Useless}))))
      (* Tentatively mark all constructors appearing in a ConApp as Useful
       * (some may later be marked as Useless or Transparent).
       * Mark any tycons created by MLton_bogus as cardinality Many.
       *)
      val _ =
         let
            fun setConRepUseful c = setConRep (c, ConRep.Useful)
            fun handleStatement (Statement.T {exp, ...}) =
               case exp of
                  ConApp {con, ...} => setConRepUseful con
                | PrimApp {prim, targs, ...} =>
                     (case Prim.name prim of
                         Prim.Name.MLton_bogus =>
                            (case Type.dest (Vector.sub (targs, 0)) of
                                Type.Datatype tycon =>
                                   Cardinality.makeMany (tyconCardinality tycon)
                              | _ => ())
                       | _ => ())
                | _ => ()
            (* Booleans are special because they are generated by primitives. *)
            val _ = setConRepUseful Con.truee
            val _ = setConRepUseful Con.falsee
            val _ = Vector.foreach (globals, handleStatement)
            val _ = List.foreach
                    (functions, fn f =>
                     Vector.foreach
                     (Function.blocks f, fn Block.T {statements, ...} =>
                      Vector.foreach (statements, handleStatement)))
         in ()
         end
      (* Compute the type cardinalities with a fixed point
       * over the Cardinality lattice.
       *)
      val {get = typeCardinality, destroy = destroyTypeCardinality} =
         Property.destGet
         (Type.plist,
          Property.initRec
          (fn (t, typeCardinality) =>
           let
              fun ptrCard t =
                 (Cardinality.prod o Vector.new2)
                 (typeCardinality t, Cardinality.many)
              fun tupleCard ts =
                 (Cardinality.prod o Vector.map)
                 (ts, typeCardinality)
              fun vecCard t =
                 (Cardinality.sum o Vector.new2)
                 (Cardinality.one,
                  (Cardinality.prod o Vector.new2)
                  (typeCardinality t, Cardinality.many))
              datatype z = datatype Type.dest
           in
              case Type.dest t of
                 Array _ => Cardinality.many
               | CPointer => Cardinality.many
               | Datatype tycon => tyconCardinality tycon
               | IntInf => Cardinality.many
               | Real _ => Cardinality.many
               | Ref t => ptrCard t
               | Thread => Cardinality.many
               | Tuple ts => tupleCard ts
               | Vector t => vecCard t
               | Weak t => ptrCard t
               | Word _ => Cardinality.many
           end))
      (* Remove useless constructors from datatypes.
       * Remove datatypes which have no cons.
       * Lower-bound cardinality of cons by product of arguments.
       * Lower-bound cardinality of tycons by sum of cons.
       *)
      val origDatatypes = datatypes
      val datatypes =
         Vector.keepAllMap
         (datatypes, fn Datatype.T {tycon, cons} =>
          let
             val cons = Vector.keepAll (cons, conIsUseful o #con)
             val _ =
                Cardinality.<=
                (Cardinality.sum
                 (Vector.map (cons, conCardinality o #con)),
                 tyconCardinality tycon)
             val _ =
                Vector.foreach
                (cons, fn {con, args} =>
                 Cardinality.<=
                 (Cardinality.prod
                  (Vector.map (args, typeCardinality)),
                  conCardinality con))
          in
             if Vector.isEmpty cons
                then (setTyconReplacement (tycon, SOME Type.unit)
                      ; NONE)
             else SOME (Datatype.T {tycon = tycon, cons = cons})
          end)
      (* diagnostic *)
      val _ =
         Control.diagnostics
         (fn display =>
          let
             open Layout
          in
             Vector.foreach
             (origDatatypes, fn Datatype.T {tycon, cons} =>
              (display (seq [str "cardinality of ",
                             Tycon.layout tycon,
                             str " = ",
                             Cardinality.layout (tyconCardinality tycon)]);
               Vector.foreach
               (cons, fn {con, ...} =>
                (display (seq [str "cardinality of ",
                               Con.layout con,
                               str " = ",
                               Cardinality.layout (conCardinality con)])))))
          end)
      fun transparent (tycon, con, args) =
         (setTyconReplacement (tycon, SOME (Type.tuple args))
          ; setConRep (con, ConRep.Transparent)
          ; setTyconNumCons (tycon, 1))
      (* "unary" is datatypes with one constructor whose rhs contains an
       * array (or vector) type.
       * For datatypes with one variant not containing an array type, eliminate
       * the datatype.
       *)
      fun containsArrayOrVector (ty: Type.t): bool =
         let
            datatype z = datatype Type.dest
            fun loop t =
               case Type.dest t of
                  Array _ => true
                | Ref t => loop t
                | Tuple ts => Vector.exists (ts, loop)
                | Vector _ => true
                | Weak t => loop t
                | _ => false
         in loop ty
         end
      val (datatypes, unary) =
         Vector.fold
         (datatypes, ([], []), fn (Datatype.T {tycon, cons}, (datatypes, unary)) =>
          let
             (* remove all cons with zero cardinality and mark them as useless *)
             val cons =
                Vector.keepAllMap
                (cons, fn c as {con, ...} =>
                 if Cardinality.isZero (conCardinality con)
                    then (setConRep (con, ConRep.Useless)
                          ; NONE)
                    else SOME c)
          in
             case Vector.length cons of
                0 => (setTyconNumCons (tycon, 0)
                      ; setTyconReplacement (tycon, SOME Type.unit)
                      ; (datatypes, unary))
              | 1 =>
                   let
                      val {con, args} = Vector.first cons
                   in
                      if Vector.exists (args, containsArrayOrVector)
                         then (datatypes,
                               {tycon = tycon, con = con, args = args} :: unary)
                         else (transparent (tycon, con, args)
                               ; (datatypes, unary))
                   end
              | _ => (Datatype.T {tycon = tycon, cons = cons} :: datatypes,
                      unary)
          end)
      fun containsTycon (ty: Type.t, tyc: Tycon.t): bool =
         let
            datatype z = datatype Type.dest
            val {get = containsTycon, destroy = destroyContainsTycon} =
               Property.destGet
               (Type.plist,
                Property.initRec
                (fn (t, containsTycon) =>
                 case Type.dest t of
                    Array t => containsTycon t
                  | Datatype tyc' =>
                       (case tyconReplacement tyc' of
                           NONE => Tycon.equals (tyc, tyc')
                         | SOME t => containsTycon t)
                  | Tuple ts => Vector.exists (ts, containsTycon)
                  | Ref t => containsTycon t
                  | Vector t => containsTycon t
                  | Weak t => containsTycon t
                  | _ => false))
            val res = containsTycon ty
            val () = destroyContainsTycon ()
         in res
         end
      (* Keep the circular transparent tycons, ditch the rest. *)
      val datatypes =
         List.fold
         (unary, datatypes, fn ({tycon, con, args}, accum) =>
          if Vector.exists (args, fn arg => containsTycon (arg, tycon))
             then Datatype.T {tycon = tycon,
                              cons = Vector.new1 {con = con, args = args}}
                  :: accum
          else (transparent (tycon, con, args)
                ; accum))
      fun makeKeepSimplifyTypes simplifyType ts =
         Vector.keepAllMap (ts, fn t =>
                            let
                               val t = simplifyType t
                            in
                               if Type.isUnit t
                                  then NONE
                                  else SOME t
                            end)
      val {get = simplifyType, destroy = destroySimplifyType} =
         Property.destGet
         (Type.plist,
          Property.initRec
          (fn (t, simplifyType) =>
           let
              val keepSimplifyTypes = makeKeepSimplifyTypes simplifyType
              open Type
           in
              case dest t of
                 Array t => array (simplifyType t)
               | Datatype tycon =>
                    (case tyconReplacement tycon of
                        SOME t =>
                           let
                              val t = simplifyType t
                              val _ = setTyconReplacement (tycon, SOME t)
                           in
                              t
                           end
                      | NONE => t)
               | Ref t => reff (simplifyType t)
               | Tuple ts => Type.tuple (keepSimplifyTypes ts)
               | Vector t => vector (simplifyType t)
               | Weak t => weak (simplifyType t)
               | _ => t
           end))
      val simplifyType =
         Trace.trace ("SimplifyTypes.simplifyType", Type.layout, Type.layout)
         simplifyType
      fun simplifyTypes ts = Vector.map (ts, simplifyType)
      val keepSimplifyTypes = makeKeepSimplifyTypes simplifyType
      (* Simplify constructor argument types. *)
      val datatypes =
         Vector.fromListMap
         (datatypes, fn Datatype.T {tycon, cons} =>
          (setTyconNumCons (tycon, Vector.length cons)
           ; Datatype.T {tycon = tycon,
                         cons = Vector.map (cons, fn {con, args} =>
                                            {con = con,
                                             args = keepSimplifyTypes args})}))
      val unitVar = Var.newNoname ()
      val {get = varInfo: Var.t -> Type.t, set = setVarInfo, ...} =
         Property.getSetOnce
         (Var.plist, Property.initRaise ("varInfo", Var.layout))
      fun simplifyVarType (x: Var.t, t: Type.t): Type.t =
         (setVarInfo (x, t)
          ; simplifyType t)
      fun simplifyMaybeVarType (x: Var.t option, t: Type.t): Type.t =
         case x of
            SOME x => simplifyVarType (x, t)
          | NONE => simplifyType t
      val oldVarType = varInfo
      val newVarType = simplifyType o oldVarType
      fun simplifyVar (x: Var.t): Var.t =
         if Type.isUnit (newVarType x)
            then unitVar
            else x
      val varIsUseless = Type.isUnit o newVarType
      fun removeUselessVars xs = Vector.keepAll (xs, not o varIsUseless)
      fun tuple xs =
         let
            val xs = removeUselessVars xs
         in
            if 1 = Vector.length xs
               then Var (Vector.first xs)
               else Tuple xs
         end
      fun simplifyFormals xts =
         Vector.keepAllMap
         (xts, fn (x, t) =>
          let
             val t = simplifyVarType (x, t)
          in
             if Type.isUnit t
                then NONE
                else SOME (x, t)
          end)
      val typeIsUseful = not o Type.isUnit o simplifyType
      datatype result = datatype Result.t
      fun simplifyExp (e: Exp.t): Exp.t result =
         case e of
            ConApp {con, args} =>
               (case conRep con of
                   ConRep.Transparent => Keep (tuple args)
                 | ConRep.Useful =>
                      Keep (ConApp {con = con,
                                    args = removeUselessVars args})
                 | ConRep.Useless => Bugg)
          | PrimApp {prim, targs, args} =>
               Keep
               (let
                   fun normal () =
                      PrimApp {prim = prim,
                               targs = simplifyTypes targs,
                               args = Vector.map (args, simplifyVar)}
                   fun equal () =
                      if 2 = Vector.length args
                         then if varIsUseless (Vector.first args)
                                 then ConApp {con = Con.truee,
                                              args = Vector.new0 ()}
                                 else normal ()
                         else Error.bug "SimplifyTypes.simplifyExp: strange eq/equal PrimApp"
                   open Prim.Name
                in
                   case Prim.name prim of
                      MLton_eq => equal ()
                    | MLton_equal => equal ()
                    | _ => normal ()
                end)
          | Select {tuple, offset} =>
               let
                  val ts = Type.deTuple (oldVarType tuple)
               in
                  Vector.fold'
                  (ts, 0, (offset, 0), fn (pos, t, (n, offset)) =>
                   if n = 0
                      then (Vector.Done
                            (Keep
                             (if offset = 0
                                 andalso not (Vector.existsR
                                              (ts, pos + 1, Vector.length ts,
                                               typeIsUseful))
                                 then Var tuple
                                 else Select {tuple = tuple,
                                              offset = offset})))
                      else Vector.Continue (n - 1,
                                            if typeIsUseful t
                                               then offset + 1
                                               else offset),
                   fn _ => Error.bug "SimplifyTypes.simplifyExp: Select:newOffset")
               end
          | Tuple xs => Keep (tuple xs)
          | _ => Keep e
      val simplifyExp =
         Trace.trace ("SimplifyTypes.simplifyExp",
                      Exp.layout, Result.layout Exp.layout)
         simplifyExp
      fun simplifyTransfer (t : Transfer.t): Statement.t vector * Transfer.t =
         case t of
            Bug => (Vector.new0 (), t)
          | Call {func, args, return} =>
               (Vector.new0 (),
                Call {func = func, return = return,
                      args = removeUselessVars args})
          | Case {test, cases = Cases.Con cases, default} =>
               let
                  val cases =
                     Vector.keepAll (cases, fn (con, _) =>
                                     not (ConRep.isUseless (conRep con)))
                  val default =
                     case (Vector.length cases, default) of
                        (_,     NONE)    => NONE
                      | (0,     SOME l)  => SOME l
                      | (n,     SOME l)  =>
                           if n = tyconNumCons (Type.deDatatype (oldVarType test))
                              then NONE
                              else SOME l
                  fun normal () =
                     (Vector.new0 (),
                      Case {test = test,
                            cases = Cases.Con cases,
                            default = default})
               in case (Vector.length cases, default) of
                  (0,         NONE)   => (Vector.new0 (), Bug)
                | (0,         SOME l) =>
                     (Vector.new0 (), Goto {dst = l, args = Vector.new0 ()})
                | (1, NONE)   =>
                     let
                        val (con, l) = Vector.first cases
                     in
                        if ConRep.isUseful (conRep con)
                           then
                              (* This case can occur because an array or vector
                               * tycon was kept around.
                               *)
                              normal ()
                        else (* The type has become a tuple.  Do the selects. *)
                           let
                              val ts = keepSimplifyTypes (conArgs con)
                              val (args, stmts) =
                                 if 1 = Vector.length ts
                                    then (Vector.new1 test, Vector.new0 ())
                                 else
                                    Vector.unzip
                                    (Vector.mapi
                                     (ts, fn (i, ty) =>
                                      let
                                         val x = Var.newNoname ()
                                      in
                                         (x,
                                          Statement.T
                                          {var = SOME x,
                                           ty = ty,
                                           exp = Select {tuple = test,
                                                         offset = i}})
                                      end))
                           in
                              (stmts, Goto {dst = l, args = args})
                           end
                     end
                | _ => normal ()
               end
          | Case _ => (Vector.new0 (), t)
          | Goto {dst, args} =>
               (Vector.new0 (), Goto {dst = dst, args = removeUselessVars args})
          | Raise xs => (Vector.new0 (), Raise (removeUselessVars xs))
          | Return xs => (Vector.new0 (), Return (removeUselessVars xs))
          | Runtime {prim, args, return} =>
               (Vector.new0 (), Runtime {prim = prim,
                                         args = Vector.map (args, simplifyVar),
                                         return = return})
      val simplifyTransfer =
         Trace.trace
         ("SimplifyTypes.simplifyTransfer",
          Transfer.layout,
          Layout.tuple2 (Vector.layout Statement.layout, Transfer.layout))
         simplifyTransfer
      fun simplifyStatement (Statement.T {var, ty, exp}) =
         let
            val ty = simplifyMaybeVarType (var, ty)
         in
            (* It is wrong to omit calling simplifyExp when var = NONE because
             * targs in a PrimApp may still need to be simplified.
             *)
            if not (Type.isUnit ty)
               orelse Exp.maySideEffect exp
               orelse (case exp of
                          Profile _ => true
                        | _ => false)
               then
                  (case simplifyExp exp of
                      Bugg => Bugg
                    | Delete => Delete
                    | Keep exp =>
                         Keep (Statement.T {var = var, ty = ty, exp = exp}))
            else Delete
         end
      val simplifyStatement =
         Trace.trace
         ("SimplifyTypes.simplifyStatement",
          Statement.layout,
          Result.layout Statement.layout)
         simplifyStatement
      fun simplifyBlock (Block.T {label, args, statements, transfer}) =
         let
            val args = simplifyFormals args
            val statements =
               Vector.fold'
               (statements, 0, [], fn (_, statement, statements) =>
                case simplifyStatement statement of
                   Bugg => Vector.Done NONE
                 | Delete => Vector.Continue statements
                 | Keep s => Vector.Continue (s :: statements),
                SOME o Vector.fromListRev)
         in
            case statements of
               NONE => Block.T {label = label,
                                args = args,
                                statements = Vector.new0 (),
                                transfer = Bug}
             | SOME statements =>
                  let
                     val (stmts, transfer) = simplifyTransfer transfer
                     val statements = Vector.concat [statements, stmts]
                  in
                     Block.T {label = label,
                              args = args,
                              statements = statements,
                              transfer = transfer}
                  end
         end
      fun simplifyFunction f =
         let
            val {args, mayInline, name, raises, returns, start, ...} =
               Function.dest f
             val args = simplifyFormals args
             val blocks = ref []
             val _ =
                Function.dfs (f, fn block =>
                              (List.push (blocks, simplifyBlock block)
                               ; fn () => ()))
             val returns = Option.map (returns, keepSimplifyTypes)
             val raises = Option.map (raises, keepSimplifyTypes)
         in
            Function.new {args = args,
                          blocks = Vector.fromList (!blocks),
                          mayInline = mayInline,
                          name = name,
                          raises = raises,
                          returns = returns,
                          start = start}
         end
      val globals =
         Vector.concat
         [Vector.new1 (Statement.T {var = SOME unitVar,
                                    ty = Type.unit,
                                    exp = Exp.unit}),
          Vector.keepAllMap (globals, fn s =>
                             case simplifyStatement s of
                                Bugg => Error.bug "SimplifyTypes.globals: bind can't fail"
                              | Delete => NONE
                              | Keep b => SOME b)]
      val shrink = shrinkFunction {globals = globals}
      val functions = List.revMap (functions, shrink o simplifyFunction)
      val program =
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = functions,
                    main = main}
      val _ = destroyTypeCardinality ()
      val _ = destroySimplifyType ()
      val _ = Program.clearTop program
   in
      program
   end

end
