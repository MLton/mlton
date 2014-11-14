(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstAtoms (S: AST_ATOMS_STRUCTS): AST_ATOMS = 
struct

open S

structure Wrap = Region.Wrap
structure Const = AstConst ()
structure Field = Record.Field

structure Tycon =
   struct
      structure Id = AstId (structure Symbol = Symbol)
      open Id

      structure P =
         PrimTycons (structure AdmitsEquality = AdmitsEquality
                     structure CharSize = CharSize
                     structure IntSize = IntSize
                     structure Kind = TyconKind
                     structure RealSize = RealSize
                     structure WordSize = WordSize
                     open Id
                     fun fromString s =
                        Id.fromSymbol (Symbol.fromString s, Region.bogus))
      open P
   end

structure Var = AstId (structure Symbol = Symbol)

structure Con =
   struct
      structure Id = AstId (structure Symbol = Symbol)
      open Id

      structure P =
         PrimCons (open Id
                   fun fromString s = fromSymbol (Symbol.fromString s,
                                                  Region.bogus))

      open P

      val it = fromSymbol (Symbol.itt, Region.bogus)

      fun ensure oper c =
         if List.exists ([cons, falsee, it, nill, reff, truee],
                         fn c' => equals (c, c'))
            then 
               let
                  open Layout
               in
                  Control.error (region c,
                                 seq [str (concat ["can not ", oper, " "]),
                                      layout c],
                                 empty)
               end
         else ()

      val ensureRedefine = ensure "redefine"

      val ensureSpecify = ensure "specify"
   end

structure Basid = AstId (structure Symbol = Symbol)
structure Sigid = AstId (structure Symbol = Symbol)
structure Strid = AstId (structure Symbol = Symbol)
structure Fctid = AstId (structure Symbol = Symbol)

structure Vid =
   struct
      structure I = AstId (structure Symbol = Symbol)
      open I

      fun fromCon c = fromSymbol (Con.toSymbol c, Con.region c)
      fun fromVar x = fromSymbol (Var.toSymbol x, Var.region x)
      local
         fun make f v = f (toSymbol v, region v)
      in
         val toCon = make Con.fromSymbol
         val toVar = make Var.fromSymbol
      end
   end

structure Longtycon =
   struct
      structure T = Longid (structure Id = Tycon
                            structure Strid = Strid
                            structure Symbol = Symbol)

      open T

      val arrow = short Tycon.arrow
   end

structure Longvar = Longid (structure Id = Var
                            structure Strid = Strid
                            structure Symbol = Symbol)

structure Longcon =
   struct
      structure L = Longid (structure Id = Con
                            structure Strid = Strid
                            structure Symbol = Symbol)

      open L
   end

structure Longstrid = Longid (structure Id = Strid
                              structure Strid = Strid
                              structure Symbol = Symbol)


structure Longvid =
   struct
      structure L = Longid (structure Id = Vid
                            structure Strid = Strid
                            structure Symbol = Symbol)

      open L
      local
         fun to (make,node, conv) x =
            let val (T {strids, id}, region) = dest x
            in make (node {strids = strids, id =  conv id}, region)
            end
      in
         val toLongcon = to (Longcon.makeRegion, Longcon.T, Vid.toCon)
      end
   end

open Layout

fun reportDuplicates (v: 'a vector,
                      {equals: 'a * 'a -> bool,
                       layout: 'a -> Layout.t,
                       name: string,
                       region: 'a -> Region.t,
                       term: unit -> Layout.t}) =
   Vector.foreachi
   (v, fn (i, a) =>
    let
       fun loop i' =
          if i = i'
             then ()
          else
             if not (equals (a, Vector.sub (v, i')))
                then loop (i' + 1)
             else
                let
                   open Layout
                in
                   Control.error
                   (region a,
                    seq [str (concat ["duplicate ", name, ": "]), layout a],
                    seq [str "in: ", term ()])
                end
    in
       loop 0
    end)

fun reportDuplicateFields (v: (Field.t * 'a) vector,
                           {region: Region.t,
                            term: unit -> Layout.t}): unit =
   reportDuplicates (v,
                     {equals = fn ((f, _), (f', _)) => Field.equals (f, f'),
                      layout = Field.layout o #1,
                      name = "label",
                      region = fn _ => region,
                      term = term})

structure Type =
   struct
      structure Record = SortedRecord
      open Wrap
      datatype node =
         Con of Longtycon.t * t vector
       | Record of t Record.t
       | Var of Tyvar.t
      withtype t = node Wrap.t
      type node' = node
      type obj = t

      fun make n = makeRegion (n, Region.bogus)
      val var = make o Var
      val record = make o Record
      val tuple = record o Record.tuple
      val unit = tuple (Vector.new0 ())

      fun con (c: Tycon.t, ts: t vector): t =
         if Tycon.equals (c, Tycon.tuple)
            then tuple ts
         else make (Con (Longtycon.short c, ts))

      fun arrow (t1, t2) = con (Tycon.arrow, Vector.new2 (t1, t2))

      fun layoutApp (tycon, args: 'a vector, layoutArg) =
         case Vector.length args of
            0 => tycon
          | 1 => seq [layoutArg (Vector.sub (args, 0)), str " ", tycon]
          | _ => seq [Vector.layout layoutArg args, str " ", tycon]

      fun layout ty =
         case node ty of
            Var v => Tyvar.layout v
          | Con (c, tys) =>
               if Longtycon.equals (c, Longtycon.arrow)
                  then if 2 = Vector.length tys
                          then
                             paren (mayAlign
                                    [layout (Vector.sub (tys, 0)),
                                     seq [str "-> ",
                                          layout (Vector.sub (tys, 1))]])
                       else Error.bug "AstAtoms.Type.layout: non-binary -> tyc"
               else layoutApp (Longtycon.layout c, tys, layout)
          | Record r => Record.layout {record = r,
                                       separator = ":", extra = "",
                                       layoutElt = layout,
                                       layoutTuple = layoutTupleTy}
      and layoutTupleTy tys =
         case Vector.length tys of
            0 => str "unit"
          | 1 => layout (Vector.sub (tys, 0))
          | _ => paren (mayAlign (separateLeft (Vector.toListMap (tys, layout),
                                                "* ")))

      fun layoutOption ty =
         case ty of
            NONE => empty
          | SOME ty => seq [str " of ", layout ty]

      fun checkSyntax (t: t): unit =
         case node t of
            Con (_, ts) => Vector.foreach (ts, checkSyntax)
          | Record r =>
               (reportDuplicateFields (Record.toVector r,
                                       {region = region t,
                                        term = fn () => layout t})
                ; Record.foreach (r, checkSyntax))
          | Var _ => ()
   end

fun bind (x, y) = mayAlign [seq [x, str " ="], y]

fun 'a layoutAnds (prefix: string,
                   xs: 'a vector, 
                   layoutX: Layout.t * 'a -> Layout.t): Layout.t =
   case Vector.toList xs of
      [] => empty
    | x :: xs => align (layoutX (str (concat [prefix, " "]), x)
                        :: List.map (xs, fn x => layoutX (str "and ", x)))

datatype bindStyle = OneLine | Split of int

fun 'a layoutBind (bind: string,
                   layout: 'a -> bindStyle * Layout.t * Layout.t)
   (prefix: Layout.t, x: 'a): Layout.t =
   let
      val (style, lhs, rhs) = layout x
      val lhs = seq [prefix, lhs, str " " , str bind]
   in
      case style of
         OneLine => seq [lhs, str " ", rhs]
       | Split indentation => align [lhs, indent (rhs, indentation)]
   end

fun layoutAndsBind (prefix, bind, xs, layout) =
   layoutAnds (prefix, xs, layoutBind (bind, layout))

(*---------------------------------------------------*)
(*                      TypBind                      *)
(*---------------------------------------------------*)

structure TypBind =
   struct
      datatype node =
         T of {tycon: Tycon.t,
               def: Type.t,
               tyvars: Tyvar.t vector} vector
      open Wrap
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout t =
         let
            val T ds = node t
         in
            layoutAndsBind
            ("type", "=", ds, fn {tycon, def, tyvars} =>
             (OneLine,
              Type.layoutApp (Tycon.layout tycon,
                              tyvars,
                              Tyvar.layout),
              Type.layout def))
         end

      val empty = makeRegion (T (Vector.new0 ()), Region.bogus)

      fun checkSyntax (b: t): unit =
         let
            val T v = node b
            val () = Vector.foreach (v, fn {def, ...} => Type.checkSyntax def)
         in
            reportDuplicates
            (v, {equals = (fn ({tycon = t, ...}, {tycon = t', ...}) =>
                           Tycon.equals (t, t')),
                 layout = Tycon.layout o #tycon,
                 name = "type definition",
                 region = Tycon.region o #tycon,
                 term = fn () => layout b})
         end
   end

(*---------------------------------------------------*)
(*                      DatBind                      *)
(*---------------------------------------------------*)

structure DatBind =
   struct
      datatype node =
         T of {datatypes: {cons: (Con.t * Type.t option) vector,
                           tycon: Tycon.t,
                           tyvars: Tyvar.t vector} vector,
               withtypes: TypBind.t}

      open Wrap
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout (prefix, d) =
         let
            val T {datatypes, withtypes} = node d
         in
            align
            [layoutAndsBind
             (prefix, "=", datatypes, fn {tyvars, tycon, cons} =>
              (OneLine,
               Type.layoutApp (Tycon.layout tycon, tyvars, Tyvar.layout),
               alignPrefix (Vector.toListMap (cons, fn (c, to) =>
                                              seq [Con.layout c,
                                                   Type.layoutOption to]),
                           "| "))),
             case TypBind.node withtypes of
                TypBind.T v =>
                   if 0 = Vector.length v
                      then empty
                   else seq [str "with", TypBind.layout withtypes]]
         end

      fun checkSyntax (b: t): unit =
         let
            val T {datatypes, withtypes} = node b
            val () =
               Vector.foreach
               (datatypes, fn {cons, ...} =>
                Vector.foreach (cons, fn (c, to) =>
                                (Con.ensureRedefine c
                                 ; Option.app (to, Type.checkSyntax))))
            fun term () = layout ("datatype", b)
            val () =
               reportDuplicates
               (Vector.concatV (Vector.map (datatypes, #cons)),
                {equals = fn ((c, _), (c', _)) => Con.equals (c, c'),
                 layout = Con.layout o #1,
                 name = "constructor",
                 region = Con.region o #1,
                 term = term})
            val () =
               reportDuplicates
               (Vector.concat [Vector.map (datatypes, #tycon),
                               let
                                  val TypBind.T v = TypBind.node withtypes
                               in
                                  Vector.map (v, #tycon)
                               end],
                {equals = Tycon.equals,
                 layout = Tycon.layout,
                 name = "type definition",
                 region = Tycon.region,
                 term = term})
         in
            ()
         end
   end

structure DatatypeRhs =
   struct
      datatype node =
         DatBind of DatBind.t
       | Repl of {lhs: Tycon.t, rhs: Longtycon.t}

      open Wrap
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout d =
         case node d of
            DatBind d => DatBind.layout ("datatype", d)
          | Repl {lhs, rhs} =>
               seq [str "datatype ", Tycon.layout lhs,
                   str " = datatype ", Longtycon.layout rhs]

      fun checkSyntax (rhs: t): unit =
         case node rhs of
            DatBind b => DatBind.checkSyntax b
          | Repl _ => ()
   end

(*---------------------------------------------------*)
(*                      ModIdBind                    *)
(*---------------------------------------------------*)

structure ModIdBind =
   struct
      datatype node =
         Fct of {lhs: Fctid.t, rhs: Fctid.t} vector
       | Sig of {lhs: Sigid.t, rhs: Sigid.t} vector
       | Str of {lhs: Strid.t, rhs: Strid.t} vector

      open Wrap
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout d =
         let
            fun doit (prefix, l, bds) =
               layoutAndsBind
               (prefix, "=", bds, fn {lhs, rhs} => (OneLine, l lhs, l rhs))
         in
            case node d of
               Fct bds => doit ("functor", Fctid.layout, bds)
             | Sig bds => doit ("signature", Sigid.layout, bds)
             | Str bds => doit ("structure", Strid.layout, bds)
         end

      fun checkSyntax d =
         let
            fun doit (bds : {lhs: 'a, rhs: 'a} Vector.t, 
                      {equalsId, layoutId, regionId, name}) =
               reportDuplicates
               (bds, {equals = (fn ({lhs = id, ...}, {lhs = id', ...}) =>
                                equalsId (id, id')),
                      layout = layoutId o #lhs,
                      name = concat [name, " definition"],
                      region = regionId o #lhs,
                      term = fn () => layout d})
         in
            case node d of
               Fct bds => doit (bds, {equalsId = Fctid.equals, 
                                      layoutId = Fctid.layout,
                                      regionId = Fctid.region, 
                                      name = "functor"})
             | Sig bds => doit (bds, {equalsId = Sigid.equals, 
                                      layoutId = Sigid.layout,
                                      regionId = Sigid.region, 
                                      name = "signature"})
             | Str bds => doit (bds, {equalsId = Strid.equals, 
                                      layoutId = Strid.layout,
                                      regionId = Strid.region, 
                                      name = "structure"})
         end
   end

end
