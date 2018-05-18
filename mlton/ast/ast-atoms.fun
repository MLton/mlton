(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstAtoms (S: AST_ATOMS_STRUCTS): AST_ATOMS = 
struct

open S

structure Wrap = Region.Wrap
structure Field = Record.Field

structure Const = AstConst ()

structure Tyvar =
   struct
      structure Id = AstId (structure Symbol = Symbol)
      open Id
      fun isEquality t =
         let
            val s = toString t
         in
            String.length s > 1 andalso String.sub (s, 1) = #"'"
         end
   end

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

      val special =
         [cons, falsee, nill, reff, truee]
   end

structure Basid = AstId (structure Symbol = Symbol)
structure Sigid = AstId (structure Symbol = Symbol)
structure Strid = AstId (structure Symbol = Symbol)
structure Strid =
   struct
      open Strid
      local
         fun make s = fromSymbol (Symbol.fromString s, Region.bogus)
      in
         val uArg = fn s => make ("_arg_" ^ s)
         val uRes = fn s => make ("_res_" ^ s)
         val uStr = make "_str"
         val uSig = make "_sig"
      end
   end
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

      val it = fromSymbol (Symbol.itt, Region.bogus)
      val equal = fromSymbol (Symbol.equal, Region.bogus)
      val specialCons = List.map (Con.special, fromCon)

      fun checkSpecial (oper, ctrl) (vid, {allowIt, ctxt, keyword}) =
         if not (Control.Elaborate.current ctrl)
            andalso
            ((not allowIt andalso equals (vid, it))
             orelse
             equals (vid, equal)
             orelse
             List.exists (specialCons, fn vid' => equals (vid, vid')))
            then
               let
                  open Layout
               in
                  Control.error (region vid,
                                 seq [str "special identifier cannot be ",
                                      str oper,
                                      str " by ",
                                      str keyword,
                                      str ": ",
                                      layout vid],
                                 ctxt ())
               end
         else ()

      val checkRedefineSpecial =
         checkSpecial ("redefined", Control.Elaborate.allowRedefineSpecialIds)

      val checkSpecifySpecial =
         checkSpecial ("specified", Control.Elaborate.allowSpecifySpecialIds)
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

fun mkCtxt (x, lay) () =
   seq [str "in: ", lay x]

fun reportDuplicates (v: 'a vector,
                      {ctxt: unit -> Layout.t,
                       equals: 'a * 'a -> bool,
                       layout: 'a -> Layout.t,
                       name: string,
                       region: 'a -> Region.t}) =
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
                    ctxt ())
                end
    in
       loop 0
    end)

fun reportDuplicateFields (v: (Field.t * (Region.t * 'a)) vector,
                           {ctxt: unit -> Layout.t}): unit =
   reportDuplicates (v,
                     {ctxt = ctxt,
                      equals = fn ((f, _), (f', _)) => Field.equals (f, f'),
                      layout = Field.layout o #1,
                      name = "label",
                      region = #1 o #2})

fun reportDuplicateTyvars (v: Tyvar.t vector,
                           {ctxt: unit -> Layout.t}): unit =
   reportDuplicates (v,
                     {ctxt = ctxt,
                      equals = Tyvar.equals,
                      layout = Tyvar.layout,
                      name = "type variable",
                      region = Tyvar.region})

structure Type =
   struct
      open Wrap
      datatype node =
         Con of Longtycon.t * t vector
       | Paren of t
       | Record of (Region.t * t) Record.t
       | Var of Tyvar.t
      withtype t = node Wrap.t
      type node' = node
      type obj = t

      fun make n = makeRegion (n, Region.bogus)
      val var = make o Var
      val record = make o Record
      val tuple = record o Record.tuple o (fn tys => Vector.map (tys, fn ty => (Region.bogus, ty)))
      val unit = tuple (Vector.new0 ())

      fun con (c: Tycon.t, ts: t vector): t =
         if Tycon.equals (c, Tycon.tuple)
            then tuple ts
         else make (Con (Longtycon.short c, ts))

      fun arrow (t1, t2) = con (Tycon.arrow, Vector.new2 (t1, t2))

      fun layoutApp (tycon, args: 'a vector, layoutArg) =
         case Vector.length args of
            0 => tycon
          | 1 => seq [layoutArg (Vector.first args), str " ", tycon]
          | _ => seq [Vector.layout layoutArg args, str " ", tycon]

      fun layout ty =
         case node ty of
            Var v => Tyvar.layout v
          | Con (c, tys) =>
               if Longtycon.equals (c, Longtycon.arrow)
                  then if 2 = Vector.length tys
                          then
                             paren (mayAlign
                                    [layout (Vector.first tys),
                                     seq [str "-> ",
                                          layout (Vector.sub (tys, 1))]])
                       else Error.bug "AstAtoms.Type.layout: non-binary -> tyc"
               else layoutApp (Longtycon.layout c, tys, layout)
          | Paren t => layout t
          | Record r => Record.layout {record = r,
                                       separator = ": ", extra = "",
                                       layoutElt = layout o #2,
                                       layoutTuple = fn rtys => layoutTupleTy (Vector.map (rtys, #2))}
      and layoutTupleTy tys =
         case Vector.length tys of
            0 => str "unit"
          | 1 => layout (Vector.first tys)
          | _ => paren (mayAlign (separateLeft (Vector.toListMap (tys, layout),
                                                "* ")))

      fun layoutOption ty =
         case ty of
            NONE => empty
          | SOME ty => seq [str " of ", layout ty]

      fun checkSyntax (t: t): unit =
         case node t of
            Con (_, ts) => Vector.foreach (ts, checkSyntax)
          | Paren t => checkSyntax t
          | Record r =>
               (reportDuplicateFields (Record.toVector r,
                                       {ctxt = mkCtxt (t, layout)})
                ; Record.foreach (r, checkSyntax o #2))
          | Var _ => ()
   end

fun bind (x, y) = mayAlign [seq [x, str " ="], y]

fun 'a layoutAndsSusp (prefix: string,
                       xs: 'a vector,
                       layoutX: bool * Layout.t * 'a -> Layout.t): (unit -> Layout.t) vector =
   Vector.mapi
   (xs, fn (i, x) => fn () =>
    layoutX (i = 0, if i = 0 then str (concat [prefix, " "]) else str "and ", x))

fun 'a layoutAnds (prefix: string,
                   xs: 'a vector, 
                   layoutX: Layout.t * 'a -> Layout.t): Layout.t =
   align (Vector.toListMap (layoutAndsSusp (prefix, xs, fn (_, prefix, x) => layoutX (prefix, x)), fn th => th ()))

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

      fun isEmpty (b: t) =
         let
            val T ds = node b
         in
            Vector.isEmpty ds
         end

      fun checkSyntax (b: t, kind: string): unit =
         let
            val T v = node b
            val () =
               Vector.foreach
               (v, fn {tyvars, tycon, def} =>
                (reportDuplicateTyvars
                 (tyvars, {ctxt = fn () =>
                           seq [str "in: ",
                                Type.layoutApp
                                (Tycon.layout tycon,
                                 tyvars, Tyvar.layout)]})
                 ; Type.checkSyntax def))
         in
            reportDuplicates
            (v, {ctxt = mkCtxt (b, layout),
                 equals = (fn ({tycon = t, ...}, {tycon = t', ...}) =>
                           Tycon.equals (t, t')),
                 layout = Tycon.layout o #tycon,
                 name = "type " ^ kind,
                 region = Tycon.region o #tycon})
         end
      fun checkSyntaxDef b = checkSyntax (b, "definition")
      fun checkSyntaxSpec b = checkSyntax (b, "specification")
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
                   if Vector.isEmpty v
                      then empty
                   else seq [str "with", TypBind.layout withtypes]]
         end

      fun checkSyntax (b: t, kind: string,
                       vidCheckSpecial: Vid.t * {allowIt: bool,
                                                 ctxt: unit -> Layout.t,
                                                 keyword: string} -> unit): unit =
         let
            val T {datatypes, withtypes} = node b
            val TypBind.T withtypes = TypBind.node withtypes
            val ctxt = mkCtxt ((), fn () => layout ("datatype", b))
            val () =
               Vector.foreach
               (datatypes, fn {tyvars, tycon, cons} =>
                (reportDuplicateTyvars
                 (tyvars, {ctxt = fn () =>
                           seq [str "in: ",
                                Type.layoutApp
                                (Tycon.layout tycon,
                                 tyvars, Tyvar.layout)]})
                 ; Vector.foreach
                   (cons, fn (c, to) =>
                    (vidCheckSpecial
                     (Vid.fromCon c,
                      {allowIt = false,
                       ctxt = ctxt,
                       keyword = "datatype"})
                     ; Option.app (to, Type.checkSyntax)))))
            val () =
               reportDuplicates
               (Vector.concatV (Vector.map (datatypes, #cons)),
                {ctxt = ctxt,
                 equals = fn ((c, _), (c', _)) => Con.equals (c, c'),
                 layout = Con.layout o #1,
                 name = "constructor " ^ kind,
                 region = Con.region o #1})
            val () =
               Vector.foreach
               (withtypes, fn {tyvars, tycon, def} =>
                (reportDuplicateTyvars
                 (tyvars, {ctxt = fn () =>
                           seq [str "in: ",
                                Type.layoutApp
                                (Tycon.layout tycon,
                                 tyvars, Tyvar.layout)]})
                 ; Type.checkSyntax def))
            val () =
               reportDuplicates
               (Vector.concat [Vector.map (datatypes, #tycon),
                               Vector.map (withtypes, #tycon)],
                {ctxt = ctxt,
                 equals = Tycon.equals,
                 layout = Tycon.layout,
                 name = "type " ^ kind,
                 region = Tycon.region})
         in
            ()
         end
      fun checkSyntaxDef b =
         checkSyntax (b, "definition", Vid.checkRedefineSpecial)
      fun checkSyntaxSpec b =
         checkSyntax (b, "specification", Vid.checkSpecifySpecial)
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

      fun checkSyntax (rhs: t, datBindCheckSyntax) =
         case node rhs of
            DatBind b => datBindCheckSyntax b
          | Repl _ => ()
      fun checkSyntaxDef rhs = checkSyntax (rhs, DatBind.checkSyntaxDef)
      fun checkSyntaxSpec rhs = checkSyntax (rhs, DatBind.checkSyntaxSpec)
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
               (bds, {ctxt = mkCtxt (d, layout),
                      equals = (fn ({lhs = id, ...}, {lhs = id', ...}) =>
                                equalsId (id, id')),
                      layout = layoutId o #lhs,
                      name = concat [name, " definition"],
                      region = regionId o #lhs})
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
