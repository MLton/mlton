(* pp.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(*
 * pp.sml - Some simple pretty-printing infrastructure for the ml-ffigen
 *          program.
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure PrettyPrint = struct

    structure PP = PPStreamFn (structure Token = StringToken
                               structure Device = CPIFDev)

    datatype mltype =
        ARROW of mltype * mltype
      | TUPLE of mltype list
      | CON of string * mltype list
      | RECORD of (string * mltype) list

    val Unit = TUPLE []
    fun Type t = CON (t, [])
    fun St tag = Type (concat ["ST_", tag, ".tag"])
    fun Un tag = Type (concat ["UT_", tag, ".tag"])
    fun En tag = Type (concat ["ET_", tag, ".tag"])

    datatype tcontext = C_STAR | C_ARROW | C_COMMA | C_CON

    fun simplify (CON ("unit", [])) = Unit
      | simplify (TUPLE [t]) = simplify t
      | simplify (CON (k, tl)) = 
        let
           fun doDefault () = CON (k, map simplify tl)
           fun doObj obj =
               case tl of
                  [CON (k, tl), c] =>
                     if List.exists (fn k' => k = k')
                        ["schar","uchar","sshort","ushort",
                         "sint","uint","slong","ulong",
                         "slonglong","ulonglong","float","double",
                         "voidptr"]
                        then CON (concat [k, "_", obj], [simplify c])
                     else if k = "fptr"
                        then case tl of 
                                [f] => CON ("fptr_" ^ obj, [simplify f, simplify c])
                              | _ => doDefault ()
                     else if k = "su"
                         then case tl of
                                [su] => CON ("su_" ^ obj, [simplify su, simplify c])
                              | _ => doDefault ()
                     else doDefault ()
                | _ => doDefault ()
           fun doDim d =
               if d = "dim"
                  then case tl of
                          [n, CON (k', [])] => 
                             if k' = "Dim.nonzero" orelse k' = "nonzero"
                                then CON ("dim", [simplify n])
                                else doDefault ()
                        | _ => doDefault ()
               else if d = "dec"
                  then case tl of 
                          [] => CON ("dec", [])
                        | _ => doDefault ()
               else if List.exists (fn d' => d = d')
                       ["dg0","dg1","dg2","dg3","dg4",
                        "dg5","dg6","dg7","dg8","dg9"]
                  then case tl of
                          [n] => CON (d, [simplify n])
                        | _ => doDefault ()
               else doDefault ()
        in
           if k = "obj" orelse k = "obj'"
              then doObj k
           else if String.isPrefix "Dim." k
              then doDim (String.extract(k,4,NONE))
           else doDefault ()
        end
      | simplify (ARROW (t1, t2)) = ARROW (simplify t1, simplify t2)
      | simplify (TUPLE tl) = TUPLE (map simplify tl)
      | simplify (RECORD ml) = RECORD (map (fn (n, t) => (n, simplify t)) ml)

    fun ppType0 s (t as ARROW _, c) =
        let fun loop (ARROW (x, y)) =
                (ppType0 s (x, C_ARROW); PP.string s " ->"; PP.space s 1;
                 loop y)
              | loop t = ppType0 s (t, C_ARROW)
            val paren = not (c = C_COMMA)
            val indent = if paren then 5 else 4
        in
            PP.openHOVBox s (PP.Rel indent);
            if paren then PP.string s "(" else ();
            loop t;
            if paren then PP.string s ")" else ();
            PP.closeBox s
        end
      | ppType0 s (TUPLE [], _) = PP.string s "unit"
      | ppType0 s (TUPLE [t], c) = ppType0 s (t, c)
      | ppType0 s (TUPLE tl, c) = let
            fun loop [] = ()    (* cannot happen *)
              | loop [t] = ppType0 s (t, C_STAR)
              | loop (h :: tl) = (ppType0 s (h, C_STAR);
                                  PP.string s " *";
                                  PP.space s 1;
                                  loop tl)
            val paren =
                case c of (C_STAR) => true
                        | (C_CON) => true
                        | (C_ARROW) => false
                        | (C_COMMA) => false
            val indent = if paren then 1 else 0
        in
            PP.openHVBox s (PP.Rel indent);
            if paren then PP.string s "(" else ();
            loop tl;
            if paren then PP.string s ")" else ();
            PP.closeBox s
        end
      | ppType0 s (RECORD [], _) = PP.string s "{}"
      | ppType0 s (RECORD tl, _) = let
            fun loop [] = ()            (* cannot happen *)
              | loop [(n, t)] = (PP.string s (n ^ " : ");
                                 ppType0 s (t, C_COMMA))
              | loop ((n, t) :: tl) = (PP.string s (n ^ " : ");
                                       ppType0 s (t, C_COMMA);
                                       PP.string s ",";
                                       PP.space s 1;
                                       loop tl)
        in
            PP.openHVBox s (PP.Rel 2);
            PP.string s "{ ";
            loop tl;
            PP.string s " }";
            PP.closeBox s
        end
      | ppType0 s (CON (k, []), _) = PP.string s k
      | ppType0 s (CON (k, [t]), _) =
        (PP.openHBox s;
         ppType0 s (t, C_CON);
         PP.space s 1;
         PP.string s k;
         PP.closeBox s)
      | ppType0 s (CON (k, tl), _) = let
            fun loop [] = ()    (* cannot happen *)
              | loop [t] = ppType0 s (t, C_COMMA)
              | loop (h :: tl) =
                (ppType0 s (h, C_COMMA); PP.string s ","; PP.space s 1; loop tl)
        in
            PP.openHBox s;
            PP.openHVBox s (PP.Rel 1);
            PP.string s "(";
            loop tl;
            PP.string s ")";
            PP.closeBox s;
            PP.space s 1;
            PP.string s k;
            PP.closeBox s
        end

    (* start with comma context *)
    fun ppType s t = ppType0 s (simplify t, C_COMMA)
    fun ppType' s (t, c) = ppType0 s (simplify t, c)

    datatype mlexp =
        ETUPLE of mlexp list
      | ERECORD of (string * mlexp) list
      | EVAR of string
      | EAPP of mlexp * mlexp
      | ECONSTR of mlexp * mltype
      | ESEQ of mlexp * mlexp
      | EPRIM of string * mltype
      | ELET of (string * mlexp) list * mlexp

    datatype econtext = EC_APP | EC_COMMA

    fun ppExp0 s (ETUPLE [], _) = PP.string s "()"
      | ppExp0 s (ETUPLE [x], c) = ppExp0 s (x, c)
      | ppExp0 s (ETUPLE xl, _) = let
            fun loop [] = ()
              | loop [x] = ppExp0 s (x, EC_COMMA)
              | loop (x :: xl) =
                (ppExp0 s (x, EC_COMMA); PP.string s ","; PP.space s 1;
                 loop xl)
        in
            PP.openHVBox s (PP.Rel 1);
            PP.string s "(";
            loop xl;
            PP.string s ")";
            PP.closeBox s
        end
      | ppExp0 s (ERECORD [], _) = PP.string s "{}"
      | ppExp0 s (ERECORD xl, _) = let
            fun loop [] = ()
              | loop [(n, x)] = (PP.string s (n ^ " =");
                                 PP.space s 1;
                                 ppExp0 s (x, EC_COMMA))
              | loop ((n, x) :: xl) = (PP.string s (n ^ " =");
                                       PP.space s 1;
                                       ppExp0 s (x, EC_COMMA);
                                       PP.string s ",";
                                       PP.space s 1;
                                       loop xl)
        in
            PP.openHVBox s (PP.Rel 2);
            PP.string s "{ ";
            loop xl;
            PP.string s " }";
            PP.closeBox s
        end
      | ppExp0 s (EVAR v, _) = PP.string s v
      | ppExp0 s (EAPP (x, y), c) = let
            fun loop (EAPP (x, y)) =
                (loop x; ppExp0 s (y, EC_APP); PP.space s 1)
              | loop x = (ppExp0 s (x, EC_APP);
                          PP.space s 1;
                          PP.openHOVBox s (PP.Rel 0))
            val paren = c = EC_APP
        in
            PP.openHOVBox s (PP.Abs 4);
            if paren then PP.string s "(" else ();
            loop x;
            ppExp0 s (y, EC_APP);
            if paren then PP.string s ")" else ();
            PP.closeBox s;
            PP.closeBox s
        end
      | ppExp0 s (ECONSTR (x, t), c) = let
            val paren = c = EC_APP
            val indent = if paren then 5 else 4
            val tc = if paren then C_CON else C_COMMA
        in
            PP.openHOVBox s (PP.Rel indent);
            if paren then PP.string s "(" else ();
            ppExp0 s (x, c);
            PP.nbSpace s 1;
            PP.string s ":";
            PP.space s 1;
            ppType' s (t, tc);
            if paren then PP.string s ")" else  ();
            PP.closeBox s
        end
      | ppExp0 s (ESEQ (x, y), c) = let
        in
            PP.string s "(";
            PP.openHVBox s (PP.Rel 0);
            ppExp0 s (x, EC_COMMA);
            PP.string s ";";
            PP.space s 1;
            ppExp0 s (y, EC_COMMA);
            PP.string s ")";
            PP.closeBox s
        end
      | ppExp0 s (EPRIM (p, t), c) = let
            val paren = c = EC_APP
            val indent = if paren then 5 else 4
            val tc = if paren then C_CON else C_COMMA
        in
            PP.openHOVBox s (PP.Rel indent);
            if paren then PP.string s "(" else ();
            PP.string s p;
            PP.nbSpace s 1;
            PP.string s ":";
            PP.space s 1;
            ppType' s (t, tc);
            PP.string s ";";
            if paren then PP.string s ")" else  ();
            PP.closeBox s
        end
     | ppExp0 s (ELET ([], e), c) = ppExp0 s (e, c)
     | ppExp0 s (ELET (bnds, e), c) = let
           fun loop [] = ()
             | loop ((v, e) :: bnds) = (PP.newline s;
                                        PP.openHOVBox s (PP.Abs 4);
                                        PP.string s "val";
                                        PP.nbSpace s 1;
                                        PP.string s v;
                                        PP.nbSpace s 1;
                                        PP.string s "=";
                                        PP.space s 1;
                                        ppExp0 s (e, EC_COMMA);
                                        PP.closeBox s;
                                        loop bnds)
       in
           PP.string s "let";
           PP.openVBox s (PP.Abs 4);
           loop bnds;
           PP.closeBox s;
           PP.newline s;
           PP.string s "in";
           PP.openVBox s (PP.Abs 4);
           PP.newline s;
           ppExp0 s (e, EC_COMMA);
           PP.closeBox s;
           PP.newline s;
           PP.string s "end"
       end

    fun ppExp s x = ppExp0 s (x, EC_COMMA)
    fun ppExp' s x = ppExp0 s (x, EC_APP)

    fun ppFun s (name, args, body) =
        (PP.openHOVBox s (PP.Rel 4);
         PP.string s ("fun " ^ name);
         PP.nbSpace s 1;
         app (fn a => (ppExp' s a; PP.space s 1)) args;
         PP.string s "=";
         PP.nbSpace s 1;
         PP.openBox s (PP.Rel 0);
         ppExp s body;
         PP.closeBox s;
         PP.closeBox s)
end
