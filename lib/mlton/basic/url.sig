(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * URLs, as described in RFC 2396.
 *
 * For ease of programming, I merged all of the nested components into one type
 * with lots of option components.
 *)
signature URL =
   sig
      structure Char:
         sig
            type t = Char.t

            val escapeHex: t -> string
         end

      structure Scheme:
         sig
            datatype t =
               File
             | Ftp
             | Gopher
             | Http
             | Https
             | Telnet
         end

      structure Authority:
         sig
            type t = {host: string,
                      port: int option,
                      user: string option}

            val equals: t * t -> bool
         end

      structure Path:
         sig
            type t = {isAbsolute: bool,
                      path: string list,
                      file: string}

            val file: t -> string
            val layout: t -> Layout.t
            val root: t
            val toString: t -> string
         end

      datatype t =
         T of {authority: Authority.t option,
               fragment: string option,
               path: Path.t option,
               query: string option,
               scheme: Scheme.t option} (* NONE in relative urls *)
       | JavaScript of string
       | MailTo of string
       | News of string
       | Opaque of {scheme: string,
                    rest: string}

      val addQuery: t * string -> t
      val canonicalize: t -> t
      val equals: t * t -> bool
      val escape: string -> string (* Insert %XX escapes into string. *)
      val escapeQuery: bool ref
      val fromString: string -> t option
      val getMatch: Regexp.Match.t -> t
      val host: t -> string
      val layout: t -> Layout.t
      val mailto: string -> t
      val path: t -> Path.t
      (* relativize {base = b, relative = r}
       * trys turn r into a url relative to b
       *)
      val relativize: {base: t, relative: t} -> t option
      (* resolve {base = b, relative = r}
       * interprets r relative to b, returning an absolute URL.
       *)
      val resolve: {base: t, relative: t} -> t
      val toString: t -> string
      val unescape: string -> string (* Remove %XX escapes from string. *) 

      structure Regexp:
         sig
            type t = Regexp.t

            val absoluteUrl: t
            val absPath: t
            val authority: t
            val query: t

            val getAbsPath: Regexp.Match.t -> Path.t
            val peekQuery: Regexp.Match.t -> string option
         end
   end

functor TestUrl (S: URL): sig end =
struct

open S

val _ =
   Assert.assert
   ("TestUrl.resolve", fn () =>
    (* Examples from RFC 2396, Appendix C. *)
    let
       val base = valOf (fromString "http://a/b/c/d;p?q")
       val examples =
          [("g", "http://a/b/c/g"),
           ("./g", "http://a/b/c/g"),
           ("g/", "http://a/b/c/g/"),
           ("/g", "http://a/g"),
           ("?y", "http://a/b/c/?y"),
           ("g?y", "http://a/b/c/g?y"),
           ("#s", "http://a/b/c/d;p?q#s"),
           ("g#s", "http://a/b/c/g#s"),
           ("g?y#s", "http://a/b/c/g?y#s"),
           (";x", "http://a/b/c/;x"),
           ("g;x", "http://a/b/c/g;x"),
           ("g;x?y#s", "http://a/b/c/g;x?y#s"),
           (".", "http://a/b/c/"),
           ("./", "http://a/b/c/"),
           ("..", "http://a/b/"),
           ("../", "http://a/b/"),
           ("../g", "http://a/b/g"),
           ("../..", "http://a/"),
           ("../../", "http://a/"),
           ("../../g", "http://a/g")]
       fun checkResolve (rel, abs) =
          abs = toString (resolve {base = base,
                                   relative = valOf (fromString rel)})
       val checkResolve =
          Trace.trace2
          ("TestUrl.checkResolve", String.layout, String.layout, Bool.layout)
          checkResolve
    in List.forall ([("g:h", "g:h"), ("//g", "http://g")],
                    checkResolve)
       andalso
       List.forall
       (examples, fn (rel, abs) =>
        checkResolve (rel, abs) andalso
        checkResolve (toString
                      (valOf
                       (relativize {base = base,
                                    relative = valOf (fromString abs)})),
                      abs))
    end)

val _ =
   Assert.assert
   ("TestUrl", fn () =>
    fromString "mailto:sweeks@sweeks.com" = SOME (MailTo "sweeks@sweeks.com")
    andalso isSome
            (fromString "http://sports.latimes.com/RealMedia/ads/adstream_lx.ads/sports.latimes.com/stats/oth/oth/oth/columnists.html/21801/Top/NextCardGW002/u40_card_dreamer_V3.gif/63306138643531333339663061393230")
    andalso isSome (fromString
           "http://dps1.travelocity.com:80/airpprice.ctl?previous_page=airpdisp&mixed_gt=N&tkt_status=N&option_num=1&seg_for_sell=1%26SJC%26San%20Jose,%20CA%2620001123%260750%26AA%26American%20Airlines%262456%26L%260%26McDonnell%20Douglas%20SP80%20Jet%26DFW%26Dallas%2fFt%20Worth,%20TX%261313%2620001123%26Thursday%26%26%26S80%26Y|1%26DFW%26Dallas%2fFt%20Worth,%20TX%2620001123%261433%26AA%26American%20Airlines%263741%26L%260%26Embraer%20ERJ-145%20Jet%26OKC%26Oklahoma%20City,%20OK%261529%2620001123%26Thursday%26%26%26ER4%26Y%3a1%26DFW%26Dallas%2fFt%20Worth,%20TX%2620001126%260918%26AA%26American%20Airlines%262451%26V%260%26McDonnell%20Douglas%20SP80%20Jet%26SJC%26San%20Jose,%20CA%261057%2620001126%26Sunday%26%26%26S80%26Y&hold_flag=N&SEQ=97122479938121310102000&LANG=EN&last_pgd_page=airpdisp.pgd")
    andalso isSome (fromString
                   "large-int.html#SIG:INT_INF.\\|@LT\\|\\|@LT\\|:VAL:SPEC")
    andalso
    List.forall
    ([("http://Norma140.emp3.com/cgibin/optin/remove.pl",
       SOME Scheme.Http, NONE, "Norma140.emp3.com", NONE,
       SOME (true, ["cgibin", "optin"], "remove.pl"),
       NONE, NONE),
      ("http://s7.sprintpcs.com/store/..\\store\\cc_Popup_aa.asp",
       SOME Scheme.Http, NONE, "s7.sprintpcs.com", NONE,
       SOME (true, ["store"], "..\\store\\cc_Popup_aa.asp"),
       NONE, NONE),
      ("http://www.sds.lcs.mit.edu/spd/larch/",
       SOME Scheme.Http, NONE, "www.sds.lcs.mit.edu", NONE,
       SOME (true, ["spd", "larch"], ""),
       NONE, NONE),
      ("http://foo.com/hello",
       SOME Scheme.Http, NONE, "foo.com", NONE, SOME (true, [], "hello"),
       NONE, NONE),
      ("http://foo.com/hello/",
       SOME Scheme.Http, NONE, "foo.com", NONE, SOME (true, ["hello"], ""),
       NONE, NONE),
      ("http://foo.com",
       SOME Scheme.Http, NONE, "foo.com", NONE, NONE, NONE, NONE),
      ("http://foo.com/",
       SOME Scheme.Http, NONE, "foo.com", NONE, SOME (true, [], ""), NONE, NONE),
      ("ftp://bar.com:80/yes/now",
       SOME Scheme.Ftp, NONE, "bar.com", SOME 80, SOME (true, ["yes"], "now"), NONE,
       NONE),
      ("http://z.com/foo?site=http://w.com/~zzz",
       SOME Scheme.Http, NONE, "z.com", NONE, SOME (true, [], "foo"),
       SOME "site=http://w.com/~zzz", NONE),
      ("http://sweeks@foo.com/yes?really#here",
       SOME Scheme.Http, SOME "sweeks", "foo.com", NONE, SOME (true, [], "yes"),
       SOME "really", SOME "here"),
      ("http://a.com/foo?%79%65%73%2e",
       SOME Scheme.Http, NONE, "a.com", NONE, SOME (true, [], "foo"), SOME "yes.", NONE),
      ("http://foo.com/a?b%20c",
       SOME Scheme.Http, NONE, "foo.com", NONE,
       SOME (true, [], "a"),
       SOME "b c",
       NONE),
      ("http://community.cnn.com/cgi-bin/WebX?13@236.FzKWcVUHjLB^0@.ee7bada",
       SOME Scheme.Http, NONE, "community.cnn.com", NONE,
       SOME (true, ["cgi-bin"], "WebX"),
       SOME "13@236.FzKWcVUHjLB^0@.ee7bada",
       NONE),
      ("//foo.com/z", NONE, NONE, "foo.com", NONE, SOME (true, [], "z"), NONE, NONE),
      ("http://ad.doubleclick.net/adj/N674.briefing.com/B22024;abr=!ie;sz=125x125;ord= [timestamp]?",
       SOME Scheme.Http, NONE, "ad.doubleclick.net", NONE,
       SOME (true, ["adj", "N674.briefing.com"],
            "B22024;abr=!ie;sz=125x125;ord= [timestamp]"),
       SOME "",
       NONE),
      ("http://tac.eecs.umich.edu/cgi-bin/botuser/ViewAccount?VIEW=INFO&VIEWALL= [bad label VIEWALL]",
       SOME Scheme.Http, NONE, "tac.eecs.umich.edu", NONE,
       SOME (true, ["cgi-bin", "botuser"], "ViewAccount"),
       SOME "VIEW=INFO&VIEWALL= [bad label VIEWALL]",
       NONE),
      ("http://phase2media.doubleclick.net/adj/ag.aol.p2m.com/asconfirmcollections;kw=shortandpunchy;kw1=;kw2=;abr=!ie;pos=1;sz=125x125;tile=10;ord=964739477869?\"",
       SOME Scheme.Http, NONE, "phase2media.doubleclick.net", NONE,
       SOME (true, ["adj", "ag.aol.p2m.com"], "asconfirmcollections;kw=shortandpunchy;kw1=;kw2=;abr=!ie;pos=1;sz=125x125;tile=10;ord=964739477869"),
       SOME "\"",
       NONE)
      ],
    fn (s, scheme, user, host, port, path, query, fragment) =>
    valOf (fromString s)
    = T {scheme = scheme,
        authority = SOME {user = user,
                         host = host,
                         port = port},
        path = Option.map (path, fn (i, p, f) => {isAbsolute = i,
                                                 path = p, file = f}),
        query = query,
        fragment = fragment}))

end
