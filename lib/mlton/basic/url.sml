(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Url: URL =
struct

val escapeQuery = ref true

structure Char =
   struct
      open Char

      val radix: int = 16

      fun fromHexChars (hi: t, lo: t) =
         chr (toHexDigit hi * radix + toHexDigit lo)

      fun toHexChars (c: t): t * t =
         let
            val (hi, lo) = Int.divMod (ord c, radix)
         in
            (fromHexDigit hi, fromHexDigit lo)
         end

      fun escapeHex (c: t): string =
         let
            val (hi, lo) = toHexChars c
         in
            implode [#"%", hi, lo]
         end

   end

fun unescape (s: string): string =
   let
      fun sub i = String.sub (s, i)
      val n = String.size s
      fun loop (i, cs) =
         if i >= n
            then implode (rev cs)
         else let val c = sub i
              in if c = #"%"
                    then loop (i + 3,
                              Char.fromHexChars (sub (i + 1), sub (i + 2)) :: cs)
                 else loop (i + 1, c :: cs)
              end
   in loop (0, [])
   end   

val shouldEscape: char -> bool =
   Char.memoize (fn c => 
                not (Char.isGraph c)
                orelse c = Char.dquote
                orelse String.contains ("?<>#% {}|\\^ []`", c))

val shouldEscape =
   Trace.trace ("Url.shouldEscape", Char.layout, Bool.layout) shouldEscape

fun escape s =
   String.translate
   (s, fn c =>
    if shouldEscape c
       then Char.escapeHex c
    else str c)

structure Scheme =
   struct
      datatype t =
         File
       | Ftp
       | Gopher
       | Http
       | Https
       | Telnet

      val map =
         [("file", File),
          ("ftp", Ftp),
          ("gopher", Gopher),
          ("http", Http),
          ("https", Https),
          ("telnet", Telnet)]

      val fromString =
         String.memoizeList (fn _ => Error.bug "Url.Scheme.fromString", map)

      val equals: t * t -> bool = op =

      fun toString s =
         #1 (valOf (List.peek (map, fn (_, s') => equals (s, s'))))

      val layout = Layout.str o toString
   end

structure Authority =
   struct
      type t = {user: string option,
                host: string,
                port: int option}

      fun layout ({user, host, port}: t) =
         Layout.record [("user", Option.layout String.layout user),
                        ("host", String.layout host),
                        ("port", Option.layout Int.layout port)]

      fun canonicalize {user, host, port} =
         {user = Option.map (user, String.toLower),
          host = String.toLower host,
          port = port}

      fun equals ({user = u, host = h, port = p}: t,
                 {user = u', host = h', port = p'}: t): bool =
         Option.equals (u, u', String.equals)
         andalso String.toLower h = String.toLower h'
         andalso Option.equals (p, p', Port.equals)

      val equals =
         Trace.trace2 ("Url.Authority.equals", layout, layout, Bool.layout) equals
   end

(* The numbers in comments are rule numbers from Section 5.2 of RFC 2396. *)
(* canonicalizePath (p1, p2, f)
 * Assume p1 is already canonicalized.
 *)
fun canonicalizePath (p1: string list, p2: string list, f: string) =
   let 
      fun loop (r, ac) =
         case r of
            [] =>
               (case f of
                   "." => (rev ac, "") (* 6d *)
                 | ".." => (case ac of
                               [] => ([], "..")
                             | ".." :: _ => (rev ac, "..")
                             | _ :: ac => (rev ac, "")) (* 6f *)
                 | _ => (rev ac, f))
          | "" :: r => loop (r, ac)
          | "." :: r => loop (r, ac) (* 6c *)
          | ".." :: r => loop (r,
                              case ac of
                                 [] => [".."]
                               | ".." :: _ => ".." :: ac
                               | _ :: ac => ac) (* 6e *)
          | s :: r => loop (r, s :: ac)
   in loop (p2, rev p1)
   end

structure Path =
   struct
      type t = {file: string,
                isAbsolute: bool,
                path: string list}

      local
         fun make f (p: t) = f p
      in
         val file = make #file
         val isAbsolute = make #isAbsolute
         val path = make #path
      end

      val root = {isAbsolute = true,
                  path = [],
                  file = ""}

      fun canonicalize {isAbsolute = i, path = p, file = f} =
         let val (p, f) = canonicalizePath ([], p, f)
         in {isAbsolute = i, path = p, file = f}
         end

      fun toString ({isAbsolute, path, file}) =
         concat [if isAbsolute then "/" else "",
                 escape (concat (List.separate (path @ [file], "/")))]

      val layout = Layout.str o toString
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

fun addQuery (u: t, q) =
   case u of
      T {authority, fragment, path, query, scheme}=>
         if isSome query
            then Error.bug "Url.addQuery"
         else
            T {authority = authority,
               fragment = fragment,
               path = path,
               query = SOME q,
               scheme = scheme}
    | _ => Error.bug "Url.addQuery"

fun host (u: t): string =
   case u of
      T {authority = SOME {host, ...}, ...} => host
    | _ => Error.bug "Url.host"

fun path (u: t): Path.t =
   case u of
      T {path = SOME p, ...} => p
    | _ => Error.bug "Url.path"

fun mo (opt, f) =
   case opt of
      NONE => ""
    | SOME x => f x

fun toString url =
   case url of
      T {scheme, authority, path, query, fragment} =>
         concat [mo (scheme, fn s => concat [Scheme.toString s, ":"]),
                 mo (authority, fn {user, host, port} => 
                    concat ["//",
                           mo (user, fn u => concat [escape u, "@"]),
                           host,
                           mo (port, fn p => concat [":", Int.toString p])]),
                 mo (path, Path.toString),
                 mo (query, fn q => concat ["?", if !escapeQuery then escape q
                                               else q]),
                 mo (fragment, fn f => concat ["#", escape f])
                 ]
    | JavaScript s => concat ["javascript:", escape s]
    | MailTo email => concat ["mailto:", escape email]
    | News group => concat ["news:", escape group]
    | Opaque {scheme, rest} => concat [scheme, ":", escape rest]

val layout = Layout.str o toString

val toString =
   Trace.trace ("Url.toString", layout, String.layout) toString

val layout =
   fn T {scheme, authority, path, query, fragment} =>
        Layout.record [("scheme", Option.layout Scheme.layout scheme),
                       ("authority", Option.layout Authority.layout authority),
                       ("path", Option.layout Path.layout path),
                       ("query", Option.layout String.layout query),
                       ("fragment", Option.layout String.layout fragment)]
    | u => layout u

val equals: t * t -> bool = op =

structure Regexp =
   struct
      open Regexp

      val digit = isChar Char.isDigit
      val upalpha = isChar Char.isUpper
      val lowalpha = isChar Char.isLower
      val alpha = isChar Char.isAlpha
      val alphanum = isChar Char.isAlphaNum
      val hex = isChar Char.isHexDigit
      val escaped = seq [char #"%", hex, hex]
      val mark = oneOf "-_.!~*' ()"
      val unreserved = or [alphanum, mark]
      val reserved = oneOf ";/?:@&=+$,"
      val printable = isChar Char.isPrint
      (*val urlc = or [reserved, unreserved, escaped]*)
      (* It's pointless to follow the spec on urlc, which rules out lots of
       * printable characters.  Lot's of sites use printable characters outside
       * the spec, and browsers handle them, so we should too.
       *)
      val urlc = printable
      val fragment' = Save.new ()
      val fragment = save (star urlc, fragment')
      val query' = Save.new ()
(* The official definition of query says urlc*, but this doesn't work with
 * our expanded meaning of urlc = printable, since then the query consumes
 * the fragment.
 *)
(*      val query = save (star urlc, query') *)
      val query = save (star (isChar (fn c => Char.isPrint c
                                      andalso c <> #"#")),
                        query')
      val port' = Save.new ()
      val port = save (star digit, port')
      val IPv4address = seq [oneOrMore digit, char #".",
                             oneOrMore digit, char #".",
                             oneOrMore digit, char #".",
                             oneOrMore digit]
      val toplabel = or [alpha,
                         seq [alpha, star (or [alphanum, char #"-"]), alphanum]]
      val domainlabel = or [alphanum,
                           seq [alphanum,
                               star (or [alphanum, char #"-"]),
                               alphanum]]
      val hostname = seq [star (seq [domainlabel, char #"."]),
                          toplabel,
                          optional (char #".")]
      val host' = Save.new ()
      val host = save (or [hostname, IPv4address], host')
      val hostport = seq [host, optional (seq [char #":", port])]
      val userinfo' = Save.new ()
      val userinfo =
         save (star (or [unreserved, escaped, oneOf ";:&=+$"]), userinfo')
      val server = optional (seq [optional (seq [userinfo, char #"@"]),
                                  hostport])
      val regName' = Save.new ()
      val regName =
         save (oneOrMore (or [unreserved,
                              escaped,
                              oneOf "$,;:@&=+"]),
               regName')
      val authority = or [server, regName]
      val scheme' = Save.new ()
      val scheme =
         save (seq [alpha, star (or [alpha, digit, oneOf "+-."])], scheme')
      val relSegment' = Save.new ()
      val relSegment =
         save (oneOrMore (or [unreserved, escaped, oneOf ";@&=+$,"]),
               relSegment')
      (* val pchar = or [unreserved, escaped, oneOf ":@&=+$,", wrong] *)
      (* val param = star pchar *)
      (* val segment = seq [star pchar, star (seq [char #";", param])] *)
      (* val pathSegments = seq [segment, star (seq [char #"/", segment])] *)
      val pathSegments' = Save.new ()
      val pathSegments =
         save (star (isChar (fn c => (Char.isPrint c andalso
                                      not (String.contains ("?#", c))))),
               pathSegments')
      val absPath = seq [char #"/", pathSegments]
      val relPath = seq [relSegment, optional absPath]
      val netPath = seq [string "//", authority, optional absPath]
      val urlcNoSlash = or [unreserved, escaped, oneOf ";?:@&=+$,"]
      val opaquePart' = Save.new ()
      val opaquePart = save (seq [urlcNoSlash, star urlc], opaquePart')
      val hierPart = seq [or [netPath, absPath],
                          optional (seq [char #"?", query])]
      (* netPath occurs before absPath in the following regexp because
       * you want urls like //foo.com/z to be a netPath with host foo.com and 
       * not as an absPath.  Fortunately, the regexp library returns the
       * first matching choice in an or.
       *)
      val relativeUrl =
         seq [or [netPath, absPath, relPath,
                  null (* null added for empty urls -- these are
                        * not in RFC 2396 as far as I can tell, but
                        * some of their examples use them.
                        *)
                  ],
              optional (seq [char #"?", query])]
      val absoluteUrl = seq [scheme, char #":", or [hierPart, opaquePart]]
      val url = seq [optional (or [absoluteUrl, relativeUrl]),
                     optional (seq [char #"#", fragment])]
      val url = Promise.lazy (fn () => compileDFA url)

      fun peekQuery (m: Match.t): string option =
         Option.map (Match.peek (m, query'), fn ss =>
                     let
                        val s = Substring.toString ss
                     in
                        if !escapeQuery
                           then unescape s
                        else s
                     end)

      fun getAbsPath (m: Match.t): Path.t =
         case Match.peek (m, pathSegments') of
            NONE => Error.bug "Url.Regexp.getAbsPath"
          | SOME ss =>
               let
                  val s = Substring.toString ss
                  val (p, f) =
                     List.splitLast
                     (String.fields (unescape s, fn c => c = #"/"))
               in {isAbsolute = true, path = p, file = f}
               end
   end

fun getMatch (m: Regexp.Match.t): t =
   let open Regexp
      val {peek, lookup, exists, ...} = Match.stringFuns m
   in if exists opaquePart'
         then
            let
               val scheme = String.toLower (lookup scheme')
               val rest = unescape (lookup opaquePart')
            in case scheme of
               "javascript" => JavaScript rest
             | "mailto" => MailTo rest
             | "news" => News rest
             | _ => Opaque {scheme = scheme, rest = rest}
            end
      else
         let
            val authority =
               if exists host'
                  then
                     SOME {user = Option.map (peek userinfo', unescape),
                           host = lookup host',
                           port = Option.map (peek port',
                                              valOf o Int.fromString)}
               else NONE
            fun split ss = String.fields (unescape ss, fn c => c = #"/")
            val path =
               case (Option.map (peek relSegment', unescape),
                     Option.map (peek pathSegments', split)) of
                  (NONE, NONE) => NONE
                | (SOME file, NONE) => SOME {isAbsolute = false,
                                             path = [],
                                             file = file}
                | (NONE, SOME ss) =>
                     let val (p, f) = List.splitLast ss
                     in SOME {isAbsolute = true,
                              path = p, file = f}
                     end
                | (SOME s, SOME ss) =>
                     let val (p, f) = List.splitLast ss
                     in SOME {isAbsolute = false,
                              path = s :: p, file = f}
                     end
         in T {scheme = Option.map (peek scheme', Scheme.fromString),
               authority = authority,
               path = path,
               query = peekQuery m,
               fragment = Option.map (peek fragment', unescape)}
         end
   end

fun fromString (urlString: string): t option =
   Option.map (Regexp.Compiled.matchAll (Regexp.url(), urlString), getMatch)

val fromString =
   Trace.trace ("Url.fromString", String.layout, Option.layout layout)
   fromString

fun equals (u: t, u': t): bool = u = u'

val mailto = MailTo
val news = News

(* ------------------------------------------------- *)
(*                    relativize                     *)
(* ------------------------------------------------- *)

fun relativize {base = b, relative = r} =
   case (b, r) of
      (T {scheme = SOME s, authority = SOME a, path = p, ...},
       T {scheme = SOME s', authority = SOME a', path = p', query = q',
          fragment = f'}) =>
      if Scheme.equals (s, s')
         andalso Authority.equals (a, a')
         then let
                 fun some (p, f) =
                    let
                       val (p, f) =
                          case (p, f) of
                             ([], "") => ([], ".")
                           | _ => (p, f)
                    in SOME {isAbsolute = false, path = p, file = f}
                    end
                 val p': Path.t option =
                    case (p, p') of
                       (NONE, NONE) => NONE
                     | (NONE, SOME {path, file, ...}) => some (path, file)
                     | (SOME {path, ...}, NONE) =>
                          some (List.map (path, fn _ => ".."), "")
                     | (SOME {path = p, ...}, SOME {path = p', file, ...}) =>
                          let
                             val (p, p') =
                                List.removeCommonPrefix (p, p', String.equals)

                          in some (List.map (p, fn _ => "..") @ p', file)
                          end
              in SOME (T {scheme = NONE, authority = NONE, path = p', query = q',
                          fragment = f'})
              end
      else NONE
                        | _ => NONE

val relativize =
   Trace.trace ("Url.relativize",
               fn {base = b, relative = r} => Layout.tuple [layout b, layout r],
               Option.layout layout)
   relativize

(* ------------------------------------------------- *)
(*                      resolve                      *)
(* ------------------------------------------------- *)

(* The numbers in comments are rule numbers from Section 5.2 of RFC 2396. *)
fun resolve {base, relative} =
   case (base, relative) of
      (_, T {scheme = SOME _, ...}) => relative (* 3 *)
    | (T {scheme = s, authority = a, path = p, query = q, ...},
       T {authority = a', path = p', query = q', fragment = f', ...}) =>
      let
         val (a, p, q) =
            case (a', p', q') of
               (SOME _, _, _) => (a', p', q') (* 4 *)
             | (_, NONE, NONE) => (a, p, q) (* 2 *)
             | (_, NONE, SOME _) => (* 6 *)
                  let
                     val p =
                        Option.map (p, fn {isAbsolute, path, file} =>
                                   {isAbsolute = isAbsolute,
                                    path = path,
                                    file = ""})
                  in (a, p, q')
                  end
             | (_, SOME {isAbsolute = true, ...}, _) => (a, p', q') (* 5 *)
             | (_, SOME {isAbsolute = false, path = p', file = f'}, _) => (* 6 *)
                  let
                     val (p', f') =
                        case p of
                           NONE => (p', f')
                         | SOME {path, ...} => canonicalizePath (path, p', f')
                  in (a, SOME {isAbsolute = true, path = p', file = f'}, q')
                  end
      in T {scheme = s, authority = a, path = p, query = q, fragment = f'}
      end
     | _ => relative

val resolve =
   Trace.trace
   ("Url.resolve",
    fn {base = b, relative = r} => Layout.tuple [layout b, layout r],
    layout)
   resolve

(* ------------------------------------------------- *)
(*                   canonicalize                    *)
(* ------------------------------------------------- *)

fun canonicalize (u: t): t =
   case u of
      T {scheme, authority, path, query, fragment} =>
         T {scheme = scheme,
           authority = Option.map (authority, Authority.canonicalize),
           path = Option.map (path, Path.canonicalize),
           query = query,
           fragment = fragment}
    | _ => u


end
