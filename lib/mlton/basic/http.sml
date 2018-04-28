(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Http: HTTP =
struct

structure Regexp =
   struct
      open Regexp

      val CHAR = ascii
      val UPALPHA = isChar Char.isUpper
      val LOALPHA = isChar Char.isLower
      val ALPHA = isChar Char.isAlpha
      val DIGIT = isChar Char.isDigit
      val CTL = isChar Char.isCntrl
      val CR = char #"\r"
      val LF = char #"\n"
      val SP = char #" "
      val HT = char #"\t"
      val CRLF = string "\r\n"
      (* #"\n" is not in the spec for CRLF, but Netscape generates it. *)        
      val CRLF = or [CRLF, char #"\n"]
      val LWS = seq [optional CRLF, oneOrMore (or [SP, HT])]
      val TEXT = isNotChar Char.isCntrl
      val HEX = isChar Char.isHexDigit
      val separatorChars = "()<>@,;:\\\"/ []?= {} \t"
      val separators = oneOf separatorChars
      val token =
         oneOrMore (isChar
                    (Char.memoize
                     (fn c =>
                      Char.isAscii c
                      andalso not (Char.isCntrl c)
                      andalso not (String.contains (separatorChars, c)))))
      val dquote = char Char.dquote
      val qdtext = isChar (fn c =>
                          not (Char.isCntrl c)
                          andalso not (c = Char.dquote))
      val quotedpair = seq [char #"\\", CHAR]
      val quotedstring =
         seq [dquote, star (or [qdtext, quotedpair]), dquote]
      val ctext =
         isChar (fn c =>
                not (Char.isCntrl c)
                andalso not (c = #"(")
                andalso not (c = #")"))
      (*         val comment =
       *            seq [char #"(",
       *                star (or [ctext, quoted-pair, comment]),
       *                char #")"]
       *)
      val major' = Save.new ()
      val minor' = Save.new ()
      val version =
         seq [string "HTTP/",
              save (oneOrMore DIGIT, major'),
              char #".",
              save (oneOrMore DIGIT, minor')]
      val fieldname' = Save.new ()
     val fieldname =
        (* fieldname should just be token, but the stupid Microsoft server
         * includes spaces in its "Content Location" field, so we need to accept
         * more. The easies thing is just to take anything but a ":".
         *)
        (* save (token, fieldname') *)
        save (star (notChar #":"), fieldname')
      val fieldcontent =
         (* fieldcontent should just be TEXT, but nytimes stores control
          * characters in cookies, and thus, need to allow more.
          *)
         (*TEXT *)
         isChar (fn c => c >= #" ")
      val fieldvalue' = Save.new ()
      val fieldvalue = star (or [fieldcontent, LWS])
      val messageheader =
         Promise.lazy
         (fn () =>
          compileDFA (seq [fieldname, char #":",
                           save (optional fieldvalue, fieldvalue'),
                           CRLF]))
      val method' = Save.new ()
      val method = save (token, method')
      val star' = Save.new ()
      val absoluteUrl' = Save.new ()
      val absPath' = Save.new ()
      val authority' = Save.new ()
      val query' = Save.new ()
      val requestUrl =
         let open Url.Regexp
         in or [save (char #"*", star'),
                save (absoluteUrl, absoluteUrl'),
                seq [save (absPath, absPath'),
                     optional (seq [char #"?", save (query, query')])],
                save (authority, authority')]
         end
      val requestLine =
         Promise.lazy
         (fn () =>
          compileDFA (seq [method, SP, requestUrl, SP, version, CRLF]))
      val contentLength =
         Promise.lazy (fn () => compileDFA (oneOrMore DIGIT))
      val status' = Save.new ()
      val status = save (seq [DIGIT, DIGIT, DIGIT], status')
      val reason =
         star (isChar (fn c =>
                     Char.isPrint c andalso c <> #"\r" andalso c <> #"\n"))
      val responseLine =
         Promise.lazy
         (fn () => compileDFA (seq [version, SP, status, SP, reason, CRLF]))
   end

structure Method =
   struct
      datatype t =
         Connect
       | Delete
       | Extension of string
       | Get
       | Head
       | Options
       | Post
       | Put
       | Trace

      val map =
         [(Connect, "CONNECT"),
          (Delete, "DELETE"),
          (Get, "GET"),
          (Head, "HEAD"),
          (Options, "OPTIONS"),
          (Post, "POST"),
          (Put, "PUT"),
          (Trace, "TRACE")]

      fun fromString s =
         case List.peek (map, fn (_, s') => s = s') of
            NONE => Extension s
          | SOME (h, _) => h

      fun toString h =
         case h of
            Extension s => s
          | _ => #2 (valOf (List.peek (map, fn (h', _) => h = h')))

      val layout = Layout.str o toString
   end

structure Version =
   struct
      datatype t = T of {major: int,
                         minor: int}

      fun toString (T {major, minor}) =
         concat ["HTTP/",
                Int.toString major,
                ".",
                Int.toString minor]

      val layout = Layout.str o toString

      val v10 = T {major = 1, minor = 0}
      val v11 = T {major = 1, minor = 1}

      fun extract m =
         T (let
              open Regexp
              fun int s = valOf (Int.fromString (Substring.toString
                                                 (Match.lookup (m, s))))
           in {minor = int minor',
               major = int major'}
           end)
   end

structure RequestUrl =
   struct
      structure Path = Url.Path
      datatype t =
         Star
       | Url of Url.t
       | Path of {path: Path.t,
                  query: string option}
       | Authority of string

      val toString =
         fn Star => "*"
          | Url url => Url.toString url
          | Path {path, query} =>
               concat [Path.toString path,
                      case query of
                         NONE => ""
                       | SOME q => concat ["?", if !Url.escapeQuery
                                                  then Url.escape q
                                               else q]]
          | Authority s => s

      val layout = Layout.str o toString
   end

exception ParseError

structure Header =
   struct
      datatype t =
         Accept of string
       | AcceptCharset of string
       | AcceptEncoding of string
       | AcceptLanguage of string
       | AcceptRanges of string
       | Age of string
       | Allow of string
       | Authorization of string
       | CacheControl of string
       | Connection of string
       | ContentEncoding of string
       | ContentLanguage of string
       | ContentLength of int
       | ContentLocation of string
       | ContentMD5 of string
       | ContentRange of string
       | ContentType of string
       | Cookie of string
       | Date of string
       | ETag of string
       | Expect of string
       | Expires of string
       | Extension of {name: string, value: string}
       | From of string
       | Host of string
       | IfMatch of string
       | LastModified of string
       | Location of string
       | Pragma of string
       | ProxyAuthenticate of string
       | ProxyConnection of string
       | Referer of string
       | RetryAfter of string
       | Server of string
       | SetCookie of string
       | Trailer of string
       | TransferEncoding of string
       | Upgrade of string
       | UserAgent of string
       | Vary of string
       | Via of string
       | WWWAuthenticate of string
       | Warning of string

      val toString =
         fn Accept s => concat ["Accept: ", s]
          | AcceptCharset s => concat ["Accept-Charset: ", s]
          | AcceptEncoding s => concat ["Accept-Encoding: ", s]
          | AcceptLanguage s => concat ["Accept-Language: ", s]
          | AcceptRanges s => concat ["Accept-Ranges: ", s]
          | Age s => concat ["Age: ", s]
          | Allow s => concat ["Allow: ", s]
          | Authorization s => concat ["Authorization: Basic ", Base64.encode s]
          | CacheControl s => concat ["Cache-Control: ", s]
          | Connection s => concat ["Connection: ", s]
          | ContentEncoding s => concat ["Content-Encoding: ", s]
          | ContentLanguage s => concat ["Content-Language: ", s]
          | ContentLength s => concat ["Content-Length: ", Int.toString s]
          | ContentLocation s => concat ["Content-Location: ", s]
          | ContentMD5 s => concat ["Content-MD5: ", s]
          | ContentRange s => concat ["Content-Range: ", s]
          | ContentType s => concat ["Content-Type: ", s]
          | Cookie s => concat ["Cookie: ", s]
          | Date s => concat ["Date: ", s]
          | ETag s => concat ["Etag: ", s]
          | Expect s => concat ["Expect: ", s]
          | Expires s => concat ["Expires: ", s]
          | Extension {name, value} => concat [name, ": ", value]
          | From s => concat ["From: ", s]
          | Host s => concat ["Host: ", s]
          | IfMatch s => concat ["If-Match: ", s]
          | LastModified s => concat ["Last-Modified: ", s]
          | Location s => concat ["Location: ", s]
          | Pragma s => concat ["Pragma: ", s]
          | ProxyAuthenticate s => concat ["Proxy-Authenticate: ", s]
          | ProxyConnection s => concat ["Proxy-Connection: ", s]
          | Referer s => concat ["Referer: ", s]
          | RetryAfter s => concat ["Retry-After: ", s]
          | Server s => concat ["Server: ", s]
          | SetCookie s => concat ["Set-Cookie: ", s]
          | Trailer s => concat ["Trailer: ", s]
          | TransferEncoding s => concat ["Transfer-Encoding: ", s]
          | Upgrade s => concat ["Upgrade: ", s]
          | UserAgent s => concat ["User-Agent: ", s]
          | Vary s => concat ["Vary: ", s]
          | Via s => concat ["Via: ", s]
          | WWWAuthenticate s => concat ["WWW-Authenticate: ", s]
          | Warning s => concat ["Warning: ", s]

      val layout = Layout.str o toString

      fun toStrings (hs: t list): string list =
         List.concatMap (hs, fn h => [toString h, "\r\n"])

      val cons: string -> string -> t option =
         String.memoizeList
         (fn s => fn s' => SOME (Extension {name = s, value = s'}),
          [("accept", SOME o Accept),
           ("accept-charset", SOME o AcceptCharset),
           ("accept-encoding", SOME o AcceptEncoding),
           ("accept-language", SOME o AcceptLanguage),
           ("accept-ranges", SOME o AcceptRanges),
           ("age", SOME o Age),
           ("allow", SOME o Allow),
           ("authorization",
            let
               open Regexp
               val enc = Save.new ()
               val reg = compileNFA (seq [string "Basic ", save (anys, enc)])
            in
               fn s =>
               let
               in Option.map
                  (Compiled.matchAll (reg, s), fn m =>
                   Authorization
                   (Base64.decode (Match.lookupString (m, enc))))
               end
            end),
           ("cache-control", SOME o CacheControl),
           ("connection", SOME o Connection),
           ("content-encoding", SOME o ContentEncoding),
           ("content-language", SOME o ContentLanguage),
           ("content-length",
            fn (s: string) =>
            let open Regexp
            in if Regexp.Compiled.matchesAll (contentLength (), s)
                  then Option.map (Int.fromString s, ContentLength)
               else NONE
            end),
           ("content-location", SOME o ContentLocation),
           ("content-md5", SOME o ContentMD5),
           ("content-range", SOME o ContentRange),
           ("content-type", SOME o ContentType),
           ("cookie", SOME o Cookie),
           ("date", SOME o Date),
           ("etag", SOME o ETag),
           ("expect", SOME o Expect),
           ("expires", SOME o Expires),
           ("from", SOME o From),
           ("host", SOME o Host),
           ("if-match", SOME o IfMatch),
           ("last-modified", SOME o LastModified),
           ("location", SOME o Location),
           ("pragma", SOME o Pragma),
           ("proxy-authenticate", SOME o ProxyAuthenticate),
           ("proxy-connection", SOME o ProxyConnection),
           ("referer", SOME o Referer),
           ("retry-after", SOME o RetryAfter),
           ("server", SOME o Server),
           ("set-cookie", SOME o SetCookie),
           ("trailer", SOME o Trailer),
           ("transfer-encoding", SOME o TransferEncoding),
           ("upgrade", SOME o Upgrade),
           ("user-agent", SOME o UserAgent),
           ("vary", SOME o Vary),
           ("via", SOME o Via),
           ("www-authenticate", SOME o WWWAuthenticate),
           ("warning", SOME o Warning)])

      fun fromString (s: string): t list Result.t =
         let
            val no = Result.No (concat ["invalid header: ", s])
            val n = String.size s
            fun loop (i: int, ac: t list) =
               if i = n
                  then Result.Yes (rev ac)
               else let open Regexp
                    in case Compiled.matchLong (messageheader (), s, i) of
                       NONE => no
                     | SOME m => 
                          let
                             val {lookup, ...} = Match.stringFuns m
                             val fieldname = String.toLower (lookup fieldname')
                             val fieldvalue =
                                String.dropl (lookup fieldvalue', Char.isSpace)
                          in case cons fieldname fieldvalue of
                             NONE => no
                           | SOME header =>
                                loop (i + Match.length m, header :: ac)
                          end
                    end
         in loop (0, [])
         end

      val fromString =
         Trace.trace ("Http.Header.fromString",
                      String.layout,
                      Result.layout (List.layout layout))
         fromString

      fun input (ins: In.t): t list Result.t =
         let
            fun loop (headers: string list): string list =
               case In.inputLine ins of
                  NONE => headers
                | SOME l => 
                     if l = "\r\n"
                        then headers
                     else loop (l :: headers)
         in
            fromString (concat (rev (loop [])))
         end
   end

structure Request =
   struct
      datatype t = T of {method: Method.t,
                         url: RequestUrl.t,
                         version: Version.t,
                         headers: Header.t list}

      val regexp = Regexp.requestLine

      fun toString (T {method, url, version, headers}) =
         concat ([Method.toString method,
                 " ",
                 RequestUrl.toString url,
                 " ",
                 Version.toString version,
                 "\r\n"]
                @ Header.toStrings headers
                @ ["\r\n"])

      val layout = Layout.str o toString

      fun output (r, out) = Out.output (out, toString r)

      fun requestLine (s: string) =
         let
            open Regexp
         in Option.map
            (Compiled.matchAll (requestLine (), s), fn m =>
             let
                val {peek, lookup, exists, ...} = Match.stringFuns m
                val method = Method.fromString (lookup method')
                open RequestUrl
                val url =
                   if exists star'
                      then Star
                   else if exists absoluteUrl'
                           then Url (Url.getMatch m)
                        else
                           (case peek authority' of
                               NONE =>
                                  Path {path = Url.Regexp.getAbsPath m,
                                       query = Url.Regexp.peekQuery m}
                             | SOME s => Authority s)
                val version = Version.extract m
             in {method = method,
                 url = url,
                 version = version}
             end)
         end

      val requestLine =
         Trace.trace ("Http.Request.requestLine",
                      String.layout,
                      Option.layout (fn {method, url, version} =>
                                     Layout.record
                                     [("method", Method.layout method),
                                      ("url", RequestUrl.layout url),
                                      ("version", Version.layout version)]))
         requestLine

      val requestIsValid = Option.isSome o requestLine

      fun input (ins: In.t): t Result.t =
         case In.inputLine ins of
            NONE => Result.No ""
          | SOME l =>
               case requestLine l of
                  NONE => Result.No l
                | SOME {method, url, version} =>
                     Result.map
                     (Header.input ins, fn hs =>
                      T {method = method,
                         url = url,
                         version = version,
                         headers = hs})

      val input =
         Trace.trace ("Http.Request.input", In.layout, Result.layout layout) input
   end

structure Rope =
   struct
      datatype t =
         Appends of t list
       | File of File.t
       | String of string

      val appends = Appends
      val file = File
      val string = String

      val empty = String ""

      fun sizePlus (r: t, ac: int): int =
         case r of
            Appends rs => List.fold (rs, ac, sizePlus)
          | File f => ac + Position.toInt (File.size f)
          | String s => ac + String.size s

      fun size (r: t): int = sizePlus (r, 0)

      fun toStrings (r: t, ac: string list): string list =
         case r of
            Appends rs => List.fold (rev rs, ac, toStrings)
          | File f => File.contents f :: ac
          | String s => s :: ac

      fun toString (r: t): string = concat (toStrings (r, []))

      fun output (r: t, out: Out.t): unit =
         let
            fun loop (r: t): unit =
               case r of
                  Appends rs => List.foreach (rs, loop)
                | File f => File.outputContents (f, out)
                | String s => Out.output (out, s)
         in
            loop r
         end
   end

structure Post =
   struct
      structure Encoding =
         struct
            datatype t = Url | Multipart
         end

      structure Value =
         struct
            datatype t =
               File of File.t
             | String of string

            val file = File
            val string = String

            fun toString (v: t): string =
               case v of
                  File f => File.contents f
                | String s => s

            fun toRope (v: t): Rope.t =
               case v of
                  File f => Rope.file f
                | String s => Rope.string s
         end

      datatype t =
         T of {encoding: Encoding.t,
               fields: {name: string,
                        value: Value.t} list}

      fun dquote s = concat ["\"", s, "\""]

      fun encode (T {encoding, fields}): {contentType: string} * Rope.t =
         case encoding of
            Encoding.Url =>
               ({contentType = "application/x-www-form-urlencoded"},
                List.fold
                (rev fields, Rope.empty, fn ({name, value}, r) =>
                 let
                    val value =
                       String.translate
                       (Value.toString value, fn c =>
                        if Char.isAlphaNum c
                           then Char.toString c
                        else
                           (case c of
                               #" " => "+"
                             | #"\n" => "%0D%0A"
                             | _ => Url.Char.escapeHex c))
                 in
                    Rope.appends [Rope.string (concat [name, "="]),
                                  Rope.string value,
                                  Rope.string "&",
                                  r]
                 end))
          | Encoding.Multipart =>
               let
                  val boundary =
                     String.tabulate
                     (56, fn i =>
                      if i < 28 then #"-" else Random.charFrom "0123456789")
               in
                  ({contentType = concat ["multipart/form-data; boundary=",
                                          boundary]},
                   List.foldr
                   (fields, Rope.string (concat ["--", boundary, "--"]),
                    fn ({name, value}, rope) =>
                    let
                       val extra =
                          case value of
                             Value.File f => concat ["; filename=", dquote f]
                           | Value.String _ => ""
                    in
                       Rope.appends
                       [Rope.string
                        (concat
                         ["--", boundary, "\r\n",
                          "Content-Disposition: form-data; name=", dquote name,
                          extra, "\r\n\r\n"]),
                        Value.toRope value, Rope.string "\r\n", rope]
                    end))
             end
   end

(* ------------------------------------------------- *)
(*                       fetch                       *)
(* ------------------------------------------------- *)

structure Path = Url.Path

fun fetch {head: bool,
           headers: Header.t list,
           post: Post.t option,
           proxy: {host: string, port: int} option,
           url: Url.t}: In.t =
   let
      open Url
   in
      case url of
         Url.T {authority = SOME {user, host, port},
                fragment, path, query,
                scheme = SOME Scheme.Http} =>
         let
            val headers = Header.Host host :: headers
            val (method, headers, postit) =
               case post of
                  NONE =>
                     (if head then Method.Head else Method.Get,
                         headers,
                         fn _ => ())
                | SOME post =>
                     let
                        datatype z = datatype Post.Encoding.t
                        val ({contentType}, rope) = Post.encode post
                        val headers =
                           headers
                           @ [Header.ContentType contentType,
                              Header.ContentLength (Rope.size rope)]
                     in
                        (Method.Post, headers,
                         fn out => (Rope.output (rope, out)
                                    ; Out.output (out, "\r\n")))
                     end
            val (scheme, authority) =
               if Option.isSome proxy
                  then (SOME Scheme.Http,
                        SOME {user = NONE,
                              host = host,
                              port = port})
               else (NONE, NONE)
            val url =
               Url.T {scheme = scheme,
                      authority = authority,
                      path = path,
                      query = query,
                      fragment = NONE}
            val headers =
               case user of
                  NONE => headers
                | SOME user => Header.Authorization user :: headers
            val request =
               Request.T {method = method,
                          url = RequestUrl.Url url,
                          version = Version.v10,
                          headers = headers}
            val (ins, out) =
               Net.connect (case proxy of
                               NONE => {host = host,
                                        port = (case port of
                                                   NONE => 80
                                                 | SOME p => p)}
                             | SOME hp => hp)
            val print = Out.outputc out
            val () = Request.output (request, out)
            val () = postit out
            val () = Out.close out
         in ins
         end
              | _ => Error.bug (concat ["Htt.fetch: ", Url.toString url])
   end

val fetch =
   Trace.trace ("Http.fetch", fn {url, ...} => Url.layout url, Layout.ignore)
   fetch

(* ------------------------------------------------- *)
(*                      Status                       *)
(* ------------------------------------------------- *)

structure Status =
   struct
      datatype t =
         Accepted
       | BadGateway
       | BadRequest
       | Conflict
       | Continue
       | Created
       | ExpectationFailed
       | Extension of string
       | Forbidden
       | Found
       | GatewayTimeout
       | Gone
       | HTTPVersionNotSupported
       | InternalServerError
       | LengthRequired
       | MethodNotAllowed
       | MovedPermanently
       | MultipleChoices
       | NoContent
       | NonAuthoritativeInformation
       | NotAcceptable
       | NotFound
       | NotImplemented
       | NotModified
       | OK
       | PartialContent
       | PaymentRequired
       | PreconditionFailed
       | ProxyAuthenticationRequired
       | RequestEntityTooLarge
       | RequestTimeout
       | RequestUriTooLarge
       | RequestedRangeNotSatisfiable
       | ResetContent
       | SeeOther
       | ServiceUnavailable
       | SwitchingProtocols
       | TemporaryRedirect
       | Unauthorized
       | UnsupportedMediaType
       | UseProxy

      val all =
         [(Continue, "100", "Continue"),
          (SwitchingProtocols, "101", "Switching Protocols"),
          (OK, "200", "OK"),
          (Created, "201", "Created"),
          (Accepted, "202", "Accepted"),
          (NonAuthoritativeInformation, "203", "Non-Authoritative Information"),
          (NoContent, "204", "No Content"),
          (ResetContent, "205", "Reset Content"),
          (PartialContent, "206", "Partial Content"),
          (MultipleChoices, "300", "Multiple Choices"),
          (MovedPermanently, "301", "Moved Permanently"),
          (Found, "302", "Found"),
          (SeeOther, "303", "See Other"),
          (NotModified, "304", "Not Modified"),
          (UseProxy, "305", "Use Proxy"),
          (TemporaryRedirect, "307", "Temporary Redirect"),
          (BadRequest, "400", "Bad Request"),
          (Unauthorized, "401", "Unauthorized"),
          (PaymentRequired, "402", "Payment Required"),
          (Forbidden, "403", "Forbidden"),
          (NotFound, "404", "Not Found"),
          (MethodNotAllowed, "405", "Method Not Allowed"),
          (NotAcceptable, "406", "Not Acceptable"),
          (ProxyAuthenticationRequired, "407", "Proxy Authentication Required"),
          (RequestTimeout, "408", "Request Time-out"),
          (Conflict, "409", "Conflict"),
          (Gone, "410", "Gone"),
          (LengthRequired, "411", "Length Required"),
          (PreconditionFailed, "412", "Precondition Failed"),
          (RequestEntityTooLarge, "413", "Request Entity Too Large"),
          (RequestUriTooLarge, "414", "Request-URI Too Large"),
          (UnsupportedMediaType, "415", "Unsupported Media Type"),
          (RequestedRangeNotSatisfiable, "416",
           "Requested range not satisfiable"),
          (ExpectationFailed, "417", "Expectation Failed"),
          (InternalServerError, "500", "Internal Server Error"),
          (NotImplemented, "501", "Not Implemented"),
          (BadGateway, "502", "Bad Gateway"),
          (ServiceUnavailable, "503", "Service Unavailable"),
          (GatewayTimeout, "504", "Gateway Time-out"),
          (HTTPVersionNotSupported, "505", "HTTP Version not supported")]

      val all =
         List.revMap (all, fn (status, code, reason) =>
                      {status = status,
                       code = code,
                       reason = reason})

      fun fromString s =
         case List.peek (all, fn {code, ...} => s = code) of
            NONE => Extension s
          | SOME {status, ...} => status

      local
         fun make (ext, sel) (s: t) =
            case s of
               Extension c => ext c
             | _ => sel (valOf (List.peek (all, fn {status, ...} => s = status)))
      in
         val code = make (fn c => c, #code)
         val reason = make (fn _ => "Extension Status Code - No Reason",
                            #reason)
      end
   end

(* ------------------------------------------------- *)
(*                     Response                      *)
(* ------------------------------------------------- *)

structure Response =
   struct
      datatype t = T of {version: Version.t,
                         status: Status.t,
                         headers: Header.t list}

      val regexp = Regexp.responseLine

      fun toString (T {version, status, headers}) =
         concat ([Version.toString version, " ",
                  Status.code status, " ",
                  Status.reason status, "\r\n"]
                 @ Header.toStrings headers
                 @ ["\r\n"])

      val layout = Layout.str o toString

      fun output (r, out) = Out.output (out, toString r)

      fun input (ins: In.t): t Result.t =
         case In.inputLine ins of
            NONE => Result.No ""
          | SOME l => 
               let 
                  open Regexp
               in
                  case Compiled.matchAll (responseLine (), l) of
                     NONE => Result.No l
                   | SOME m => 
                        let
                           val {lookup, ...} = Match.stringFuns m
                           val version = Version.extract m
                           val status = Status.fromString (lookup status')
                        in
                           Result.map (Header.input ins, fn hs =>
                                       T {version = version,
                                          status = status,
                                          headers = hs})
                        end
               end
   end

end
