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
      (* 	 val comment =
       * 	    seq [char #"(",
       * 		star (or [ctext, quoted-pair, comment]),
       * 		char #")"]
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
	 compileDFA (seq [fieldname, char #":",
			  save (optional fieldvalue, fieldvalue'),
			  CRLF])
      val method' = Save.new ()
      val method = save (token, method')
      val star' = Save.new ()
      val absoluteUri' = Save.new ()
      val absPath' = Save.new ()
      val authority' = Save.new ()
      val query' = Save.new ()
      val requestUri = let open Uri.Regexp
		       in or [save (char #"*", star'),
			     save (absoluteUri, absoluteUri'),
			     seq [save (absPath, absPath'),
				 optional (seq [char #"?", save (query, query')])],
			     save (authority, authority')]
		       end
      val requestLine =
	 compileDFA (seq [method, SP, requestUri, SP, version, CRLF])
      val contentLength = compileDFA (oneOrMore DIGIT)
      val status' = Save.new ()
      val status = save (seq [DIGIT, DIGIT, DIGIT], status')
      val reason =
	 star (isChar (fn c =>
		     Char.isPrint c andalso c <> #"\r" andalso c <> #"\n"))
      val responseLine =
	 compileDFA (seq [version, SP, status, SP, reason, CRLF])
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
	  | _ => #2 (List.lookup (map, fn (h', _) => h = h'))

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

      fun extract m =
	 T (let
	      open Regexp
	      fun int s = valOf (Int.fromString (Substring.toString
						 (Match.lookup (m, s))))
	   in {minor = int minor',
	       major = int major'}
	   end)
   end

structure RequestUri =
   struct
      structure Path = Uri.Path
      datatype t =
	 Star
       | Uri of Uri.t
       | Path of {path: Path.t,
		  query: string option}
       | Authority of string

      val toString =
	 fn Star => "*"
	  | Uri uri => Uri.toString uri
	  | Path {path, query} =>
	       concat [Path.toString path,
		      case query of
			 NONE => ""
		       | SOME q => concat ["?", if !Uri.escapeQuery
						  then Uri.escape q
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
       | Extension of string * string
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
	  | Extension (s, s') => concat [s, ": ", s']
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
	 (fn s => fn s' => SOME (Extension (s, s')),
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
	    in if Regexp.Compiled.matchesAll (contentLength, s)
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
		    in case Compiled.matchLong (messageheader, s, i) of
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
	 Trace.trace ("Header.fromString",
		      String.layout,
		      Result.layout (List.layout layout))
	 fromString

      fun input (ins: In.t): t list Result.t =
	 let
	    fun loop (headers: string list): string list =
	       let val line = In.inputLine ins
	       in if line = "\r\n" orelse line = ""
		     then headers
		  else loop (line :: headers)
	       end
	 in fromString (concat (rev (loop [])))
	 end
   end

structure Request =
   struct
      datatype t = T of {method: Method.t,
			 uri: RequestUri.t,
			 version: Version.t,
			 headers: Header.t list}

      val regexp = Regexp.requestLine
	 
      fun toString (T {method, uri, version, headers}) =
	 concat ([Method.toString method,
		 " ",
		 RequestUri.toString uri,
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
	    (Compiled.matchAll (requestLine, s), fn m =>
	     let
		val {peek, lookup, exists, ...} = Match.stringFuns m
		val method = Method.fromString (lookup method')
		open RequestUri
		val uri =
		   if exists star'
		      then Star
		   else if exists absoluteUri'
			   then Uri (Uri.getMatch m)
			else
			   (case peek authority' of
			       NONE =>
				  Path {path = Uri.Regexp.getAbsPath m,
				       query = Uri.Regexp.peekQuery m}
			     | SOME s => Authority s)
		val version = Version.extract m
	     in {method = method,
		 uri = uri,
		 version = version}
	     end)
	 end

      val requestLine =
	 Trace.trace ("requestLine",
		      String.layout,
		      Option.layout (fn {method, uri, version} =>
				     Layout.record
				     [("method", Method.layout method),
				      ("uri", RequestUri.layout uri),
				      ("version", Version.layout version)]))
	 requestLine

      val requestIsValid = Option.isSome o requestLine
	 
      fun input (ins: In.t): t Result.t =
	 let val line = In.inputLine ins
	 in case requestLine line of
	    NONE => Result.No line
	  | SOME {method, uri, version} =>
	       Result.map
	       (Header.input ins, fn hs =>
		T {method = method,
		  uri = uri,
		  version = version,
		  headers = hs})
	 end

      val input =
	 Trace.trace ("Request.input", In.layout, Result.layout layout) input
   end

(* ------------------------------------------------- *)
(*                       fetch                       *)
(* ------------------------------------------------- *)

structure Path = Uri.Path
   
fun fetch {uri: Uri.t,
	  headers: Header.t list,
	  head: bool,
	  post: string option,
	  proxy: {host: string, port: int} option}: In.t =
   let open Uri
   in case uri of
      Uri.T {scheme = SOME Scheme.Http,
	    authority = SOME {user, host, port},
	    path, query, fragment} =>
	 let
	    val headers = Header.Host host :: headers
	    val (method, headers) =
	       case post of
		  NONE => (if head then Method.Head else Method.Get, headers)
		| SOME s =>
		     (Method.Post,
		      Header.ContentType "application/x-www-form-urlencoded"
		      :: Header.ContentLength (String.size s)
		      :: headers)
	    val (scheme, authority) =
	       if Option.isSome proxy
		  then (SOME Scheme.Http,
			SOME {user = NONE,
			     host = host,
			     port = port})
	       else (NONE, NONE)
	    val uri =
	       Uri.T {scheme = scheme,
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
			 uri = RequestUri.Uri uri,
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
	    val _ = Request.output (request, out)
	    val _ = (case post of
			NONE => ()
		      | SOME s => (print s; print "\r\n"))
	    val _ = Out.close out
	 in ins
	 end
    | _ => Error.bug (concat ["can't fetch Uri: ", Uri.toString uri])
   end

val fetch =
   Trace.trace
   ("Http.fetch",
    fn {uri, ...} => Uri.layout uri,
    Layout.ignore)
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
	     | _ => sel (List.lookup (all, fn {status, ...} => s = status))
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
	 let
	    val line = In.inputLine ins
	    open Regexp
	 in
	    case Compiled.matchAll (responseLine, line) of
	       NONE => Result.No line
	     | SOME m => 
		  let val {lookup, ...} = Match.stringFuns m
		     val version = Version.extract m
		     val status = Status.fromString (lookup status')
		  in Result.map (Header.input ins, fn hs =>
				 T {version = version,
				    status = status,
				    headers = hs})
		  end
	 end
   end

end
