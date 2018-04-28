(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Based on RFC 2616. *)
signature HTTP =
   sig
      structure Method:
         sig
            datatype t =
               Connect
             | Delete
             | Get
             | Head
             | Options
             | Post
             | Put
             | Trace
             | Extension of string
         end

      structure RequestUrl:
         sig
            datatype t =
               Star
             | Url of Url.t
             | Path of {path: Url.Path.t,
                        query: string option}
             | Authority of string

            val toString: t -> string
         end

      structure Version:
         sig
            datatype t = T of {major: int,
                               minor: int}

            val v10: t
            val v11: t
         end

      structure Header:
         sig
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

            val fromString: string -> t list Result.t
            val input: In.t -> t list Result.t
            val toString: t -> string
         end

      structure Request:
         sig
            datatype t = T of {method: Method.t,
                               url: RequestUrl.t,
                               version: Version.t,
                               headers: Header.t list}

            val input: In.t -> t Result.t
            val layout: t -> Layout.t
            val output: t * Out.t -> unit
            val regexp: unit -> Regexp.Compiled.t
            val requestLine: string -> {method: Method.t,
                                        url: RequestUrl.t,
                                        version: Version.t} option
            val toString: t -> string
         end

      structure Status:
         sig
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

            val code: t -> string
            val fromString: string -> t (* string is a code, eg "502" *)
            val reason: t -> string
         end

      structure Response:
         sig
            datatype t = T of {headers: Header.t list,
                               status: Status.t,
                               version: Version.t}

            val input: In.t -> t Result.t
            val layout: t -> Layout.t
            val output: t * Out.t -> unit
            val regexp: unit -> Regexp.Compiled.t
            val toString: t -> string
         end

      structure Post:
         sig
            structure Encoding:
               sig
                  datatype t = Url | Multipart
               end

            structure Value:
               sig
                  type t

                  val file: File.t -> t
                  val string: string -> t
               end

            datatype t =
               T of {encoding: Encoding.t,
                     fields: {name: string,
                              value: Value.t} list}
         end

      val fetch:
         {head: bool,
          headers: Header.t list,
          post: Post.t option,
          proxy: {host: string, port: int} option,
          url: Url.t} -> In.t
   end


functor TestHttp (S: HTTP): sig end =
struct

open S

val _ = 
   Assert.assert
   ("TestHttp", fn () =>
    Regexp.Compiled.matchesAll (Request.regexp (),
                                "CONNECT trading.etrade.com:443 HTTP/1.0\r\n")
    andalso
    isSome (Request.requestLine "GET http://Norma140.emp3.com/ HTTP/1.0\n")
    andalso
    let
       val s =
          "Date: Wed, 08 Mar 2000 09:26:18 GMT\r\n\
           \Server: Apache/1.3.6 (Unix)  (Red Hat/Linux)\r\n\
           \Last-Modified: Thu, 02 Mar 2000 22:55:44 GMT\r\n\
           \ETag: \"23a07c-2ae-38bef170\"\r\n\
           \Accept-Ranges: bytes\r\n\
           \Content-Length: 686\r\n\
           \Connection: close\r\n\
           \Content-Type: text/html\r\n"
       val zzz = "GET http://www.nytimes.com/auth/chk_login?is_continue=true&URI=http%3A%2F%2Fwww.nytimes.com%2Flibrary%2Ftech%2Fyr%2Fmo%2Fbiztech%2Farticles%2F17blue.html&Tag=&site=&banner=&sweeps=&USERID=cypherpunk&PASSWORD=cypherpunk&SAVEOPTION=YES HTTP/1.0\r\n"
       val s =
          "Referer: http://www.nytimes.com/auth/chk_login?is_continue=true&URI=http%3A%2F%2Fwww.nytimes.com%2Flibrary%2Ftech%2Fyr%2Fmo%2Fbiztech%2Farticles%2F17blue.html&Tag=&site=&banner=&sweeps=&USERID=hqbovik&PASSWORD=hqbovik&SAVEOPTION=YES\r\nUser-Agent: Mozilla/4.7 [en]\r\nHost: www.nytimes.com\r\nAccept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, */*\r\nAccept-Encoding: gzip\r\nAccept-Language: en\r\nAccept-Charset: iso-8859-1,*,utf-8\r\nCookie: RMID=c603a30338b9ce60; NYT-S=0UtWyAdJ/Hc94BS7pHO0q4Pek6E1oJ.FMxFTIduykzwDgubECS6cqpWk.Duqut/D9GDBO6lz6cXYs0; PW=\161%.69,.)03\223; ID=\161%.69,.)03\223; RDB=C80200D6EF0000555301001E2719270101000000000002\r\n"

       val s = "Cookie: PW=\161%.69\r\n"
    in Result.isYes (Header.fromString s)
    end)

end
