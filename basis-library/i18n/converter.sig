signature CHARSET_CONVERTER =
   sig
      structure Encoding:
         sig
            type t

            val equals: t * t -> bool
            
            val fromName: string -> t option
            val toName: t -> string
            
            (* also needed for bare minimum support (in order of usefulness):
            val punycode: t
            val utf7: t
            val gb18030: t
            val cesu8: t
            val scsu: t
            *)
            val utf8: t
            (* the "be" and "le" control endian in the absense of FFFE *)
            val utf16be: t
            val utf16le: t
            val utf32be: t
            val utf32le: t
         end
      
      (* Unfortunately, unlike all of the StringCvt methods provided in the
       * basis, charset encodings can be stateful. For example, consider a
       * fictituous charset consisting of letters A-Z and a 'uppercase'
       * and 'lowercase' char (u & l respectively). Then "BlBBuCB" = "BbbCB".
       * For this reason, decoders need to keep a state in addition to the
       * stream position. Encoders also need to be 'flush'ed at the end of
       * encoding to restore a stateful output stream to the initial state.
       *)
      type state
      val embed: unit -> ('a -> state) * (state -> 'a option)
      
      type 'a decoder = {
         initial: state,
         decoder:  (Word8.word, 'a) reader -> (WideChar.char, 'a * state) reader
      }
      val decoder: Encoding.t -> 'a decoder
      
      (* The encoder will only write up to the first unicode character which
       * cannot be represented in the output charset.
       *)
      type encoder = {
         initial: state * Word8Vector.vector,
         encoder: state * WideSubtring.substring -> 
                  state * WideSubstring.substring * Word8Vector.vector,
         flush:   state -> Word8Vector.vector
         }
      val encoder: Encoding.t -> encoder
      
      (* Convenience functions *)
      val decode: Encoding.t * Word8VectorSlice.vector_slice -> WideString.string option
      val encode: Encoding.t * WideSubstring.substring -> Word8Vector.vector option
      
      (* The register method allows you to add support for new encodings.
       * The name is used case insensitively.
       * The decoder has concrete type "state decoder" to work around SML's
       * lack of higher order types. However, you must not peek inside it.
       *)
      type user_coder = {
         name: string,
         decoder: state decoder,
         encoder: encoder
         }
      val register: user_coder -> unit
   end
