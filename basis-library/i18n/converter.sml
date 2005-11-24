structure CharsetConverter :> CHARSET_CONVERTER =
   struct
      (* http://mlton.org/UniversalType *)
      type state = exn
      fun 'a embed () =
         let
            exception E of 'a
            fun project (e: t): 'a option =
               case e of
                  E a => SOME a
                | _ => NONE
         in
            (E, project)
         end
      
      type 'a decoder = {
         initial: state,
         decoder:  (Word8.word, 'a) reader -> (WideChar.char, 'a * state) reader
      }
      
      type encoder = {
         initial: state * Word8Vector.vector,
         encoder: state * WideSubtring.substring -> 
                  state * WideSubstring.substring * Word8Vector.vector,
         flush:   state -> Word8Vector.vector
         }
      
      type user_coder = {
         name: string,
         decoder: state decoder,
         encoder: encoder
         }
      
      val coders : user_coder list ref = ref []
      fun register x = coders := x :: (!coders)
      
      structure Encoding =
         struct
            datatype t = 
               UTF8 | UTF16BE | UTF16LE | UTF32BE | UTF32LE |
               USER of user_coder
            
            val equals = op =
            
            fun canonName s =
               String.translate
                  (fn #"_" => ""
                    | #"-" => ""
                    | x => String.str (Char.toUpper x)) s
                  
            fun fromName s = case canonName s of
               "UTF8" => SOME UTF8
             | "UCS2" => SOME UTF16LE
             | "UCS2LE" => SOME UTF16LE
             | "UCS2BE" => SOME UTF16BE
             | "UTF16" => SOME UTF16LE (* guess little-endian for now *)
             | "UTF16LE" => SOME UTF16LE
             | "UTF16BE" => SOME UTF16BE
             | "UCS4" => SOME UTF32LE
             | "UCS4LE" => SOME UTF32LE
             | "UCS4BE" => SOME UTF32BE
             | "UTF32" => SOME UTF32LE (* guess little-endian for now *)
             | "UTF32LE" => SOME UTF32LE
             | "UTF32BE" => SOME UTF32BE
             | s => 
                  case List.find (fn {name, ...} => name = s) (!coders) of
                     NONE => NONE
                   | SOME x => SOME (USER x)
            
            fun toName UTF8 = "UTF-8"
              | toName UTF16BE = "UTF-16BE"
              | toName UTF16LE = "UTF-16LE"
              | toName UTF32BE = "UTF-32BE"
              | toName UTF32LE = "UTF-32LE"
              | toName (User {name, ...}) => name
            
            val utf8 = UTF8
            val utf16be = UTF16BE
            val utf16le = UTF16LE
            val utf32be = UTF32BE
            val utf32le = UTF32LE
         end
      
      fun decode (e, vs) =
         let
            val { initial, decoder } = decoder e
            fun get vs =
               if Word8VectorSlice.length vs = 0 then NONE else
               SOME (Word8VectorSlice.sub (vs, 0), 
                     Word8VectorSlice.subslice (vs, 1, NONE))
         in
            ()
         end
   end
