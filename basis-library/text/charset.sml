signature CHARSET =
   sig
      type t = Word32.word
      
      val isAscii: t -> bool
      val isAlpha: t -> bool
      val isAlphaNum: t -> bool
      val isCntrl: t -> bool
      val isDigit: t -> bool
      val isGraph: t -> bool
      val isHexDigit: t -> bool
      val isLower: t -> bool
      val isPrint: t -> bool
      val isSpace: t -> bool
      val isPunct: t -> bool
      val isUpper: t -> bool
      
      val toUpper: t -> t
      val toLower: t -> t
   end

structure Charset :> CHARSET =
   struct
      local
         open Primitive.Word32
      in
         infix 5 >> << andb orb xorb
         type t = Word32.word
         
         (* these are computed by running ./hash *)
         val size = 32768
         val mask = 0wx7FFF
         val shift = 0w14
         val factor = 0w5390
         
         fun hash c = ((c >> shift) * factor + c) andb mask
         
         (* this is computed by running ./parse *)
         val unicodedb = "12345678"
         val udelta = Vector.fromList [ 0w12, 0w23, 0w22 ]
         val ldelta = Vector.fromList [ 0w2, 0w3, 0w5 ]
         
         (* decode the compressed unicode database.
          * Each entry in the resulting table has format:
          *   bits [  0, 21) = the Unicode code point in this bucket
          *   bits [ 21, 28) = the uppercase delta
          *   bits [ 28, 30) = CLASS = LETTER | NUMERAL | CONTROL | PUNCTUATION
          *   bits [ 30, 32) = CASE = UPPER | LOWER | WHITESPACE | (other)
          *)
         val CODEPOINT   = 0wx001FFFFF
         val CLASS       = 0wx30000000
         val LETTER      = 0wx00000000
         val NUMERAL     = 0wx10000000
         val CONTROL     = 0wx20000000
         val PUNCTUATION = 0wx30000000
         val CASE        = 0wxC0000000
         val UPPER       = 0wx00000000
         val LOWER       = 0wx40000000
         val WHITESPACE  = 0wx80000000
         val DELTASHIFT  = 0w21
         val DELTAMASK   = 0wx7F
         val table =
            let
               val a = Array.tabulate (size, fn _ => 0w0)
            in
               Array.vector a
            end
         
         fun fetch c =
            let
               val x = toInt (hash c)
               val v = Vector.sub (table, x)
            in
               if c = v andb CODEPOINT then SOME v else
               (* only 6 unicode chars fail that test, catch them here: *)
               let
                  val v = Vector.sub (table, Primitive.Int.+ (x, 1))
               in
                  if c = v andb CODEPOINT then SOME v else NONE
               end
            end
         
         fun isClass class c = 
            case fetch c of
               NONE => false
             | SOME v => v andb CLASS = class
         
         val isAlpha = isClass LETTER
         val isDigit = isClass NUMERAL
         val isCntrl = isClass CONTROL
         val isPunct = isClass PUNCTUATION
         
         fun isCase cs c =
            case fetch c of
               NONE => false
             | SOME v => v andb CASE = cs
         
         val isUpper = isCase UPPER
         val isLower = isCase LOWER
         val isSpace = isCase WHITESPACE
         
         (* derived methods *)
         fun isAscii c = c < 0w128
         
         (* Both LETTER and NUMERAL have a 0 in bit 28 *)
         fun isAlphaNum c = 
            case fetch c of
               NONE => false
             | SOME v => c andb CONTROL = 0w0
         
         fun isPrint c = 
            case fetch c of
               NONE => false (* complement of control, excludes non-unicode *)
             | SOME v => v andb CLASS <> CONTROL
         
         (* printable, but not whitespace *)
         fun isGraph c = 
            case fetch c of
               NONE => false
             | SOME v => v andb CLASS <> CONTROL andalso v andb CASE <> WHITESPACE
         
         fun isHexDigit c = false (* !!! damn !!! *)
         
         (* Use the delta tables to convert case 
          * We exploit the fact that a character cannot have both
          * uppercase and lowercase mappings simultaneously.
          *)
         fun delta v = toInt (v >> DELTASHIFT andb DELTAMASK)
         fun toLower c =
            case fetch c of
               NONE => c
             | SOME v => c + Vector.sub (ldelta, delta v)
         fun toUpper c =
            case fetch c of
               NONE => c
             | SOME v => c + Vector.sub (udelta, delta v)
      end
   end
