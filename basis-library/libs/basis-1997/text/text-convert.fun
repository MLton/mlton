functor TextConvert
        (structure Text: TEXT) :
	sig
	   structure Char: CHAR
	   structure String: STRING_1997
	   structure Substring: SUBSTRING_1997
           sharing type Char.char = String.Char.char = Substring.String.Char.char
	   sharing type String.string = Substring.String.string
	end =
  struct
     structure C =
        struct
	   open Char
	end
     structure S =
        struct
	   structure Char = C
	   open String
	end
     structure SS =
        struct
	   structure String = S
	   open Substring
	   val all = full
	end
     structure Char = C
     structure String = S
     structure Substring = SS
  end