structure FolLrVals : Fol_LRVALS =
   FolLrValsFun(structure Token = LrParser.Token
                structure Absyn = Absyn);

structure Interface : INTERFACE = Interface();
structure FolLex : LEXER =
   FolLexFun(structure Tokens = FolLrVals.Tokens
             structure Interface = Interface);

structure FolParser : PARSER =
   Join(structure ParserData = FolLrVals.ParserData
        structure Lex = FolLex
	structure LrParser = LrParser);

structure Parse : PARSE =
   Parse (structure Absyn = Absyn
	  structure Interface = Interface
	  structure Parser = FolParser
	  structure Tokens = FolLrVals.Tokens );
