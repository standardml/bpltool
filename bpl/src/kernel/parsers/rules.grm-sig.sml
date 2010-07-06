signature Rules_TOKENS =
sig
type ('a,'b) token
type svalue
val IDP:  'a * 'a -> (svalue,'a) token
val IDW0:  'a * 'a -> (svalue,'a) token
val IDW:  'a * 'a -> (svalue,'a) token
val ONE:  'a * 'a -> (svalue,'a) token
val IDBB0:  'a * 'a -> (svalue,'a) token
val IDX0:  'a * 'a -> (svalue,'a) token
val ATAT:  'a * 'a -> (svalue,'a) token
val AT:  'a * 'a -> (svalue,'a) token
val GTGT:  'a * 'a -> (svalue,'a) token
val LTLT:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val BQUOTE:  'a * 'a -> (svalue,'a) token
val PAR:  'a * 'a -> (svalue,'a) token
val PRI:  'a * 'a -> (svalue,'a) token
val XX:  'a * 'a -> (svalue,'a) token
val OO:  'a * 'a -> (svalue,'a) token
val DASHSLASHSLASH:  'a * 'a -> (svalue,'a) token
val DASHSLASH:  'a * 'a -> (svalue,'a) token
val SLASHSLASH:  'a * 'a -> (svalue,'a) token
val SLASH:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val MERGE:  'a * 'a -> (svalue,'a) token
val INST:  'a * 'a -> (svalue,'a) token
val REACT:  'a * 'a -> (svalue,'a) token
val REDEX:  'a * 'a -> (svalue,'a) token
val NAME:  'a * 'a -> (svalue,'a) token
val EQEQ:  'a * 'a -> (svalue,'a) token
val RRARROW:  'a * 'a -> (svalue,'a) token
val RARROW:  'a * 'a -> (svalue,'a) token
val EQEQCOLON:  'a * 'a -> (svalue,'a) token
val DASHDASHCOLON:  'a * 'a -> (svalue,'a) token
val EQCOLON:  'a * 'a -> (svalue,'a) token
val DASHCOLON:  'a * 'a -> (svalue,'a) token
val MAPPSTO:  'a * 'a -> (svalue,'a) token
val MAPSTO:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val AMP:  'a * 'a -> (svalue,'a) token
val ATOMIC:  'a * 'a -> (svalue,'a) token
val PASSIVE:  'a * 'a -> (svalue,'a) token
val ACTIVE:  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val CTRLID: (string) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val SIGNATURE:  'a * 'a -> (svalue,'a) token
val RULELIST:  'a * 'a -> (svalue,'a) token
val BGTERM:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Rules_LRVALS=
sig
structure Tokens : Rules_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
