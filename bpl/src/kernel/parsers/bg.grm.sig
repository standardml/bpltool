signature BgTerm_TOKENS =
sig
type ('a,'b) token
type svalue
val ATAT:  'a * 'a -> (svalue,'a) token
val AT:  'a * 'a -> (svalue,'a) token
val IDPn: (int) *  'a * 'a -> (svalue,'a) token
val IDW0:  'a * 'a -> (svalue,'a) token
val IDW:  'a * 'a -> (svalue,'a) token
val ONE:  'a * 'a -> (svalue,'a) token
val IDBB0:  'a * 'a -> (svalue,'a) token
val IDX0:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val BQUOTE:  'a * 'a -> (svalue,'a) token
val QUOTE:  'a * 'a -> (svalue,'a) token
val PAR:  'a * 'a -> (svalue,'a) token
val PRI:  'a * 'a -> (svalue,'a) token
val XX:  'a * 'a -> (svalue,'a) token
val OO:  'a * 'a -> (svalue,'a) token
val SLASH:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val MERGEn: (int) *  'a * 'a -> (svalue,'a) token
val CTRLID: (string) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature BgTerm_LRVALS=
sig
structure Tokens : BgTerm_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
