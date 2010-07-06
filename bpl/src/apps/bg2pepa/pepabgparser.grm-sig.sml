signature PepaBg_TOKENS =
sig
type ('a,'b) token
type svalue
val STATE:  'a * 'a -> (svalue,'a) token
val STATELIST:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val ABSENT:  'a * 'a -> (svalue,'a) token
val PRESENT:  'a * 'a -> (svalue,'a) token
val PORT:  'a * 'a -> (svalue,'a) token
val PRNT:  'a * 'a -> (svalue,'a) token
val CTRL:  'a * 'a -> (svalue,'a) token
val ROOT:  'a * 'a -> (svalue,'a) token
val NAME:  'a * 'a -> (svalue,'a) token
val EDGE:  'a * 'a -> (svalue,'a) token
val NODE:  'a * 'a -> (svalue,'a) token
val UUSCORE:  'a * 'a -> (svalue,'a) token
val COLLABSTAR:  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature PepaBg_LRVALS=
sig
structure Tokens : PepaBg_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
