signature Bpl_TOKENS =
sig
type ('a,'b) token
type svalue
val STATE:  'a * 'a -> (svalue,'a) token
val VAL:  'a * 'a -> (svalue,'a) token
val USING:  'a * 'a -> (svalue,'a) token
val SIGNATURE:  'a * 'a -> (svalue,'a) token
val SIG:  'a * 'a -> (svalue,'a) token
val RULE:  'a * 'a -> (svalue,'a) token
val PASSIVE:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val ATOMIC:  'a * 'a -> (svalue,'a) token
val ACTIVE:  'a * 'a -> (svalue,'a) token
val PAR:  'a * 'a -> (svalue,'a) token
val DPAR:  'a * 'a -> (svalue,'a) token
val UNDERSCORE:  'a * 'a -> (svalue,'a) token
val QUOT:  'a * 'a -> (svalue,'a) token
val COMP:  'a * 'a -> (svalue,'a) token
val STAR:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SLASH:  'a * 'a -> (svalue,'a) token
val BARREN:  'a * 'a -> (svalue,'a) token
val RBRK:  'a * 'a -> (svalue,'a) token
val LBRK:  'a * 'a -> (svalue,'a) token
val RBRC:  'a * 'a -> (svalue,'a) token
val LBRC:  'a * 'a -> (svalue,'a) token
val RANG:  'a * 'a -> (svalue,'a) token
val LANG:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val NAME: (string) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Bpl_LRVALS=
sig
structure Tokens : Bpl_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
