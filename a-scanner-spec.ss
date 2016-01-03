;A scanner specification in SLLGEN is a list that satisfies this grammers:
;  Scanner-spec      ::= ({Regexp-and-action}*)
;  Regexp-and-action ::= (Name ({Regexp}*) Action)
;  Name              ::= Symbol
;  Regexp            ::= String | letter | digit | whitespace | any
;                    ::= (not Character) | (or {Regexp}*)
;                    ::= (arbno Regexp) | (concat {Regexp}*)
;  Action            ::= skip | symbol | number | string

(define a-scaner-spec
	'((white-sp (whitespace) skip)
	  (commet ("%" (arbno (not #\newline))) skip)
	  (identifier (letter (arbno (or letter digit))) symbol)
	  (number (digit (arbno digit)) number)))