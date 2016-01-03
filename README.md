# eopl3

In general, a define-datatype declaration has the form:
	(define-datatype type-name type-predicate-name
		{(variant-name {(field-name predicate)}*)}+)


The general syntax of cases is:
	(cases type-name expression
		{(variant-name ({field-name}*) consequent)}*
		(else default))
	
	
A scanner specification in SLLGEN is a list that satisfies this grammers:
	Scanner-spec      ::= ({Regexp-and-action}*)
	Regexp-and-action ::= (Name ({Regexp}*) Action)
	Name              ::= Symbol
	Regexp            ::= String | letter | digit | whitespace | any
			  ::= (not Character) | (or {Regexp}*)
                      ::= (arbno Regexp) | (concat {Regexp}*)
	Action            ::= skip | symbol | number | string


A grammar in SLLGEN is a list described by the following grammar:
	Grammar    ::= ({Production}*)
	Production ::= (Lhs ({Rhs-item}*) Prod-name)
	Lhs    	   ::= Symbol
	Rhs-item   ::= Symbol | String
		   ::= (arbno {Rhs-item}*)
		   ::= (separated-list {Rhs-item}* String)
	Prod-name  ::= Symbol
