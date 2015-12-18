(define a-scaner-spec
	'((white-sp (whitespace) skip)
	  (commet ("%" (arbno (not #\newline))) skip)
	  (identifier (letter (arbno (or letter digit)))) symbol
	  (number (digit (arbno digit)) number)))