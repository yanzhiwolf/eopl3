(define a-grammar-spec
  '((statement ("{" statement ";" statement "}") compound-statement)
    (statement ("while" expression "do" statement) while-statement)
	(statement (identifier ":=" expression) assign-statement)
	(expression (identifier) var-exp)
	(expression ("(" expression "-" expression ")") diff-exp)))