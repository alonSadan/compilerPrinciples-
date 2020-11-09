1. <lparen> → <space>* '(' <space>*
2. <digit> → '0'-'9'
3. <rparen> → <space>* ')' <space>*
4. <expop> → '^' <space>*
5. <mulop> → <space>* '*' <space>*
6. <addop> → <space>* '+' <space>*
Production rules
1. <num> → <digit>+
2. <paren> → <num> | <lparen> <expr> <rparen>
1
3. <exp> → (<paren> <expop>)* <paren>
4. <mul> → <exp> (<mulop> <exp>)*
5. <add> → <mul> (<addop> <mul>)*
6. <expr> → <add>


(* comment ::=  ; all subsequent characters up to a line-ending
  or paragraph-separator
| #; (whitespace | comment)* datum



