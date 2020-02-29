tok ::= ID [char] posn | CHAR char posn
posn == (num,num)

analyse =
%lex
letter = `a-z\xfff\&A-Z-`
letter+ -> ID $$ $#
. -> CHAR (hd $$) $#
%%
