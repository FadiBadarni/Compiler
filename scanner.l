/*Scanner-file*/
%{
#include<stdio.h>
#include "y.tab.h"
%}

%%

 /*Operators*/
"/" {return DIVISION;}
"-" {return MINUS;}
"+" {return PLUS;}
"*" {return MULTI;}

  /*Literl*/
[a-zA-Z][a-zA-Z0-9_]* { yylval.string= strdup(yytext);return IDENTIFIER;}

 /*Skip Whitespace, Enter, Tab*/
[\n] {yylineno++;}
[ \t]+ ;
. return yytext[0];
%%

