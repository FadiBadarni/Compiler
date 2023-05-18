%{
#include<stdio.h>
#include "y.tab.h"
%}

%%

 /*Keywords Lexemes*/
"char"      { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"int"       { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"real"      { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"string"    { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }

"var"       { yylval.string = strdup(yytext); printf("Returning VAR: %s\n", yytext); return VAR; }

 /*Operator Lexemes*/
"/"         { printf("Returning DIVISION: %s\n", yytext); return DIVISION;}
"-"         { printf("Returning MINUS: %s\n", yytext); return MINUS;}
"+"         { printf("Returning PLUS: %s\n", yytext); return PLUS;}
"*"         { printf("Returning MULTI: %s\n", yytext); return MULTI;}
"="         { printf("Returning ASSIGNMENT: %s\n", yytext); return ASSIGNMENT;}
";"         { printf("Returning SEMICOLON: %s\n", yytext); return SEMICOLON;}
":"         { printf("Returning COLON: %s\n", yytext); return COLON;}

 /*Literal Lexemes*/
[a-zA-Z][a-zA-Z0-9_]*   { yylval.string= strdup(yytext); printf("Returning IDENTIFIER: %s\n", yytext); return IDENTIFIER;}

 /*Skip Whitespace, Enter, Tab*/
[\n]                    {yylineno++;}
[ \t]+                  ;
[^a-zA-Z0-9_ \t\n]      { printf("Unrecognized character: %s\n", yytext); exit(-1); }

%%
