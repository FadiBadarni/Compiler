%{
#include<stdio.h>
#include "y.tab.h"
%}

%%

 /*Keywords Lexemes*/
"bool"      { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"char"      { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"int"       { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"real"      { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"string"    { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"char*"      { yylval.string = strdup(yytext); printf("Returning POINTER_TYPE: %s\n", yytext); return POINTER_TYPE; }
"int*"       { yylval.string = strdup(yytext); printf("Returning POINTER_TYPE: %s\n", yytext); return POINTER_TYPE; }
"real*"      { yylval.string = strdup(yytext); printf("Returning POINTER_TYPE: %s\n", yytext); return POINTER_TYPE; }

"null"      { yylval.string = strdup(yytext); printf("Returning NULL: %s\n", yytext); return NULL_PTR; }

"procedure" { yylval.string = strdup(yytext); printf("Returning PROCEDURE: %s\n", yytext); return PROCEDURE; }
"function"  { yylval.string = strdup(yytext); printf("Returning FUNCTION: %s\n", yytext); return FUNCTION; }
"main"      { yylval.string = strdup(yytext); printf("Returning MAIN: %s\n", yytext); return MAIN; }

"var"       { yylval.string = strdup(yytext); printf("Returning VAR: %s\n", yytext); return VAR; }
"if"       { yylval.string = strdup(yytext); printf("Returning IF: %s\n", yytext); return IF; }
"else"       { yylval.string = strdup(yytext); printf("Returning ELSE: %s\n", yytext); return ELSE; }

 /*Operator Lexemes*/
"/"         { printf("Returning DIVISION: %s\n", yytext); return DIVISION;}
"-"         { printf("Returning MINUS: %s\n", yytext); return MINUS;}
"+"         { printf("Returning PLUS: %s\n", yytext); return PLUS;}
"*"         { printf("Returning MULTI: %s\n", yytext); return MULTI;}
"="         { printf("Returning ASSIGNMENT: %s\n", yytext); return ASSIGNMENT;}
";"         { printf("Returning SEMICOLON: %s\n", yytext); return SEMICOLON;}
":"         { printf("Returning COLON: %s\n", yytext); return COLON;}
"("         { printf("Returning LPAREN: %s\n", yytext); return LPAREN;}
")"         { printf("Returning RPAREN: %s\n", yytext); return RPAREN;}
"{"         { printf("Returning LBRACE: %s\n", yytext); return LBRACE;}
"}"         { printf("Returning RBRACE: %s\n", yytext); return RBRACE;}
"&"         { printf("Returning ADDRESS: %s\n", yytext); return ADDRESS; }
"->"        { printf("Returning ARROW: %s\n", yytext); return ARROW; }
","         { printf("Returning COMMA: %s\n", yytext); return COMMA; }

 /*Literal Lexemes*/
[a-zA-Z][a-zA-Z0-9_]*   { yylval.string= strdup(yytext); printf("Returning IDENTIFIER: %s\n", yytext); return IDENTIFIER;}
[0-9]+                  { yylval.string = strdup(yytext); printf("Returning INT_LITERAL: %s\n", yytext); return INT_LITERAL; }
\"([^"])*\"             { yylval.string = strdup(yytext); printf("Returning STRING_LITERAL: %s\n", yytext); return STRING_LITERAL; }
\'(.)\'                 { yylval.string = strdup(yytext); printf("Returning CHAR_LITERAL: %s\n", yytext); return CHAR_LITERAL; }

 /*Skip Whitespace, Enter, Tab*/
[\n]                    {yylineno++;}
[ \t]+                  ;
[^a-zA-Z0-9_ \t\n]      { printf("Unrecognized character: %s\n", yytext); exit(-1); }

%%
