%{
#include<stdio.h>
#include "y.tab.h"
%}

%x COMMENT

%%

 /*Keywords Lexemes*/
"bool"          { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"char"          { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"int"           { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"real"          { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"string"        { yylval.string = strdup(yytext); printf("Returning TYPE: %s\n", yytext); return TYPE; }
"char*"         { yylval.string = strdup(yytext); printf("Returning POINTER_TYPE: %s\n", yytext); return POINTER_TYPE; }
"int*"          { yylval.string = strdup(yytext); printf("Returning POINTER_TYPE: %s\n", yytext); return POINTER_TYPE; }
"real*"         { yylval.string = strdup(yytext); printf("Returning POINTER_TYPE: %s\n", yytext); return POINTER_TYPE; }
"if"            { yylval.string = strdup(yytext); printf("Returning IF: %s\n", yytext); return IF; }
"else"          { yylval.string = strdup(yytext); printf("Returning ELSE: %s\n", yytext); return ELSE; }
"while"         { yylval.string = strdup(yytext); printf("Returning WHILE: %s\n", yytext); return WHILE; }
"for"           { yylval.string = strdup(yytext); printf("Returning FOR: %s\n", yytext); return FOR; }
"var"           { yylval.string = strdup(yytext); printf("Returning VAR: %s\n", yytext); return VAR; }
"function"      { yylval.string = strdup(yytext); printf("Returning FUNCTION: %s\n", yytext); return FUNCTION; }
"main"          { yylval.string = strdup(yytext); printf("Returning MAIN: %s\n", yytext); return MAIN; }
"return"        { yylval.string = strdup(yytext); printf("Returning RETURN: %s\n", yytext); return RETURN; }
"null"          { yylval.string = strdup(yytext); printf("Returning NULL: %s\n", yytext); return NULL_PTR; }
"void"          { yylval.string = strdup(yytext); printf("Returning VOID: %s\n", yytext); return VOID; }
"do"            { yylval.string = strdup(yytext); printf("Returning DO: %s\n", yytext); return DO; }

 /*Operator Lexemes*/
"&&"        { printf("Returning AND: %s\n", yytext); return AND;}
"/"         { printf("Returning DIVISION: %s\n", yytext); return DIVISION;}
"="         { printf("Returning ASSIGNMENT: %s\n", yytext); return ASSIGNMENT;}
"=="        { printf("Returning EQUALS: %s\n", yytext); return EQUALS;}
">"         { printf("Returning GT: %s\n", yytext); return GT;}
">="        { printf("Returning GTE: %s\n", yytext); return GTE;}
"<"         { printf("Returning LT: %s\n", yytext); return LT;}
"<="        { printf("Returning LTE: %s\n", yytext); return LTE;}
"-"         { printf("Returning MINUS: %s\n", yytext); return MINUS;}
"!"         { printf("Returning NOT: %s\n", yytext); return NOT;}
"!="        { printf("Returning NEQ: %s\n", yytext); return NEQ;}
"||"        { printf("Returning OR: %s\n", yytext); return OR;}
"+"         { printf("Returning PLUS: %s\n", yytext); return PLUS;}
"*"         { printf("Returning MULTI: %s\n", yytext); return MULTI;}
"&"         { printf("Returning ADDRESS: %s\n", yytext); return ADDRESS; }

 /*Literal Lexemes*/
 "true"                  {
    yylval.string = strdup(yytext);
    printf("Returning BOOL_LITERAL: %s\n", yytext);
    return BOOL_LITERAL;
}
"false"                 {
    yylval.string = strdup(yytext);
    printf("Returning BOOL_LITERAL: %s\n", yytext);
    return BOOL_LITERAL;
}
[a-zA-Z][a-zA-Z0-9_]*   {
    yylval.string = strdup(yytext);
    printf("Returning IDENTIFIER: %s\n", yytext);
    return IDENTIFIER;
}
[0-9]+                  {
    yylval.string = strdup(yytext);
    printf("Returning INT_LITERAL: %s\n", yytext);
    return INT_LITERAL;
}
[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)? {
    yylval.string = strdup(yytext);
    printf("Returning REAL_LITERAL: %s\n", yytext);
    return REAL_LITERAL;
}
\"([^"])*\"             {
    yylval.string = strdup(yytext);
    printf("Returning STRING_LITERAL: %s\n", yytext);
    return STRING_LITERAL;
}
\'(.)\'                 {
    yylval.string = strdup(yytext);
    printf("Returning CHAR_LITERAL: %s\n", yytext);
    return CHAR_LITERAL;
}

 /*Other Lexemes*/
";"         { printf("Returning SEMICOLON: %s\n", yytext); return SEMICOLON;}
":"         { printf("Returning COLON: %s\n", yytext); return COLON;}
"("         { printf("Returning LPAREN: %s\n", yytext); return LPAREN;}
")"         { printf("Returning RPAREN: %s\n", yytext); return RPAREN;}
"{"         { printf("Returning LBRACE: %s\n", yytext); return LBRACE;}
"}"         { printf("Returning RBRACE: %s\n", yytext); return RBRACE;}
","         { printf("Returning COMMA: %s\n", yytext); return COMMA; }
"->"        { printf("Returning ARROW: %s\n", yytext); return ARROW; }
"|"         { printf("Returning PIPE: %s\n", yytext); return PIPE;}
"["         { printf("Returning LBRACKET: %s\n", yytext); return LBRACKET;}
"]"         { printf("Returning RBRACKET: %s\n", yytext); return RBRACKET;}

 /*Comments */
"/*"      BEGIN(COMMENT);
<COMMENT>"*/"     BEGIN(INITIAL);
<COMMENT>.|\n     ; /* eat up anything (including newlines) within the comments */

 /*Skip Whitespace, Enter, Tab*/
[\n]                    {yylineno++;}
[ \t]+                  ;
[^a-zA-Z0-9_ \t\n]      { printf("Unrecognized character: %s\n", yytext); exit(-1); }

%%
