%{
#include<stdio.h>
#include "y.tab.h"
%}

%x COMMENT LINE_COMMENT

%%

 /*Comments */
"/*"      BEGIN(COMMENT);
"//"      BEGIN(LINE_COMMENT);

<COMMENT>"*/"     BEGIN(INITIAL);
<COMMENT>.|\n     ; /* eat up anything (including newlines) within the comments */

<LINE_COMMENT>\n  BEGIN(INITIAL);
<LINE_COMMENT>.    ; /* eat up anything within the line comments until newline */


 /*Keywords Lexemes*/
"bool"          { yylval.string = strdup(yytext); return BOOL; }
"char"          { yylval.string = strdup(yytext); return CHAR; }
"int"           { yylval.string = strdup(yytext); return INT; }
"real"          { yylval.string = strdup(yytext); return REAL; }
"string"        { yylval.string = strdup(yytext); return STRING; }
"char*"         { yylval.string = strdup(yytext); return POINTER_TYPE; }
"int*"          { yylval.string = strdup(yytext); return POINTER_TYPE; }
"real*"         { yylval.string = strdup(yytext); return POINTER_TYPE; }
"if"            { yylval.string = strdup(yytext); return IF; }
"else"          { yylval.string = strdup(yytext); return ELSE; }
"while"         { yylval.string = strdup(yytext); return WHILE; }
"for"           { yylval.string = strdup(yytext); return FOR; }
"var"           { yylval.string = strdup(yytext); return VAR; }
"function"      { yylval.string = strdup(yytext); return FUNCTION; }
"main"          { yylval.string = strdup(yytext); return MAIN; }
"return"        { yylval.string = strdup(yytext); return RETURN; }
"null"          { yylval.string = strdup(yytext); return NULL_PTR; }
"void"          { yylval.string = strdup(yytext); return VOID; }
"do"            { yylval.string = strdup(yytext); return DO; }

 /*Operator Lexemes*/
"&&"        { return AND;}
"/"         { return DIVISION;}
"="         { return ASSIGNMENT;}
"=="        { return EQUALS;}
">"         { return GT;}
">="        { return GTE;}
"<"         { return LT;}
"<="        { return LTE;}
"-"         { return MINUS;}
"!"         { return NOT;}
"!="        { return NEQ;}
"||"        { return OR;}
"+"         { return PLUS;}
"++"         { return INCREMENT;}
"*"         { return MULTI;}
"&"         { return ADDRESS; }

 /*Literal Lexemes*/
 "true"                  {
    yylval.string = strdup(yytext);
    return BOOL_LITERAL;
}
"false"                 {
    yylval.string = strdup(yytext);
    return BOOL_LITERAL;
}
[a-zA-Z][a-zA-Z0-9_]*   {
    yylval.string = strdup(yytext);
    return IDENTIFIER;
}
[0-9]+                  {
    yylval.string = strdup(yytext);
    return INT_LITERAL;
}
[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)? {
    yylval.string = strdup(yytext);
    return REAL_LITERAL;
}
\"([^"])*\"             {
    yylval.string = strdup(yytext);
    return STRING_LITERAL;
}
\'(.)\'                 {
    yylval.string = strdup(yytext);
    return CHAR_LITERAL;
}

 /*Other Lexemes*/
";"         { return SEMICOLON;}
":"         { return COLON;}
"("         { return LPAREN;}
")"         { return RPAREN;}
"{"         { return LBRACE;}
"}"         { return RBRACE;}
","         { return COMMA; }
"->"        { return ARROW; }
"|"         { return PIPE;}
"["         { return LBRACKET;}
"]"         { return RBRACKET;}

 /*Skip Whitespace, Enter, Tab*/
[\n]                    {yylineno++;}
[ \t]+                  ;
[^a-zA-Z0-9_ \t\n]      { printf("Unrecognized character: %s\n", yytext); exit(-1); }

%%
