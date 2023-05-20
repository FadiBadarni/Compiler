%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "lex.yy.c"

    typedef struct node
    {
        char *token;
        struct node *left;
        struct node *right;
    } node;

    node* createNode(char* token, node *left, node *right);
    void printTree (node *tree);
    void indent(int n);
    int yylex();
    int yyerror(const char *e);

    int printlevel=0;
    node *root;
%}

%union
{
    struct node *node;
    char *string;
}


%token <string> DIVISION PLUS MINUS MULTI IDENTIFIER
%token <string> INT_LITERAL CHAR_LITERAL STRING_LITERAL
%token <string> CHAR INT REAL STRING BOOL
%token <string> VAR ASSIGNMENT SEMICOLON COLON ARROW COMMA
%token <string> TYPE FUNCTION PROCEDURE MAIN
%token <string> NULL_PTR POINTER_TYPE ADDRESS
%token <string> IF ELSE WHILE FOR

%token LPAREN RPAREN LBRACE RBRACE

%type <node> statement statements_list expression
%type <node> subroutines subroutine main arguments arguments_list argument identifiers_list
%type <node> program
%type <node> if_statement

%left PLUS MINUS
%left MULTI DIVISION

%nonassoc IDENTIFIER
%nonassoc ADDRESS

%%

// Start symbol
program:
    subroutines main { root = createNode("program", $1, $2); printTree(root); }
    ;

subroutines:
    /* empty */ { $$ = NULL; }
    | subroutines subroutine { $$ = createNode("subroutines", $1, $2); }
    ;

subroutine:
    FUNCTION IDENTIFIER LPAREN arguments RPAREN COLON TYPE LBRACE statements_list RBRACE
        { $$ = createNode("function", createNode($2, createNode("arguments", $4, NULL), NULL), $9); }
    | PROCEDURE IDENTIFIER LPAREN arguments RPAREN LBRACE statements_list RBRACE
        { $$ = createNode("procedure", createNode($2, createNode("arguments", $4, NULL), NULL), $7); }
    ;

main:
    FUNCTION MAIN LPAREN RPAREN COLON TYPE LBRACE statements_list RBRACE
        { $$ = createNode("function", createNode("main", NULL, NULL), createNode("body", $8, NULL)); }
    ;

arguments:
    /* empty */ { $$ = createNode("arguments_none", NULL, NULL); }
    | arguments_list { $$ = $1; }
    ;

arguments_list:
    argument { $$ = createNode("arguments_list", $1, NULL); }
    | arguments_list SEMICOLON argument {
        node* current = $1;
        while(current->right != NULL) {
            current = current->right;
        }
        current->right = createNode("argument", $3, NULL);
        $$ = $1;
    }
    ;



argument:
    IDENTIFIER ARROW identifiers_list COLON TYPE
        { $$ = createNode("argument", createNode($5, NULL, NULL), $3); }
    ;

identifiers_list:
    IDENTIFIER { $$ = createNode(strdup($1), NULL, NULL); }
    | identifiers_list COMMA IDENTIFIER {
        char* new_identifier = malloc(strlen($1->token) + strlen($3) + 2);  // for the space and the null terminator
        strcpy(new_identifier, $1->token);
        strcat(new_identifier, " ");
        strcat(new_identifier, $3);
        free($1->token);  // free the old identifier list
        $$ = createNode(new_identifier, NULL, NULL);
      }
    ;

if_statement:
    IF LPAREN expression RPAREN LBRACE statement RBRACE
    {
        $$ = createNode("if", $3, createNode("body", $6, NULL));
    }
    | IF LPAREN expression RPAREN LBRACE statement RBRACE ELSE LBRACE statement RBRACE
    {
        node* if_body_node = createNode("if_body", $6, NULL);
        node* else_body_node = createNode("else_body", $10, NULL);
        $$ = createNode("if_else", $3, createNode("if_else_wrapper", if_body_node, else_body_node));
    }
    ;




statements_list:
    statement
    | statements_list statement { $$ = createNode("statements_list", $1, $2); }
    ;

statement:
    VAR IDENTIFIER COLON POINTER_TYPE SEMICOLON
        { $$ = createNode("declare_pointer", createNode($2, NULL, NULL), createNode($4, NULL, NULL)); } // handle pointer variable declaration
    | VAR IDENTIFIER COLON TYPE SEMICOLON
        { $$ = createNode("declare", createNode($2, NULL, NULL), createNode($4, NULL, NULL)); } // handle variable declaration
    | IDENTIFIER ASSIGNMENT expression SEMICOLON
        { $$ = createNode("=", createNode($1, NULL, NULL), $3); } // handle variable assignment
    | if_statement
    ;

expression:
    IDENTIFIER { $$ = createNode($1, NULL, NULL); }  // Terminal: IDENTIFIER
    | INT_LITERAL { $$ = createNode($1, NULL, NULL); }  // Terminal: INT_LITERAL
    | CHAR_LITERAL { $$ = createNode($1, NULL, NULL); }  // Terminal: CHAR_LITERAL
    | STRING_LITERAL { $$ = createNode($1, NULL, NULL); }  // Terminal: STRING_LITERAL
    | CHAR { $$ = createNode($1, NULL, NULL); }  // Terminal: CHAR
    | INT { $$ = createNode($1, NULL, NULL); }  // Terminal: INT
    | REAL { $$ = createNode($1, NULL, NULL); }  // Terminal: REAL
    | STRING { $$ = createNode($1, NULL, NULL); }  // Terminal: STRING
    | BOOL { $$ = createNode($1, NULL, NULL); }  // Terminal: STRING
    | NULL_PTR { $$ = createNode("null", NULL, NULL); }  // Terminal: NULL_PTR
    | POINTER_TYPE { $$ = createNode($1, NULL, NULL); }  // Terminal: POINTER_TYPE
    | '*' IDENTIFIER { $$ = createNode("*", createNode($2, NULL, NULL), NULL); }  // Terminal: dereference pointer
    | '&' IDENTIFIER { $$ = createNode("&", createNode($2, NULL, NULL), NULL); }  // Terminal: address of variable
    | expression PLUS expression     { $$ = createNode("+", $1, $3); }
    | expression MINUS expression    { $$ = createNode("-", $1, $3); }
    | expression MULTI expression    { $$ = createNode("*", $1, $3); }
    | expression DIVISION expression { $$ = createNode("/", $1, $3); }
    | '(' expression ')'             { $$ = $2; }
    ;

%%

node* createNode(char* token, node *left, node *right) {
    node *newNode = (node*)malloc(sizeof(node));
    if (newNode == NULL) {
        fprintf(stderr, "Error: Unable to allocate memory for node\n");
        return NULL;
    }

    newNode->token = strdup(token);
    if (newNode->token == NULL) {
        fprintf(stderr, "Error: Unable to allocate memory for token\n");
        free(newNode);
        return NULL;
    }

    newNode->left = left;
    newNode->right = right;
    return newNode;
}


void indent(int n)
{
    for(int i=0; i<n; i++)
        printf("\t");
}

void printTree(node *tree)
{
    if (tree == NULL) return;

    // Determine if this node is an "operator" or not
    int isOperator = (tree->left != NULL || tree->right != NULL);

    // If this is the "subroutines" node, don't print it and just continue with the children
    if (strcmp(tree->token, "subroutines") == 0) {
        printTree(tree->left);
        printTree(tree->right);
        return;
    }

    // If this is the "arguments" node, don't print it and just continue with the children
    if (strcmp(tree->token, "arguments") == 0) {
        printTree(tree->left);
        printTree(tree->right);
        return;
    }

    // If this is the "if_else_wrapper" node, don't print it and just continue with the children
    if (strcmp(tree->token, "if_else_wrapper") == 0) {
        printTree(tree->left);
        printTree(tree->right);
        return;
    }


    // If this is an operator, print it before the children
    if (isOperator) {
        indent(printlevel);
        printf("(%s\n", tree->token);
        printlevel++;
    }

    // Recurse for the left child
    if (tree->left != NULL) {
        printTree(tree->left);
    }

    // If this node is not an operator, print it without indent
    if (!isOperator) {
        indent(printlevel);
        printf("%s\n", tree->token);
    }

    // Recurse for the right child
    if (tree->right != NULL) {
        printTree(tree->right);
    }

    // If this is an operator, close the parentheses after the children have been printed
    if (isOperator) {
        printlevel--;
        indent(printlevel);
        printf(")\n");
    }
}




int yyerror(const char *e)
{
    fprintf(stderr, "Error: %s\n", e);
    return 0;
}

int main(int argc, char *argv[])
{
    FILE *inputFile = fopen(argv[1], "r");
    if (!inputFile) {
        printf("Error: Unable to open file %s\n", argv[1]);
        return -1;
    }

    yyin = inputFile;
    yyparse();
    fclose(inputFile);

    return 0;
}
