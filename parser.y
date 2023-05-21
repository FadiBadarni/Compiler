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
%token <string> AND OR EQUALS
%token <string> INT_LITERAL CHAR_LITERAL STRING_LITERAL BOOL_LITERAL REAL_LITERAL
%token <string> CHAR INT REAL STRING BOOL VOID
%token <string> VAR ASSIGNMENT SEMICOLON COLON ARROW COMMA PIPE LBRACKET RBRACKET
%token <string> TYPE FUNCTION MAIN RETURN
%token <string> NULL_PTR POINTER_TYPE ADDRESS
%token <string> IF ELSE WHILE DO FOR
%token <string> GT GTE LT LTE NOT NEQ

%token LPAREN RPAREN LBRACE RBRACE

%type <node> statement statements_list expression function_call
%type <node> subroutines subroutine main arguments arguments_list argument identifiers_list
%type <node> program
%type <node> code_block if_statement while_statement do_while_statement for_statement

%left OR
%left AND
%left NEQ EQUALS
%left LT GT LTE GTE
%right NOT
%left PLUS MINUS
%left MULTI DIVISION

%%

/* The entry point of the grammar. A program consists of a set of subroutines and a main function. */
program:
    subroutines main { root = createNode("program", $1, $2); printTree(root); }
    ;

/* This represents a list of subroutines. */
subroutines:
    { $$ = NULL; }
    | subroutines subroutine { $$ = createNode("subroutines", $1, $2); }
    ;

/* A subroutine can either be a function, which returns a specific type, or a procedure, which does not return anything. */
subroutine:
    FUNCTION IDENTIFIER LPAREN arguments RPAREN COLON TYPE LBRACE statements_list RBRACE
        { $$ = createNode("function", createNode($2, createNode("arguments", $4, NULL), NULL), $9); }
    | FUNCTION IDENTIFIER LPAREN arguments RPAREN COLON VOID LBRACE statements_list RBRACE
        { $$ = createNode("procedure", createNode($2, createNode("arguments", $4, NULL), NULL), $9); }
    ;

/* The main function of the program. */
main:
    FUNCTION MAIN LPAREN RPAREN COLON TYPE LBRACE statements_list RBRACE
        { $$ = createNode("function", createNode("main", NULL, NULL), createNode("body", $8, NULL)); }
    | FUNCTION MAIN LPAREN RPAREN COLON VOID LBRACE statements_list RBRACE
        { $$ = createNode("procedure", createNode("main", NULL, NULL), createNode("body", $8, NULL)); }
    ;

/* Represents the list of arguments in a function or procedure definition. If there are no arguments, a 'none' argument node is created. */
arguments:
    { $$ = createNode("arguments_none", NULL, NULL); }  // No arguments.
    | arguments_list { $$ = $1; }  // One or more arguments.
    ;

/* Used to parse the list of arguments. */
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

/* Used to parse individual arguments. */
argument:
    IDENTIFIER ARROW identifiers_list COLON TYPE
        { $$ = createNode("argument", createNode($5, NULL, NULL), $3); }
    ;

/* Used to parse lists of identifiers, separated by commas. */
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

/* Used to parse lists of statements. */
statements_list:
    statement
    | statements_list statement { $$ = createNode("statements_list", $1, $2); }
    ;

/* non-terminal for a function call. */
function_call:
    IDENTIFIER LPAREN RPAREN { $$ = createNode("call", createNode($1, NULL, NULL), NULL); }
    ;

/* Used to parse individual statements, including variable declarations, variable assignments, return statements, and different types of control structures like if, while, do-while, and for loops. */
statement:
    VAR IDENTIFIER ASSIGNMENT expression COLON POINTER_TYPE SEMICOLON
        { $$ = createNode("declare_initialize_pointer", createNode($2, NULL, NULL), createNode("declare_initialize_data", $4, createNode($6, NULL, NULL))); } // handle pointer variable declaration with initialization
    | VAR IDENTIFIER ASSIGNMENT expression COLON TYPE SEMICOLON
        { $$ = createNode("declare_initialize", createNode($2, NULL, NULL), createNode("declare_initialize_data", $4, createNode($6, NULL, NULL))); } // handle variable declaration with initialization
    | VAR IDENTIFIER COLON POINTER_TYPE SEMICOLON
        { $$ = createNode("declare_pointer", createNode($2, NULL, NULL), createNode($4, NULL, NULL)); } // handle pointer variable declaration
    | VAR IDENTIFIER COLON TYPE SEMICOLON
        { $$ = createNode("declare", createNode($2, NULL, NULL), createNode($4, NULL, NULL)); } // handle variable declaration
    | IDENTIFIER ASSIGNMENT expression SEMICOLON
        { $$ = createNode("=", createNode($1, NULL, NULL), $3); } // handle variable assignment
    | TYPE IDENTIFIER LBRACKET INT_LITERAL RBRACKET SEMICOLON
        { $$ = createNode("declare_string", createNode($2, NULL, NULL), createNode($4, NULL, NULL)); } /* Strings declarations */
    | TYPE IDENTIFIER LBRACKET INT_LITERAL RBRACKET ASSIGNMENT STRING_LITERAL SEMICOLON
        { $$ = createNode("declare_initialize_string", createNode($2, NULL, NULL), createNode("declare_initialize_data", createNode($4, NULL, NULL), createNode($6, NULL, NULL))); }
    | IDENTIFIER LBRACKET expression RBRACKET ASSIGNMENT CHAR_LITERAL SEMICOLON
        { $$ = createNode("array_assign", createNode("array_index", createNode($1, NULL, NULL), $3), createNode($5, NULL, NULL)); } /* String element assignments */
    | RETURN expression SEMICOLON
        { $$ = createNode("return", $2, NULL); } // handle return statement
    | subroutine
        { $$ = createNode("nested_function", $1, NULL); } // handle nested function
    | expression
    | code_block
    | if_statement
    | while_statement
    | do_while_statement
    | for_statement
    ;

code_block:
    LBRACE statements_list RBRACE { $$ = createNode("block", $2, NULL); }
    | LBRACE RBRACE { $$ = createNode("block_empty", NULL, NULL); }
    ;

/* Used to parse if-else statements as well as standalone if statements. */
if_statement:
    IF LPAREN expression RPAREN LBRACE statements_list RBRACE ELSE LBRACE statements_list RBRACE
    {
        node* if_body_node = createNode("if_body", $6, NULL);
        node* else_body_node = createNode("else_body", $10, NULL);
        $$ = createNode("if_else", $3, createNode("if_else_wrapper", if_body_node, else_body_node));
    }
    | IF LPAREN expression RPAREN LBRACE statements_list RBRACE
    {
        $$ = createNode("if", $3, createNode("if_body", $6, NULL));
    }
    ;

/* Used to parse while loops. */
while_statement:
    WHILE LPAREN expression RPAREN LBRACE statements_list RBRACE
    {
        $$ = createNode("while", $3, createNode("body", $6, NULL));
    }
    ;

/* Used to parse do-while loops. */
do_while_statement:
    DO LBRACE statements_list RBRACE WHILE LPAREN expression RPAREN SEMICOLON
    {
        $$ = createNode("do_while", $3, $7);
    }
    ;

/* Used to parse for loops. */
for_statement:
    FOR LPAREN expression SEMICOLON expression SEMICOLON expression RPAREN LBRACE statements_list RBRACE
    {
        node* initialization = $3;
        node* condition = $5;
        node* increment = $7;
        node* body = $10;
        $$ = createNode("for", initialization, createNode("for_body", condition, createNode("for_increment", increment, body)));
    }
    ;

expression:
    function_call
    |
    IDENTIFIER
        { $$ = createNode($1, NULL, NULL); }  // Terminal: IDENTIFIER
    | INT_LITERAL
        { $$ = createNode($1, NULL, NULL); }  // Terminal: INT_LITERAL
    | CHAR_LITERAL
        { $$ = createNode($1, NULL, NULL); }  // Terminal: CHAR_LITERAL
    | STRING_LITERAL
        { $$ = createNode($1, NULL, NULL); }  // Terminal: STRING_LITERAL
    | BOOL_LITERAL
        { $$ = createNode($1, NULL, NULL); }  // Terminal: BOOL_LITERAL
    | REAL_LITERAL
        { $$ = createNode($1, NULL, NULL); }  // Terminal: REAL_LITERAL
    | CHAR
        { $$ = createNode($1, NULL, NULL); }  // Terminal: CHAR
    | INT
        { $$ = createNode($1, NULL, NULL); }  // Terminal: INT
    | REAL
        { $$ = createNode($1, NULL, NULL); }  // Terminal: REAL
    | STRING
        { $$ = createNode($1, NULL, NULL); }  // Terminal: STRING
    | BOOL
        { $$ = createNode($1, NULL, NULL); }  // Terminal: BOOL
    | VOID
        { $$ = createNode($1, NULL, NULL); }  // Terminal: VOID
    | NULL_PTR
        { $$ = createNode("null", NULL, NULL); }  // Terminal: NULL_PTR
    | POINTER_TYPE
        { $$ = createNode($1, NULL, NULL); }  // Terminal: POINTER_TYPE
    | '*' IDENTIFIER
        { $$ = createNode("*", createNode($2, NULL, NULL), NULL); }  // Terminal: dereference pointer
    | '&' IDENTIFIER
        { $$ = createNode("&", createNode($2, NULL, NULL), NULL); }  // Terminal: address of variable
    | PIPE IDENTIFIER PIPE
        { $$ = createNode("length_of", createNode($2, NULL, NULL), NULL); }  // Retrieve the length of a string
    | IDENTIFIER LBRACKET expression RBRACKET
        { $$ = createNode("array_index", createNode($1, NULL, NULL), $3); }  // Retrieve a character from a string
    | expression PLUS expression
        { $$ = createNode("+", $1, $3); }
    | expression MINUS expression
        { $$ = createNode("-", $1, $3); }
    | expression MULTI expression
        { $$ = createNode("*", $1, $3); }
    | expression DIVISION expression
        { $$ = createNode("/", $1, $3); }
    | expression AND expression
        { $$ = createNode("&&", $1, $3); }
    | expression OR expression
        { $$ = createNode("||", $1, $3); }
    | expression EQUALS expression
        { $$ = createNode("==", $1, $3); }
    | expression NEQ expression
        { $$ = createNode("!=", $1, $3); }
    | expression LT expression
        { $$ = createNode("<", $1, $3); }
    | expression GT expression
        { $$ = createNode(">", $1, $3); }
    | expression LTE expression
        { $$ = createNode("<=", $1, $3); }
    | expression GTE expression
        { $$ = createNode(">=", $1, $3); }
    | NOT expression
        { $$ = createNode("!", $2, NULL); }
    | '(' expression ')'
        { $$ = $2; }
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

    // If this is the "statements_list" node, don't print it and just continue with the children
    if (strcmp(tree->token, "statements_list") == 0) {
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
        printf("\033[0;32m%s\033[0m\n", tree->token);
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
