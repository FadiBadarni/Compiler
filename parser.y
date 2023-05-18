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
%token <string> CHAR INT REAL STRING VAR ASSIGNMENT SEMICOLON COLON
%token <string> TYPE

%type <node> statement expression
%type <node> program

%left PLUS MINUS
%left MULTI DIVISION

%nonassoc IDENTIFIER

%%

// Start symbol
program:
    /* empty */
    | program statement { root = $2; printTree(root); }
    ;

statement:
    VAR IDENTIFIER COLON TYPE SEMICOLON { $$ = createNode("declare", createNode($2, NULL, NULL), createNode($4, NULL, NULL)); } // handle variable declaration
    | IDENTIFIER ASSIGNMENT expression SEMICOLON { $$ = createNode("=", createNode($1, NULL, NULL), $3); } // handle variable assignment
    ;

expression:
    IDENTIFIER { $$ = createNode($1, NULL, NULL); }  // Terminal: IDENTIFIER
    | CHAR { $$ = createNode($1, NULL, NULL); }  // Terminal: CHAR
    | INT { $$ = createNode($1, NULL, NULL); }  // Terminal: INT
    | REAL { $$ = createNode($1, NULL, NULL); }  // Terminal: REAL
    | STRING { $$ = createNode($1, NULL, NULL); }  // Terminal: STRING
    | expression PLUS expression     { $$ = createNode("+", $1, $3); }
    | expression MINUS expression    { $$ = createNode("-", $1, $3); }
    | expression MULTI expression    { $$ = createNode("*", $1, $3); }
    | expression DIVISION expression { $$ = createNode("/", $1, $3); }
    | '(' expression ')'             { $$ = $2; }
    ;

%%

node* createNode(char* token, node *left, node *right)
{
    node *newNode = (node*)malloc(sizeof(node));
    newNode->token = strdup(token);
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

    // If this is an operator (it has left and right children), print it before the children
    if (tree->left != NULL && tree->right != NULL) {
        indent(printlevel);
        printf("(%s\n", tree->token);
        printlevel++;
    }

    // Recurse for the left child
    if (tree->left != NULL) {
        printTree(tree->left);
    }

    // If this is an identifier (it doesn't have children), print it without indent
    if (tree->left == NULL && tree->right == NULL) {
        indent(printlevel);
        printf("%s\n", tree->token);
    }

    // Recurse for the right child
    if (tree->right != NULL) {
        printTree(tree->right);
        if (tree->left != NULL && tree->right != NULL) {
            printlevel--;
            indent(printlevel);
            printf(")\n");
        }
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
