%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "lex.yy.c"
    #include <stdbool.h>

    typedef struct node
    {
        char *token;
        struct node *left;
        struct node *right;
    } node;

    typedef struct symbol_table_entry
    {
        char *name;
        char *type;
        struct symbol_table_entry *next;
    } symbol_table_entry;

    typedef struct symbol_table
    {
        symbol_table_entry *head;
        struct symbol_table *prev;
    } symbol_table;

    node* createNode(char* token, node *left, node *right);
    void printTree (node *tree);
    void indent(int n);
    int yylex();
    int yyerror(const char *e);
    symbol_table *current_table = NULL; // this points to the top of the stack
    int printlevel=0;
    node *root;

    symbol_table* createSymbolTable()
    {
        symbol_table *table = (symbol_table*) malloc(sizeof(symbol_table));
        if (table == NULL)
        {
            fprintf(stderr, "Error: Unable to allocate memory for symbol table\n");
            return NULL;
        }
        table->head = NULL;
        table->prev = NULL;
        return table;
    }

    int getSymbolTableDepth()
    {
        int depth = 0;
        symbol_table *table = current_table;
        while (table != NULL)
        {
            depth++;
            table = table->prev;
        }
        return depth;
    }

    void printSymbolTable(symbol_table *table)
    {
        printf("Symbol Table :\n");

        // Print the table header
        printf("-----------------------------------------------\n");
        printf("| %-20s | %-20s |\n", "Name", "Type");
        printf("-----------------------------------------------\n");

        // Print each entry in the table
        symbol_table_entry *entry = table->head;
        while (entry != NULL)
        {
            printf("| %-20s | %-20s |\n", entry->name, entry->type);
            entry = entry->next;
        }

        // Print the table footer
        printf("-----------------------------------------------\n");
    }

    void pushSymbolTable(symbol_table *table)
    {
        table->prev = current_table;
        current_table = table;
        printf("Pushed symbol table. Current scope depth after push: %d\n", getSymbolTableDepth());
    }


    void popSymbolTable()
    {
        symbol_table *table = current_table;
        printf("Popping symbol table. Current scope depth before pop: %d\n", getSymbolTableDepth());
        current_table = current_table->prev;
        free(table);
        printf("Popped symbol table. Current scope depth after pop: %d\n", getSymbolTableDepth());
    }


    symbol_table_entry* lookupSymbolTableInCurrentScope(char *name)
    {
        if (current_table == NULL) {
            return NULL;
        }
        symbol_table_entry *entry = current_table->head;
        while (entry != NULL)
        {
            if (strcmp(entry->name, name) == 0)
                return entry;
            entry = entry->next;
        }
        return NULL;
    }

    symbol_table_entry* lookupSymbolTable(char *name)
    {
        symbol_table *table = current_table;
        while (table != NULL)
        {
            symbol_table_entry *entry = table->head;
            while (entry != NULL)
            {
                if (strcmp(entry->name, name) == 0)
                    return entry;
                entry = entry->next;
            }
            table = table->prev;
        }
        return NULL;
    }

    void addSymbolTableEntry(char *name, char *type)
    {
        /* Check if the name already exists in the current scope */
        symbol_table_entry *existingEntry = lookupSymbolTableInCurrentScope(name);
        if (existingEntry != NULL) {
            fprintf(stderr, "Error: Name %s is already declared in the current scope\n", name);
            return;
        }

        /* If name does not exist in current scope, add it */
        symbol_table_entry *entry = (symbol_table_entry*) malloc(sizeof(symbol_table_entry));
        if (entry == NULL)
        {
            fprintf(stderr, "Error: Unable to allocate memory for symbol table entry\n");
            return;
        }
        entry->name = strdup(name);
        entry->type = strdup(type);
        entry->next = current_table->head;
        current_table->head = entry;

        printf("Added symbol to table. Current table:\n");
        printSymbolTable(current_table);
    }

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

%type <node> statement statements_list expression function_call function_call_arguments
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

%nonassoc ASSIGNMENT

%%

/* The entry point of the grammar. A program consists of a set of subroutines and a main function. */
program:
    subroutines main {
        root = createNode("program", $1, $2);
            printTree(root);
    }
    ;

/* This represents a list of subroutines. */
subroutines:
    { $$ = NULL; }
    | subroutines subroutine { $$ = createNode("subroutines", $1, $2); }
    ;

/* A subroutine can either be a function, which returns a specific type, or a procedure, which does not return anything. */
subroutine:
    FUNCTION IDENTIFIER LPAREN arguments RPAREN COLON TYPE LBRACE
        {
            /* Check if a function with the same name is already declared in the global scope */
            symbol_table_entry *existingEntry = lookupSymbolTableInCurrentScope($2);
            if (existingEntry != NULL) {
                yyerror("Error: Function with this name is already declared in the current scope");
                YYABORT;
            }

            /* Add the function name to the global symbol table */
            addSymbolTableEntry($2, "function");

            /* When we start a new function, we enter a new scope.
            So we create a new symbol table and push it onto the stack. */
            symbol_table *newTable = createSymbolTable();
            if (newTable == NULL) {
                yyerror("Failed to create symbol table");
                YYABORT;
            }
            pushSymbolTable(newTable);
        }
    statements_list
        {
            /* When we're done with the function, we exit its scope,
            so we pop its symbol table off the stack. */
            popSymbolTable();
        }
    RBRACE
        {
            $$ = createNode("function", createNode($2, createNode("arguments", $4, NULL), NULL), createNode("body", $10, NULL));
        }
    | FUNCTION IDENTIFIER LPAREN arguments RPAREN COLON VOID LBRACE statements_list RBRACE
        { $$ = createNode("procedure", createNode($2, createNode("arguments", $4, NULL), NULL), createNode("body", $9, NULL)); }
    ;

/* The main function of the program. */
main:
    FUNCTION MAIN LPAREN RPAREN COLON VOID LBRACE
        {
            /* Check if a function with the same name is already declared in the global scope */
            symbol_table *globalTable = current_table;
            while (globalTable->prev != NULL) {
                globalTable = globalTable->prev;
            }

            symbol_table_entry *existingEntry = lookupSymbolTableInCurrentScope($2);
            if (existingEntry != NULL) {
                yyerror("Error: The program can only have one main function.");
                YYABORT;
            }

            /* Add the main function name to the symbol table */
            addSymbolTableEntry("main", "function");

            /* When we start the main function, we enter a new scope.
            So we create a new symbol table and push it onto the stack. */
            symbol_table *newTable = createSymbolTable();
            if (newTable == NULL) {
                yyerror("Failed to create symbol table");
                YYABORT;
            }
            pushSymbolTable(newTable);
        }
    statements_list
        {
            /* When we're done with the main function, we exit its scope,
            so we pop its symbol table off the stack. */
            popSymbolTable();
        }
    RBRACE
        {
            $$ = createNode("procedure", createNode("main", NULL, NULL), createNode("body", $9, NULL));
        }
    ;



/* Represents the list of arguments in a function or procedure definition. If there are no arguments, a 'none' argument node is created. */
arguments:
    /* No arguments. */
    { $$ = createNode("arguments_none", NULL, NULL); }
    | arguments_list { $$ = $1; }  /* One or more arguments. */
    ;

/* Used to parse the list of arguments. */
arguments_list:
    argument { $$ = createNode("arguments_list", $1, NULL); } /* Single argument */
    | arguments_list SEMICOLON argument { $$ = createNode("arguments_list", $1, $3); } /* Multiple arguments */
    ;

/* Used to parse individual arguments. */
argument:
    IDENTIFIER ARROW identifiers_list COLON TYPE
        { $$ = createNode("argument", createNode($5, NULL, NULL), $3); }
    ;


/* Used to parse lists of identifiers, separated by commas. */
identifiers_list:
    IDENTIFIER { $$ = createNode(strdup($1), NULL, NULL); }
    | IDENTIFIER COMMA identifiers_list {
        char* new_identifier = malloc(strlen($1) + strlen($3->token) + 2);  // for the space and the null terminator
        strcpy(new_identifier, $1);
        strcat(new_identifier, " ");
        strcat(new_identifier, $3->token);
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
    IDENTIFIER LPAREN RPAREN { $$ = createNode("call", createNode($1, NULL, NULL), NULL); } // handle function call without arguments
    | IDENTIFIER LPAREN function_call_arguments RPAREN { $$ = createNode("call", createNode($1, createNode("arguments", $3, NULL), NULL), NULL); } // handle function call with arguments
;


function_call_arguments:
    expression { $$ = createNode("argument", $1, NULL); } // single argument
    | function_call_arguments COMMA expression { $$ = createNode("arguments", $1, createNode("argument", $3, NULL)); } // multiple arguments
;


/* Used to parse individual statements, including variable declarations, variable assignments, return statements, and different types of control structures like if, while, do-while, and for loops. */
statement:
    VAR identifiers_list ASSIGNMENT expression COLON POINTER_TYPE SEMICOLON
        { $$ = createNode("declare_initialize_pointer", $2, createNode("declare_initialize_data", $4, createNode($6, NULL, NULL))); } // handle pointer variable declaration with initialization
    | VAR identifiers_list ASSIGNMENT expression COLON TYPE SEMICOLON
        { $$ = createNode("declare_initialize", $2, createNode("declare_initialize_data", $4, createNode($6, NULL, NULL))); } // handle variable declaration with initialization
    | VAR identifiers_list COLON POINTER_TYPE SEMICOLON
        { $$ = createNode("declare_pointer", $2, createNode($4, NULL, NULL)); } // handle pointer variable declaration
    | VAR identifiers_list COLON TYPE SEMICOLON
        { $$ = createNode("declare", $2, createNode($4, NULL, NULL)); } // handle variable declaration
    | IDENTIFIER ASSIGNMENT expression SEMICOLON
        { $$ = createNode("=", createNode($1, NULL, NULL), $3); } // handle variable assignment
    | TYPE IDENTIFIER LBRACKET INT_LITERAL RBRACKET SEMICOLON
        {
            if (current_table == NULL) {
                fprintf(stderr, "Error: Symbol table is not initialized\n");
                return -1;
            }
            if ($1 == NULL || $2 == NULL) {
                fprintf(stderr, "Error: Null values provided\n");
                return -1;
            }

            symbol_table_entry* entry = lookupSymbolTableInCurrentScope($2);
            if (entry != NULL) {
                fprintf(stderr, "Error: Variable %s already declared\n", $2);
                return -1;
            }

            addSymbolTableEntry($2, $1);
            $$ = createNode("declare_string", createNode($2, NULL, NULL), createNode($4, NULL, NULL));
        } /* Strings declarations */
    | TYPE IDENTIFIER LBRACKET INT_LITERAL RBRACKET ASSIGNMENT STRING_LITERAL SEMICOLON
        { $$ = createNode("declare_initialize_string", createNode($2, NULL, NULL), createNode("initialize_data", createNode("size", createNode($4, NULL, NULL), NULL), createNode("value", createNode($7, NULL, NULL), NULL))); }
    | IDENTIFIER LBRACKET expression RBRACKET ASSIGNMENT CHAR_LITERAL SEMICOLON
        { $$ = createNode("array_assign", createNode("array_index", createNode($1, NULL, NULL), $3), createNode($7, NULL, NULL)); } /* String element assignments */
    | RETURN expression SEMICOLON
        { $$ = createNode("return", $2, NULL); } // handle return statement
    | subroutine
        { $$ = createNode("nested_function", $1, NULL); } // handle nested function
    | function_call SEMICOLON
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
    | ADDRESS IDENTIFIER
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

    // If this is the "arguments_list" node, don't print it and just continue with the children
    if (strcmp(tree->token, "arguments_list") == 0) {
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
    // check command line arguments
    if(argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return -1;
    }

    FILE *inputFile = fopen(argv[1], "r");
    if (!inputFile) {
        fprintf(stderr, "Error: Unable to open file %s\n", argv[1]);
        return -1;
    }

    symbol_table *newTable = createSymbolTable();
    if (newTable == NULL) {
        fprintf(stderr, "Failed to create symbol table\n");
        return -1;
    }
    pushSymbolTable(newTable);


    yyin = inputFile;
    if (yyparse() != 0) {
        fprintf(stderr, "Error: Parsing failed\n");
        fclose(inputFile);
        return -1;
    }

    fclose(inputFile);

    return 0;
}