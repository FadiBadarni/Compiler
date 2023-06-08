%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "lex.yy.c"
    #include <stdbool.h>
    #include <stdarg.h>
    #include <ctype.h>
    #define HASH_TABLE_SIZE 509
    #define EXPR_TABLE_SIZE 509

    // Structure representing a node in the abstract syntax tree
    typedef struct node
    {
        char *token;  // The token associated with the node
        struct node *left;  // Left child of the node
        struct node *right;  // Right child of the node
        char *tac;  // Three-address code associated with this node
    } node;

    // Structure representing an entry in the arguments list of a function
    typedef struct argument_entry
    {
        char *name;  // Name of the argument
        char *type;  // Type of the argument
        struct argument_entry *next;  // Pointer to the next argument in the list
    } argument_entry;

    // Structure representing an entry in the symbol table
    typedef struct symbol_table_entry
    {
        char *name;  // Name of the symbol (variable or function name)
        char *type;  // Type of the symbol (could be variable type or function return type)
        argument_entry *arguments;  // If symbol is a function, this is its arguments list
        char *return_type;  // Return type of the function, if the symbol is a function
        int hasReturnStatement;  // Flag indicating if the function has a return statement
        struct symbol_table_entry *next;  // Pointer to the next entry in the symbol table
    } symbol_table_entry;

    // Structure representing the symbol table
    typedef struct symbol_table
    {
        symbol_table_entry *head;  // Pointer to the first entry in the table
        struct symbol_table *prev;  // Pointer to the previous symbol table in the stack
    } symbol_table;

    // Structure representing a node in the stack of function names
    typedef struct function_stack_node
    {
        char* function_name;  // Name of the function
        struct function_stack_node* next;  // Pointer to the next function in the stack
    } function_stack_node;

    int labelCounter = 0;  // Counter for generating unique labels
    int tempVarCounter = 0;  // Counter for generating unique temporary variables
    int printlevel = 0;  // Current level of printing (used for indentation)

    // Arrays for tracking status of functions and variables
    bool calledFunctions[HASH_TABLE_SIZE] = {false}; // Hashtable to check if functions were called
    bool declaredVariables[HASH_TABLE_SIZE] = {false}; // Hashtable to check if variables were declared
    bool usedVariables[HASH_TABLE_SIZE] = {false}; // Hashtable to check if variables were used

    char* variableNames[HASH_TABLE_SIZE] = {NULL}; // Array to store variable names
    char* expressionTable[EXPR_TABLE_SIZE] = {NULL}; // Array to store expressions

    symbol_table *current_table = NULL; // Pointer to the current (top) symbol table in the stack
    function_stack_node** functionsStack = NULL; // Pointer to the top of the functions stack
    function_stack_node** temporaryStack = NULL; // Pointer to a temporary stack (used in stack operations)

    node *root;  // Root of the abstract syntax tree

    // Creates a new syntax tree node with given token, left and right children
    node* createNode(char* token, node *left, node *right);

    // Creates a new symbol table
    symbol_table* createSymbolTable();

    // Returns the depth of the current symbol table
    int getSymbolTableDepth();

    // Prints the given symbol table
    void printSymbolTable(symbol_table* table);

    // Pushes the given symbol table onto the symbol table stack
    void pushSymbolTable(symbol_table *table);

    // Pops the topmost symbol table from the symbol table stack
    void popSymbolTable();

    // Looks up a symbol in the current scope's symbol table
    symbol_table_entry* lookupSymbolTableInCurrentScope(char *name);

    // Looks up a symbol in all scopes' symbol tables, starting from the current scope
    symbol_table_entry* lookupSymbolTable(char *name);

    // Adds a new entry to the symbol table
    int addSymbolTableEntry(char *name, char *type);

    // Sets the return type of a given function in the symbol table
    void setFunctionReturnType(char *functionName, char *returnType);

    // Returns the return type of a given function
    char* getFunctionReturnType(char *functionName);

    // Adds an argument to a function in the symbol table
    void addArgumentToFunction(char *functionName, char *argumentName, char *argumentType);

    // Processes a list of identifiers (used when declaring variables or function arguments)
    void processIdentifiers(node* idList, char* argumentType);

    // Counts the number of arguments for a given symbol table entry
    int countArguments(symbol_table_entry *entry);

    // Counts the number of nodes in a syntax tree
    int countNodes(node *n);

    // Checks if a string is a numeric literal
    int isNumericLiteral(const char* str);

    // Checks if a string is a character literal
    int isCharLiteral(const char* str);

    // Checks if a string is a string literal
    int isStringLiteral(const char* str);

    // Checks if a string is a real (floating point) literal
    int isRealLiteral(const char* str);

    // Checks if a string is a unary operator
    int isUnaryOperator(const char* token);

    // Checks the types of the operands in a binary operation, returns the resulting type
    char* checkBinaryOperationType(node *left, node *right, char *operation);

    // Checks the type of the operand in a unary operation, returns the resulting type
    char* checkUnaryOperationType(node *operand, char *operation);

    // Checks if a string is an operator
    bool isOperator(char* token);

    // Returns the type of a syntax tree node
    char* getNodeType(node *n);

    // Returns the type of an expression syntax tree
    char* getTypeOfExpression(node* expr);

    // Reverses a syntax tree
    node* reverseTree(node* root);

    // Prints the contents of a function stack
    void printStack(function_stack_node* stack);

    // Pushes a function name onto a function stack
    void push(function_stack_node** stack, char* function_name);

    // Pops the top function from a function stack and returns its name
    char* pop(function_stack_node** stack);

    // Returns the name of the top function on a function stack without popping it
    char* peek(function_stack_node* stack);

    // Computes the hash of a string
    unsigned long hash(char* str);

    // Marks a function as called
    void addCalledFunction(char* functionName);

    // Checks if a function was called
    bool isFunctionCalled(char* functionName);

    // Gathers all called functions from a syntax tree
    void gatherCalledFunctions(node* root);

    // Marks a variable as declared
    void addDeclaredVariable(char* varName);

    // Marks a variable as used
    void useVariable(char* varName);

    // Prints the contents of the hash tables
    void printHashTable();

    // Checks if a piece of code is dead code
    bool isDeadCode(symbol_table* table, function_stack_node* functionStack, node* root);

    // Checks if a string is an operator (version 2)
    int isOperator2(char* token);

    // Computes the hash of an expression
    unsigned int hashExpression(char* expr);

    // Prints indentation spaces
    void indent(int n);

    // Prints a syntax tree
    void printTree(node *tree);

    // Prints the three-address code of a syntax tree
    void printThreeAddressCode(node *tree, int indentLevel);

    // Frees the memory occupied by a syntax tree node
    void freeNode(node* n);

    // Performs a binary operation on two values
    double performOperation(char* op, double leftVal, double rightVal);

    // Converts a numeric result to a string
    char* convertResultToString(double result);

    // Updates the token of the root node of a syntax tree
    void updateRootNode(node* root, char* resultStr);

    // Performs constant folding optimization on a syntax tree
    node* constantFoldingOptimization(node* root);

    // Lexer function, returns the next token from the input
    int yylex();

    // Error handling function, prints an error message and exits the program
    int yyerror(const char *fmt, ...);

%}

%union
{
    struct node *node;
    char *string;
}

// Token definitions for operators
%token <string> DIVISION PLUS MINUS MULTI
%token <string> AND OR EQUALS INCREMENT

// Token definitions for literals
%token <string> INT_LITERAL CHAR_LITERAL STRING_LITERAL BOOL_LITERAL REAL_LITERAL

// Token definitions for data types
%token <string> BOOL CHAR INT REAL STRING VOID

// Token definitions for variable handling and assignment
%token <string> VAR IDENTIFIER ASSIGNMENT SEMICOLON COLON ARROW COMMA PIPE LBRACKET RBRACKET

// Token definitions for function-related keywords
%token <string> FUNCTION MAIN RETURN

// Token definitions for pointer-related keywords
%token <string> NULL_PTR POINTER_TYPE ADDRESS

// Token definitions for control flow structures
%token <string> IF ELSE WHILE DO FOR

// Token definitions for comparison operators
%token <string> GT GTE LT LTE NOT NEQ

// Token definitions for parentheses and braces
%token LPAREN RPAREN LBRACE RBRACE

// Definitions for nonterminal symbols, specifying what type of value they will hold
%type <node> statement statements_list expression function_call function_call_arguments
%type <node> subroutines subroutine main arguments arguments_list argument identifiers_list type
%type <node> program return_type relational assignment
%type <node> code_block if_statement while_statement do_while_statement for_statement factor term unary atom

// Operator precedence, from highest to lowest
%right ASSIGNMENT
%left MULTI DIVISION
%left PLUS MINUS
%left LT GT LTE GTE
%left NEQ EQUALS
%left AND
%left OR
%nonassoc NOT

%%

/* The entry point of the grammar. A program consists of a set of subroutines and a main function. */
program:
    subroutines main {
        root = createNode("program", $1, $2);
            if (!isDeadCode(current_table, *temporaryStack, root)) {
                printf("There is no dead code in the program.\n");
            }
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
    FUNCTION IDENTIFIER LPAREN RPAREN COLON return_type
        {
            /* Check if a function with the same name is already declared in the current scope */
            symbol_table_entry *existingEntry = lookupSymbolTableInCurrentScope($2);
            if (existingEntry != NULL) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Function '%s' is already declared in the current scope", $2);
                yyerror(errorMessage);
                YYABORT;
            }

            /* Add the function name to the symbol table */
            addSymbolTableEntry($2, "function");

            /* Push the function name onto the functionsStack */
            push(functionsStack, strdup($2));
            push(temporaryStack, strdup($2));

            /* When we start a new function, we enter a new scope.
            So we create a new symbol table and push it onto the stack. */
            symbol_table *newTable = createSymbolTable();
            if (newTable == NULL) {
                yyerror("Failed to create symbol table");
                YYABORT;
            }
            pushSymbolTable(newTable);

             /* Add the function name to its own symbol table (the one we just pushed) */
            addSymbolTableEntry($2, "function");

            setFunctionReturnType(peek(*functionsStack), $6->token);

        }
    LBRACE statements_list
        {
            /* When we're done with the function, we exit its scope,
            so we pop its symbol table off the stack. */
            popSymbolTable();
        }
    RBRACE
        {
            /* Pop the function name from the functionsStack since we're done parsing the function */
            free(pop(functionsStack));

            $$ = createNode("procedure", createNode($2, NULL, NULL), createNode("body", $9, NULL));
        }
    | FUNCTION IDENTIFIER LPAREN
        {
            /* Check if a function with the same name is already declared in the current scope */
            symbol_table_entry *existingEntry = lookupSymbolTableInCurrentScope($2);
            if (existingEntry != NULL) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Function '%s' is already declared in the current scope", $2);
                yyerror(errorMessage);
                YYABORT;
            }

            /* Add the function name to the symbol table */
            addSymbolTableEntry($2, "function");

            /* Push the function name onto the functionsStack */
            push(functionsStack, strdup($2));
            push(temporaryStack, strdup($2));

            /* When we start a new function, we enter a new scope.
            So we create a new symbol table and push it onto the stack. */
            symbol_table *newTable = createSymbolTable();
            if (newTable == NULL) {
                yyerror("Failed to create symbol table");
                YYABORT;
            }
            pushSymbolTable(newTable);
        }
    arguments RPAREN COLON return_type
        {
            /* Extract the return type */
            setFunctionReturnType(peek(*functionsStack), $8->token);

        }
    LBRACE statements_list RBRACE
        {
            /* Retrieve the function's entry from the symbol table */
            symbol_table_entry *functionEntry = lookupSymbolTable(peek(*functionsStack));
            if (functionEntry == NULL) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Function '%s' not found in the symbol table", peek(*functionsStack));
                yyerror(errorMessage);
                YYABORT;
            }

            /* Check if a function that should have a return statement does not */
            char *functionReturnType = getFunctionReturnType(peek(*functionsStack));

            if(strcmp(functionReturnType, "void") != 0 && functionEntry->hasReturnStatement == 0) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Function '%s' must have a return statement", peek(*functionsStack));
                yyerror(errorMessage);
                YYABORT;
            }

            /* When we're done with the function, we exit its scope,
            so we pop its symbol table off the stack. */
            popSymbolTable();

            free(pop(functionsStack));

            $$ = createNode("function", createNode($2, createNode("arguments", $5, NULL), createNode("return_type", $8, NULL)), createNode("body", $11, NULL));
        }
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

            /* Add the main function name to the symbol table */
            addSymbolTableEntry("main", "function");

            /* Push the function name to the stack */
            push(functionsStack, strdup("main"));

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
            /* Pop the function name from the stack */
            free(pop(functionsStack));

            $$ = createNode("procedure", createNode("main", NULL, NULL), createNode("body", $9, NULL));
        }
    ;

return_type:
    VOID { $$ = createNode("void", NULL, NULL); }
    | type
    ;


/* Represents the list of arguments in a function or procedure definition. If there are no arguments, a 'none' argument node is created. */
arguments:
    arguments_list { $$ = $1; }  /* One or more arguments. */
    ;

/* Used to parse the list of arguments. */
arguments_list:
    argument { $$ = createNode("argument", $1, NULL); }  /* Single argument */
    | arguments_list SEMICOLON argument { $$ = createNode("argument", $1, $3); } /* Multiple arguments */
    ;


/* Used to parse individual arguments. */
argument:
    IDENTIFIER ARROW identifiers_list COLON POINTER_TYPE
        {
            /* Add argument to symbol table */
            char *argumentName = $1;
            char *argumentType = $5;

            // Process identifiers_list
            processIdentifiers($3, argumentType);

            $$ = createNode("argument", createNode(argumentType, NULL, NULL), $3);
        }
    | IDENTIFIER ARROW identifiers_list COLON type
        {
            /* Add argument to symbol table */
            char *argumentName = $1;
            struct node *argumentType = $5;

            // Process identifiers_list
            processIdentifiers($3, argumentType->token);

            $$ = createNode("argument", createNode(argumentType->token, NULL, NULL), $3);
        }
    ;

/* Used to parse lists of identifiers, separated by commas. */
identifiers_list:
    IDENTIFIER {  $$ = createNode(strdup($1), NULL, NULL); }
    | IDENTIFIER COMMA identifiers_list { $$ = createNode(strdup($1), NULL, $3); }
;

/* Used to parse lists of statements. */
statements_list:
    statement  { $$ = createNode("statements", $1, NULL); }
    | statements_list statement  { $$ = createNode("statements", $1, $2); }
    ;


/* Used to parse individual statements, including variable declarations, variable assignments, return statements, and different types of control structures like if, while, do-while, and for loops. */
statement:
     VAR identifiers_list COLON POINTER_TYPE SEMICOLON
        {
            $$ = createNode("declare_pointer", $2, createNode($4, NULL, NULL));

            node* id_node = $2;
            while(id_node != NULL) {
                char *type = malloc(strlen($4) + 2);
                strcat(type, $4);
                if (addSymbolTableEntry(id_node->token, type) == -1) {
                    char errorMessage[150];
                    sprintf(errorMessage, "Error: Variable '%s' redeclaration or memory allocation error.", id_node->token);
                    yyerror(errorMessage);
                    YYABORT;
                }
                id_node = id_node->left;
                free(type);
            }
        }
    | VAR identifiers_list COLON type SEMICOLON
        {
            $$ = createNode("declare", $2, $4);

            // Assuming $2 is a linked list of identifier names
            node* id_node = $2;
            while(id_node != NULL) {
                if (addSymbolTableEntry(id_node->token, $4->token) == -1) {
                    char errorMessage[150];
                    sprintf(errorMessage, "Error: Variable '%s' redeclaration or memory allocation error.", id_node->token);
                    yyerror(errorMessage);
                    YYABORT;
                }
                // Add variable to declared list
                addDeclaredVariable(id_node->token);
                id_node = id_node->right;
            }
        }
    | IDENTIFIER ASSIGNMENT expression SEMICOLON
        {
            $$ = createNode("=", createNode($1, NULL, NULL), $3);

            // Check if variable has been declared
            symbol_table_entry* entry = lookupSymbolTable($1);
            if (entry == NULL) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Variable '%s' not defined.", $1);
                yyerror(errorMessage);
                YYABORT;
            }

            // Mark variable as used
            useVariable($1);

            // Check if the type of the expression matches the type of the variable
            char* expression_type = getTypeOfExpression($3);
            if (expression_type == NULL) {
                // An error message would have been printed by getTypeOfExpression
                YYABORT;
            }

            if (strcmp(entry->type, expression_type) != 0) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Type mismatch in assignment to variable '%s'. Expected: '%s', Found: '%s'.", $1, entry->type, expression_type);
                yyerror(errorMessage);
                YYABORT;
            }

            // Check if expression is a function call and perform the necessary actions
            if(strcmp($3->token, "call") == 0) {
                // Retrieve the function's return type
                char* functionReturnType = getFunctionReturnType($3->left->token);
                if (functionReturnType == NULL) {
                    char errorMessage[150];
                    sprintf(errorMessage, "Error: Failed to retrieve return type for function '%s'.", $3->left->token);
                    yyerror(errorMessage);
                    YYABORT;
                }

                // Check if the return type of the function matches the type of the identifier
                if (strcmp(entry->type, functionReturnType) != 0) {
                    char errorMessage[150];
                    sprintf(errorMessage, "Error: Type mismatch in assignment to variable '%s'. Expected: '%s', Found: '%s'.", $1, entry->type, functionReturnType);
                    yyerror(errorMessage);
                    YYABORT;
                }
            }
        }
    | MULTI IDENTIFIER ASSIGNMENT expression SEMICOLON
        {
            // Check if variable has been declared
            symbol_table_entry* entry = lookupSymbolTable($2);
            if (entry == NULL) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Variable '%s' not defined.", $2);
                yyerror(errorMessage);
                YYABORT;
            }
            if (entry->type[strlen(entry->type) - 1] != '*') {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Variable '%s' is not a pointer.", $2);
                yyerror(errorMessage);
                YYABORT;
            }

            // Create a new string to store the type pointed to by the pointer
            char* pointed_type = strdup(entry->type);
            pointed_type[strlen(pointed_type) - 1] = '\0'; // Remove the '*' at the end of the string

            // Check if the type of the expression matches the type pointed to by the pointer
            char* expression_type = getTypeOfExpression($4);
            if (expression_type == NULL) {
                // An error message would have been printed by getTypeOfExpression
                free(pointed_type);
                YYABORT;
            }
            if (strcmp(pointed_type, expression_type) != 0) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Type mismatch in assignment to pointer variable '%s'. Expected: '%s', Found: '%s'.", $2, pointed_type, expression_type);
                yyerror(errorMessage);
                free(pointed_type);
                YYABORT;
            }
            free(pointed_type);

            $$ = createNode("=", createNode("*", createNode($2, NULL, NULL), NULL), $4);
        }
    //TODO: MIGHT NEED CHANGING IN SYNTAX
    | type IDENTIFIER LBRACKET INT_LITERAL RBRACKET SEMICOLON
        {
            if (current_table == NULL) {
                yyerror("Error: Symbol table is not initialized\n");
                YYABORT;
            }

            if ($1 == NULL || $2 == NULL) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Null values provided for declaration.");
                yyerror(errorMessage);
                YYABORT;
            }

            symbol_table_entry* entry = lookupSymbolTableInCurrentScope($2);
            if (entry != NULL) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Variable '%s' already declared in current scope.", $2);
                yyerror(errorMessage);
                YYABORT;
            }

            addSymbolTableEntry($2, $1->token);
            $$ = createNode("declare_string", createNode($2, NULL, NULL), createNode($4, NULL, NULL));
        } /* Strings declarations */
    | type IDENTIFIER LBRACKET INT_LITERAL RBRACKET ASSIGNMENT STRING_LITERAL SEMICOLON
        { $$ = createNode("declare_initialize_string", createNode($2, NULL, NULL), createNode("initialize_data", createNode("size", createNode($4, NULL, NULL), NULL), createNode("value", createNode($7, NULL, NULL), NULL))); }
    | IDENTIFIER LBRACKET expression RBRACKET ASSIGNMENT CHAR_LITERAL SEMICOLON
        { $$ = createNode("array_assign", createNode("array_index", createNode($1, NULL, NULL), $3), createNode($7, NULL, NULL)); } /* String element assignments */
    | RETURN expression SEMICOLON
        {
            /* Retrieve the function's entry from the symbol table */
            symbol_table_entry *functionEntry = lookupSymbolTable(peek(*functionsStack));
            if (functionEntry == NULL) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Function '%s' not found in the symbol table.", peek(*functionsStack));
                yyerror(errorMessage);
                YYABORT;
            }

            functionEntry->hasReturnStatement = 1;

            /* Peek the top of the function stack to get the name of the current function */
            char* currentFunction = peek(*functionsStack);

            /* Check if currentFunction is main */
            if (strcmp(currentFunction, "main") == 0) {
                yyerror("Error: Main function should not have a return statement");
                YYABORT;
            }

            /* Get the expected return type of the function */
            char* expectedReturnType = functionEntry->return_type;

            /* Get the type of the actual return expression */
            char* actualReturnType = getTypeOfExpression($2);

            /* Compare the expected return type with the actual return type */
            if (strcmp(expectedReturnType, actualReturnType) != 0) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Type mismatch in return statement for function '%s'. Expected '%s' but found '%s'.", currentFunction, expectedReturnType, actualReturnType);
                yyerror(errorMessage);
                YYABORT;
            }

            $$ = createNode("return", $2, NULL); // handle return statement
        }
    | subroutine
        {
            pushSymbolTable(createSymbolTable()); // create new scope for the subroutine

            $$ = createNode("nested_function", $1, NULL); // handle nested function
            popSymbolTable(); // end the scope for the subroutine
        }
    | function_call SEMICOLON
    | code_block
    | if_statement
    | while_statement
    | do_while_statement
    | for_statement
    ;


/* non-terminal for a function call. */
function_call:
    IDENTIFIER LPAREN RPAREN
        {
            symbol_table_entry *existingEntry = lookupSymbolTable($1);
            if (existingEntry == NULL) {
                yyerror("Function '%s' not defined", $1);
                YYABORT;
            }
            if (countArguments(existingEntry) != 0) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Wrong number of arguments in function call for '%s'. Expected 0 but found %d.", $1, countArguments(existingEntry));
                yyerror(errorMessage);
                YYABORT;
            }
            $$ = createNode("call", createNode($1, NULL, NULL), NULL);
        } // handle function call without arguments
    | IDENTIFIER LPAREN function_call_arguments RPAREN
        {
            symbol_table_entry *existingEntry = lookupSymbolTable($1);
            if (existingEntry == NULL) {
                yyerror("Function '%s' not defined", $1);
                YYABORT;
            }
            int numCallArgs = countNodes($3);
            if (countArguments(existingEntry) != numCallArgs) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Wrong number of arguments in function call for '%s'. Expected %d but got %d.", $1, countArguments(existingEntry), numCallArgs);
                yyerror(errorMessage);
                YYABORT;
            }

            argument_entry *argument = existingEntry->arguments;
            node *callArgument = $3;

            callArgument = reverseTree(callArgument);

            while (argument != NULL && callArgument != NULL) {
                if (strcmp(argument->type, getTypeOfExpression(callArgument->left)) != 0) {
                    char errorMessage[150];
                    sprintf(errorMessage, "Error: Argument type mismatch in function call for '%s'. Expected type '%s' but got '%s'.", $1, argument->type, getTypeOfExpression(callArgument->left));
                    yyerror(errorMessage);
                    YYABORT;
                }
                argument = argument->next;
                callArgument = callArgument->right;
            }

            $$ = createNode("call", createNode($1, createNode("arguments", $3, NULL), NULL), NULL);
        } // handle function call with arguments
;

function_call_arguments:
    expression { $$ = createNode("argument", $1, NULL); } // single argument
    | function_call_arguments COMMA expression { $$ = createNode("argument", $3, $1); } // multiple arguments
;

code_block:
    LBRACE // Beginning of a new code block
    {
        // A new symbol table is created and pushed onto the stack
        // This will hold all variables declared in the new scope
        pushSymbolTable(createSymbolTable());
    }
    statements_list // variable declarations
    RBRACE // End of code block
    {
        // Create a block node and associate it with the statements within the block
        $$ = createNode("block", $3, NULL);

        // As we're exiting the block, we pop its symbol table off the stack
        // This effectively destroys the block's scope
        popSymbolTable();
    }
    | LBRACE RBRACE { $$ = createNode("block_empty", NULL, NULL); }
    ;



/* Used to parse if-else statements as well as standalone if statements. */
if_statement:
    IF LPAREN expression RPAREN LBRACE statements_list RBRACE ELSE LBRACE statements_list RBRACE
        {
            /* Check if the expression in the condition is of type bool */
            char *expressionType = getTypeOfExpression($3);
            if (strcmp(expressionType, "bool") != 0) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Condition of an if statement must be of type bool, but got %s.\n", expressionType);
                yyerror(errorMessage);
                YYABORT;
            }

            /* Push a new symbol table for if statement scope */
            symbol_table *ifTable = createSymbolTable();
            pushSymbolTable(ifTable);

            node* if_body_node = createNode("if_body", $6, NULL);
            popSymbolTable(); /* Pop the symbol table after exiting if block scope */

            /* Push a new symbol table for else statement scope */
            symbol_table *elseTable = createSymbolTable();
            pushSymbolTable(elseTable);

            node* else_body_node = createNode("else_body", $10, NULL);
            popSymbolTable(); /* Pop the symbol table after exiting else block scope */

            $$ = createNode("if_else", $3, createNode("if_else_wrapper", if_body_node, else_body_node));
        }
    | IF LPAREN expression RPAREN LBRACE statements_list RBRACE
        {
            /* Check if the expression in the condition is of type bool */
            char *expressionType = getTypeOfExpression($3);
            if (strcmp(expressionType, "bool") != 0) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Condition of an if statement must be of type bool, but got %s.\n", expressionType);
                yyerror(errorMessage);
                YYABORT;
            }

            /* Push a new symbol table for if statement scope */
            symbol_table *ifTable = createSymbolTable();
            pushSymbolTable(ifTable);

            $$ = createNode("if", $3, createNode("if_body", $6, NULL));

            popSymbolTable(); /* Pop the symbol table after exiting if block scope */
        }
;

/* Used to parse while loops. */
while_statement:
    WHILE LPAREN expression RPAREN LBRACE statements_list RBRACE
        {
            /* Check if the expression in the condition is of type bool */
            char *expressionType = getTypeOfExpression($3);

            if (strcmp(expressionType, "bool") != 0) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Condition of a while statement must be of type bool, but got %s.\n", expressionType);
                yyerror(errorMessage);
                YYABORT;
            }

            /* Push a new symbol table for while loop scope */
            symbol_table *whileTable = createSymbolTable();
            pushSymbolTable(whileTable);

            $$ = createNode("while", $3, createNode("body", $6, NULL));

            popSymbolTable(); /* Pop the symbol table after exiting while loop scope */
        }
;

/* Used to parse do-while loops. */
do_while_statement:
    DO LBRACE statements_list RBRACE WHILE LPAREN expression RPAREN SEMICOLON
        {
            /* Check if the expression in the condition is of type bool */
            char *expressionType = getTypeOfExpression($7);

            if (strcmp(expressionType, "bool") != 0) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Condition of a do-while statement must be of type bool, but got %s.\n", expressionType);
                yyerror(errorMessage);
                YYABORT;
            }

            /* Push a new symbol table for do-while loop scope */
            symbol_table *doWhileTable = createSymbolTable();
            pushSymbolTable(doWhileTable);

            $$ = createNode("do_while", $3, $7);

            popSymbolTable(); /* Pop the symbol table after exiting do-while loop scope */
        }
;

/* Used to parse for loops. */
for_statement:
    FOR LPAREN expression SEMICOLON expression SEMICOLON expression RPAREN LBRACE statements_list RBRACE
        {
            /* Check if the expression in the condition is of type bool */
            char *expressionType = getTypeOfExpression($5);

            if (strcmp(expressionType, "bool") != 0) {
                char errorMessage[150];
                sprintf(errorMessage, "Error: Condition of a for statement must be of type bool, but got %s.\n", expressionType);
                yyerror(errorMessage);
                YYABORT;
            }

            /* Push a new symbol table for for loop scope */
            symbol_table *forTable = createSymbolTable();
            pushSymbolTable(forTable);

            node* initialization = $3;
            node* condition = $5;
            node* increment = $7;
            node* body = $10;

            /* Build the nodes in stages */
            node* initialization_node = createNode("for_initialization", initialization, NULL);
            node* condition_node = createNode("for_condition", condition, NULL);
            node* increment_node = createNode("for_increment", increment, NULL);
            node* body_node = createNode("for_body", body, NULL);

            /* Combine the four nodes into one */
            $$ = createNode("for_statement",createNode("for_header",initialization_node,createNode("for_conditions",condition_node,increment_node)),body_node);

            popSymbolTable(); /* Pop the symbol table after exiting for loop scope */
        }
    ;

expression:
    /* Logical operations */
    expression AND relational {
        if(checkBinaryOperationType($1, $3, "&&") == NULL)
            YYABORT;
        $$ = createNode("&&", $1, $3);
    }
    | expression OR relational {
        if(checkBinaryOperationType($1, $3, "||") == NULL)
            YYABORT;
        $$ = createNode("||", $1, $3);
    }
    | assignment
    | relational
    | function_call
    ;

assignment:
    IDENTIFIER ASSIGNMENT expression {
        symbol_table_entry* entry = lookupSymbolTable($1);
        if (entry == NULL) {
            yyerror("Undeclared identifier: %s", $1);
            YYABORT;
        }
        $$ = createNode("=", createNode($1, NULL, NULL), $3);
    }
;


relational:
    /* Relational operations */
    relational EQUALS term {
        if(checkBinaryOperationType($1, $3, "==") == NULL)
            YYABORT;
        $$ = createNode("==", $1, $3);
    }
    | relational NEQ term {
        if(checkBinaryOperationType($1, $3, "!=") == NULL)
            YYABORT;
        $$ = createNode("!=", $1, $3);
    }
    | relational LT term {
        if(checkBinaryOperationType($1, $3, "<") == NULL)
            YYABORT;
        $$ = createNode("<", $1, $3);
    }
    | relational GT term {
        if(checkBinaryOperationType($1, $3, ">") == NULL)
            YYABORT;
        $$ = createNode(">", $1, $3);
    }
    | relational LTE term {
        if(checkBinaryOperationType($1, $3, "<=") == NULL)
            YYABORT;
        $$ = createNode("<=", $1, $3);
    }
    | relational GTE term {
        if(checkBinaryOperationType($1, $3, ">=") == NULL)
            YYABORT;
        $$ = createNode(">=", $1, $3);
    }
    | term
    ;

term:
    /* Addition and Subtraction operations */
    term PLUS factor {
        if(checkBinaryOperationType($1, $3, "+") == NULL)
            YYABORT;
        $$ = createNode("+", $1, $3);
    }
    | term MINUS factor {
        if(checkBinaryOperationType($1, $3, "-") == NULL)
            YYABORT;
        $$ = createNode("-", $1, $3);
    }
    | factor
    ;

factor:
    /* Multiplication and Division operations */
    factor MULTI unary {
        if(checkBinaryOperationType($1, $3, "*") == NULL)
            YYABORT;
        $$ = createNode("*", $1, $3);
    }
    | factor DIVISION unary {
        if(checkBinaryOperationType($1, $3, "/") == NULL)
            YYABORT;
        $$ = createNode("/", $1, $3);
    }
    | unary

    ;

unary:
    /* Unary operation */
    NOT unary {
        if(checkUnaryOperationType($2, "!") == NULL)
            YYABORT;
        $$ = createNode("!", $2, NULL);
    }
    | PIPE expression PIPE {
        char* exprType = getTypeOfExpression($2);
        if (exprType == NULL) {
            char errorMessage[150];
            sprintf(errorMessage, "Error: Invalid expression type for 'abs' operation.\n");
            yyerror(errorMessage);
            YYABORT;
        }
        if(strcmp(exprType, "string") != 0) {
            char errorMessage[150];
            sprintf(errorMessage, "Error: Invalid type for 'abs' operation. Operand must be of type string, but got %s.\n", exprType);
            yyerror(errorMessage);
            YYABORT;
        }
        $$ = createNode("abs", $2, NULL);
    }
    | MULTI IDENTIFIER {
        // Check if variable has been declared
        symbol_table_entry* entry = lookupSymbolTable($2);
        if (entry == NULL) {
            char errorMessage[150];
            sprintf(errorMessage, "Error: Undefined variable: '%s'.\n", $2);
            yyerror(errorMessage);
            YYABORT;
        }
        if (entry->type[strlen(entry->type) - 1] != '*') {
            char errorMessage[150];
            sprintf(errorMessage, "Error: Invalid operation on '%s'. It is not a pointer type.\n", $2);
            yyerror(errorMessage);
            YYABORT;
        }
        $$ = createNode("*", createNode($2, NULL, NULL), NULL);
    }
    | MULTI LPAREN expression RPAREN {
        // Check if the expression is of a pointer type
        char* exprType = getTypeOfExpression($3);
        if (exprType == NULL || exprType[strlen(exprType) - 1] != '*') {
            char errorMessage[150];
            sprintf(errorMessage, "Error: Invalid type for dereferencing operation. Operand must be of pointer type, but got %s.\n", exprType);
            yyerror(errorMessage);
            YYABORT;
        }
        $$ = createNode("*", $3, NULL);
    }
    | IDENTIFIER INCREMENT {
        symbol_table_entry* entry = lookupSymbolTable($1);
        if (entry == NULL) {
            yyerror("Undeclared identifier: %s", $1);
            YYABORT;
        }
        if(strcmp(entry->type, "int") != 0) {
            yyerror("Increment operation only applicable to integer type");
            YYABORT;
        }
        node* identifier_node = createNode("identifier", createNode($1, NULL, NULL), NULL);
        $$ = createNode("post_inc", identifier_node, createNode("increment", NULL, NULL));
    }
    | atom
    ;

atom:
    /* Literals and Identifiers */
    IDENTIFIER
        {
            symbol_table_entry* entry = lookupSymbolTable($1);
            if (entry == NULL) {
                yyerror("Undeclared identifier: %s", $1);
                YYABORT;
            }
            // Mark variable as used
            useVariable($1);

            $$ = createNode($1, NULL, NULL);
        }
    | INT_LITERAL { $$ = createNode($1, NULL, NULL); }
    | CHAR_LITERAL { $$ = createNode($1, NULL, NULL); }
    | STRING_LITERAL { $$ = createNode($1, NULL, NULL); }
    | BOOL_LITERAL { $$ = createNode($1, NULL, NULL); }
    | REAL_LITERAL { $$ = createNode($1, NULL, NULL); }
    /* Type Keywords */
    | CHAR { $$ = createNode($1, NULL, NULL); }
    | INT { $$ = createNode($1, NULL, NULL); }
    | REAL { $$ = createNode($1, NULL, NULL); }
    | STRING { $$ = createNode($1, NULL, NULL); }
    | BOOL { $$ = createNode($1, NULL, NULL); }
    | VOID { $$ = createNode($1, NULL, NULL); }
    /* Special Expressions */
    | NULL_PTR { $$ = createNode("null", NULL, NULL); }
    | POINTER_TYPE { $$ = createNode($1, NULL, NULL); }
    | ADDRESS IDENTIFIER { $$ = createNode("&", createNode($2, NULL, NULL), NULL); }
    | ADDRESS IDENTIFIER LBRACKET expression RBRACKET {
        symbol_table_entry* entry = lookupSymbolTable($2);
        if (entry == NULL) {
            char errorMessage[150];
            sprintf(errorMessage, "Error: Undeclared identifier: '%s'.\n", $2);
            yyerror(errorMessage);
            YYABORT;
        }
        if (strcmp(entry->type, "string") != 0) {
            char errorMessage[150];
            sprintf(errorMessage, "Error: Invalid operation on '%s'. It is not a string array.\n", $2);
            yyerror(errorMessage);
            YYABORT;
        }
        $$ = createNode("ARRAY_ELEMENT", createNode($2, NULL, NULL), $4);
    }
    | IDENTIFIER LBRACKET expression RBRACKET { $$ = createNode("array_index", createNode($1, NULL, NULL), $3); }
    /* Parenthesized Expression */
    | LPAREN expression RPAREN { $$ = $2; }
    ;

type:
    BOOL { $$ = createNode("bool", NULL, NULL); }
    | CHAR { $$ = createNode("char", NULL, NULL); }
    | INT { $$ = createNode("int", NULL, NULL); }
    | REAL { $$ = createNode("real", NULL, NULL); }
    | STRING { $$ = createNode("string", NULL, NULL); }
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

void printSymbolTable(symbol_table* table)
{
    printf("Symbol Table:\n");
    printf("+----------------------+----------------------+----------------------+\n");
    printf("|        Name          |        Type          |   Return Type (Fn)   |\n");
    printf("+----------------------+----------------------+----------------------+\n");

    symbol_table_entry* entry = table->head;
    while (entry != NULL)
    {
        if (strcmp(entry->type, "function") == 0) {
            printf("| %-20s | %-20s | %-20s |\n", entry->name, entry->type, entry->return_type != NULL ? entry->return_type : "N/A");
        } else {
            printf("| %-20s | %-20s | %-20s |\n", entry->name, entry->type, "N/A");
        }
        // Print a separator between entries
        printf("+----------------------+----------------------+----------------------+\n");

        entry = entry->next;
    }
}

void pushSymbolTable(symbol_table *table)
{
    table->prev = current_table;
    current_table = table;
}

void popSymbolTable()
{
    symbol_table *table = current_table;
    current_table = current_table->prev;

    symbol_table_entry* entry = table->head;
    while (entry != NULL)
    {
        symbol_table_entry* next = entry->next;
        free(entry->name);
        free(entry->type);
        free(entry);
        entry = next;
    }

    free(table);
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

int addSymbolTableEntry(char *name, char *type)
{
    /* Check if the name already exists in the current scope */
    symbol_table_entry *existingEntry = lookupSymbolTableInCurrentScope(name);
    if (existingEntry != NULL) {
        fprintf(stderr, "Error: Name %s is already declared in the current scope\n", name);
        return -1;
    }

    /* If name does not exist in current scope, add it */
    symbol_table_entry *entry = (symbol_table_entry*) malloc(sizeof(symbol_table_entry));
    if (entry == NULL)
    {
        fprintf(stderr, "Error: Unable to allocate memory for symbol table entry\n");
        return -1;
    }

    entry->name = strdup(name);
    entry->type = strdup(type);
    entry->next = current_table->head;
    current_table->head = entry;
    entry->hasReturnStatement = 0;

    printf("Added symbol to table. Current table:\n");
    printSymbolTable(current_table);

    return 0;
}

void setFunctionReturnType(char *functionName, char *returnType) {

    /* Retrieve the function's entry from the symbol table */
    symbol_table_entry *functionEntry = lookupSymbolTable(functionName);

    if (functionEntry == NULL) {
        fprintf(stderr, "Error: Function not found in the symbol table\n");
        return;
    }
    /* Set the return type */
    functionEntry->return_type = strdup(returnType);

}

char* getFunctionReturnType(char *functionName) {
    /* Find the function in the symbol table */
    symbol_table_entry *entry = lookupSymbolTable(functionName);

    /* If the function does not exist in the symbol table, return NULL */
    if (entry == NULL) {
        fprintf(stderr, "Error: Function %s is not declared in any scope\n", functionName);
        return NULL;
    }

    /* If the function exists but its type is not 'function', return NULL */
    if (strcmp(entry->type, "function") != 0) {
        fprintf(stderr, "Error: %s is not a function\n", functionName);
        return NULL;
    }
    /* If the function exists and its type is 'function', return its return type */
    return entry->return_type;
}

void addArgumentToFunction(char *functionName, char *argumentName, char *argumentType) {
    /* Retrieve the function's entry from the symbol table */
    symbol_table_entry *functionEntry = lookupSymbolTable(functionName);
    if (functionEntry == NULL) {
        fprintf(stderr, "Error: Function not found in the symbol table\n");
        return;
    }

    /* Create a new argument entry */
    argument_entry *newArgument = (argument_entry*) malloc(sizeof(argument_entry));
    if (newArgument == NULL) {
        fprintf(stderr ,"Error: Unable to allocate memory for argument entry");
        return;
    }

    newArgument->name = strdup(argumentName);
    newArgument->type = strdup(argumentType);
    newArgument->next = NULL;

    /* Prepend the new argument to the function's argument list */
    if (functionEntry->arguments == NULL) {
        functionEntry->arguments = newArgument;
    } else {
        argument_entry *arg = functionEntry->arguments;
        while (arg->next != NULL) {
            arg = arg->next;
        }
        arg->next = newArgument;
    }
}

void processIdentifiers(node* idList, char* argumentType) {
    if (idList == NULL) {
        return;
    }
    processIdentifiers(idList->right, argumentType);

    // check if identifier is already in the symbol table
    symbol_table_entry *existingEntry = lookupSymbolTableInCurrentScope(idList->token);
    if (existingEntry != NULL) {
        yyerror("Error: Identifier with this name is already declared in the current scope");
        return;
    }

    // add identifier to the symbol table
    addSymbolTableEntry(idList->token, argumentType);
    addArgumentToFunction(peek(*functionsStack), idList->token, argumentType);
}

int countArguments(symbol_table_entry *entry) {
    int count = 0;
    argument_entry *arg = entry->arguments;
    while(arg != NULL) {
        count++;
        arg = arg->next;
    }
    return count;
}

int countNodes(node *n) {
    int count = 0;
    while (n != NULL) {
        if (strcmp(n->token, "argument") == 0) {
            // Increment count for each argument node
            count++;

            // Move to the next argument (right child)
            n = n->right;
        }
    }
    return count;
}

int isNumericLiteral(const char* str) {
    int start = 0;

    // If the string starts with a '-', it could be a negative number
    // So start checking from the second character
    if (str[0] == '-') {
        start = 1;
    }

    for(int i = start; str[i] != '\0'; i++) {
        if (!isdigit(str[i])) {
            return 0;
        }
    }

    return 1;
}

int isCharLiteral(const char* str) {
    // str[0] should be a single quote, str[1] any character and str[2] a single quote again
    if (strlen(str) == 4 && str[0] == '\'' && str[3] == '\'') {
        // Check if it's a null character
        if (str[1] == '\\' && str[2] == '0') {
            return 1;
        }
    }
    else if (strlen(str) == 3 && str[0] == '\'' && str[2] == '\'') {
        return 1;
    }

    // if the string does not match these patterns, it is not a char literal
    return 0;
}

int isStringLiteral(const char* str) {
    // A string literal is usually enclosed in double quotes (like "foo")
    // so we expect the string to be at least 2 characters long.
    // str[0] should be a double quote and str[strlen(str) - 1] should be a double quote again
    int len = strlen(str);
    if (len >= 2 && str[0] == '\"' && str[len - 1] == '\"') {
        return 1;
    }

    // If the string does not match this pattern, it is not a string literal
    return 0;
}

int isRealLiteral(const char* str) {
    char* endptr;
    strtod(str, &endptr);

    // Check if the string contains a decimal point '.'
    if (strchr(str, '.') == NULL) {
        return 0;
    }

    return *endptr == '\0' && str != endptr && *str != '\0';
}

int isUnaryOperator(const char* token) {
    // Currently only support address-of operator
    return strcmp(token, "&") == 0 || strcmp(token, "*") == 0 || strcmp(token, "ARRAY_ELEMENT") == 0 || strcmp(token, "abs") == 0;
}

char* checkBinaryOperationType(node *left, node *right, char *operation) {
    char* leftType = getNodeType(left);
    char* rightType = getNodeType(right);
    if (leftType == NULL || rightType == NULL) {
        return NULL;
    }
    // Handle arithmetic operations
    if (strcmp(operation, "+") == 0 || strcmp(operation, "-") == 0 ||
        strcmp(operation, "*") == 0 || strcmp(operation, "/") == 0) {
        if((strcmp(leftType, "int") == 0 || strcmp(leftType, "real") == 0) &&
        (strcmp(rightType, "int") == 0 || strcmp(rightType, "real") == 0)) {
            if (strcmp(leftType, "int") == 0 && strcmp(rightType, "int") == 0) {
                return "int";
            } else {
                return "real";
            }
        } else {
            char errorMsg[256];
            sprintf(errorMsg, "Error: Invalid types for operation '%s'. Operands must be int or real. Left operand is '%s', right operand is '%s'",operation, leftType, rightType);
            yyerror(errorMsg);
            return NULL;
        }
    }

    // Handle logical operations
    if (strcmp(operation, "&&") == 0 || strcmp(operation, "||") == 0) {
        if (strcmp(leftType, "bool") == 0 && strcmp(rightType, "bool") == 0) {
            return "bool";
        } else {
            char errorMessage[100];
            sprintf(errorMessage, "Error: Invalid types for operation. Operands must be of type bool, but got '%s' and '%s'", leftType, rightType);
            yyerror(errorMessage);
            return NULL;
        }
    }

    // Handle comparison operations
    if (strcmp(operation, ">") == 0 || strcmp(operation, "<") == 0 ||
        strcmp(operation, "<=") == 0 || strcmp(operation, ">=") == 0) {
        if ((strcmp(leftType, "int") == 0 || strcmp(leftType, "real") == 0) &&
            (strcmp(rightType, "int") == 0 || strcmp(rightType, "real") == 0)) {
            return "bool";
        } else {
            char errorMessage[100];
            sprintf(errorMessage, "Error: Invalid types for operation. Operands must be of type int or real, but got '%s' and '%s'", leftType, rightType);
            yyerror(errorMessage);
            return NULL;
        }
    }

    // Handle equality/inequality operations
    if (strcmp(operation, "==") == 0 || strcmp(operation, "!=") == 0) {
        if (strcmp(leftType, rightType) == 0) {
            return "bool";
        } else {
            // Handle the case of comparing a pointer with a non-pointer
            if ((leftType[0] == '*' && strcmp(leftType, rightType + 1) == 0) ||
                (rightType[strlen(rightType) - 1] == '*' && strcmp(rightType, leftType + 1) == 0)) {
                return "bool";
            }
            char errorMessage[150];
            sprintf(errorMessage, "Error: Invalid types for operation. Operands must be of the same type or compatible pointer types, but got '%s' and '%s'", leftType, rightType);
            yyerror(errorMessage);
            return NULL;
        }
    }

    // Handle array indexing
    if (strcmp(operation, "array_index") == 0) {
        if (strcmp(leftType, "string") == 0 && strcmp(rightType, "int") == 0) {
            return "char";
        }
        // Check if the left operand type ends with '*' (is a pointer)
        else if (leftType[strlen(leftType) - 1] == '*' && strcmp(rightType, "int") == 0) {
            // Copy the left type and remove the last character (*) to get the indexed type
            char *indexedType = strdup(leftType);
            indexedType[strlen(indexedType) - 1] = '\0';
            return indexedType;
        }
        else {
            char errorMessage[150];
            sprintf(errorMessage, "Error: Invalid types for array indexing. Left operand must be of pointer type and right operand must be of type int, but got '%s' and '%s'", leftType, rightType);
            yyerror(errorMessage);
            return NULL;
        }
    }


    // Catch-all for unsupported operations
    yyerror("Error: Unsupported operation: %s", operation);
    return NULL;
}

char* checkUnaryOperationType(node *operand, char *operation) {
    symbol_table_entry *entry = lookupSymbolTable(operand->token);
    if (entry == NULL) {
        char errorMessage[150];
        sprintf(errorMessage, "Error: Undefined variable: %s\n", operand->token);
        yyerror(errorMessage);
        return NULL;
    }
    if (strcmp(operation, "!") == 0 && strcmp(entry->type, "bool") != 0) {
        char errorMessage[150];
        sprintf(errorMessage, "Error: Invalid type for operation '!'. Operand must be of type bool, but got %s\n", entry->type);
        yyerror(errorMessage);
        return NULL;
    }
    if (strcmp(operation, "abs") == 0 && !(strcmp(entry->type, "string") == 0)) {
        char errorMessage[150];
        sprintf(errorMessage, "Error: Invalid type for operation 'abs'. Operand must be of type string, but got %s\n", entry->type);
        yyerror(errorMessage);
        return NULL;
    }
    if (strcmp(operation, "&") == 0) {
        char* type = malloc(strlen(entry->type) + 2); // Allow space for '*' and '\0'
        strcpy(type, entry->type);
        strcat(type, "*");
        return type;
    }
    // Handle dereference operation
    if (strcmp(operation, "*") == 0) {
        if (entry->type[strlen(entry->type) - 1] != '*') {
            char errorMessage[150];
            sprintf(errorMessage, "Error: Invalid type for dereference operation. Operand must be of pointer type, but got %s\n", entry->type);
            yyerror(errorMessage);
                return NULL;
        }
        // remove the last character (*) to get the dereferenced type
        char *dereferenced_type = strdup(entry->type);
        dereferenced_type[strlen(dereferenced_type) - 1] = '\0';
        return dereferenced_type;
    }
    return entry->type;
}

bool isOperator(char* token) {
    char* operators[] = {"&&", "||", "==", "!=", "<", ">", "<=", ">=", "+", "-", "*", "/", "!", "abs"};
    int num_operators = sizeof(operators) / sizeof(operators[0]);

    for (int i = 0; i < num_operators; i++) {
        if (strcmp(token, operators[i]) == 0) {
            return true;
        }
    }

    return false;
}

char* getNodeType(node *n) {
    if (n == NULL) {
        yyerror("Error: Null node encountered when trying to get node type");
        return NULL;
    }
    if (n->left == NULL && n->right == NULL) { // Identifier or literal
        if (isOperator(n->token)) {
            yyerror("Error: Unexpected operator '%s' encountered when a variable or literal was expected", n->token);
            return NULL; // Operators don't have a type in the same sense as variables or literals
        }
        if (isNumericLiteral(n->token)) {
            return "int";
        }
        if (isCharLiteral(n->token)) {
            return "char";
        }
        if (strcmp(n->token, "true") == 0 || strcmp(n->token, "false") == 0) {
            return "bool";
        }
        if (isStringLiteral(n->token)) {
            return "string";
        }
        if (isRealLiteral(n->token)) {
            return "real";
        }
        symbol_table_entry* entry = lookupSymbolTable(n->token);
        if (entry == NULL) {
            yyerror("Error: Identifier '%s' not declared in the current scope", n->token);
            return NULL;
        }
        return entry->type;
    }
    else if (strcmp(n->token, "call") == 0) { // Function call
        // assume the function return type is stored in the symbol table
        symbol_table_entry* entry = lookupSymbolTable(n->left->token); // n->left->token is function name
        if (entry == NULL) {
            yyerror("Error: Function '%s' not declared", n->left->token);
            return NULL;
        }
        return entry->type; // return type of the function
    }
    else if (n->right == NULL) { // Unary operation
        if (!isUnaryOperator(n->token)) {
            yyerror("Error: Operator '%s' not applicable in unary operation", n->token);
            return NULL;
        }
        if (strcmp(n->token, "abs") == 0) {
            return "int";
        }
        return checkUnaryOperationType(n->left, n->token);
    }
    else { // Binary operation
        if (n->left == NULL || n->right == NULL) {
            yyerror("Error: Binary operation '%s' missing an operand", n->token);
            return NULL;
        }
        char* leftType = getNodeType(n->left);
        char* rightType = getNodeType(n->right);

        if (leftType == NULL || rightType == NULL) {
            return NULL; // Error would have been printed already
        }

        // Check the type compatibility of operands with the operation
        return checkBinaryOperationType(n->left, n->right, n->token);
    }
    yyerror("Error: Node '%s' represents an operation but it's missing type at this level", n->token);
    return NULL; // For nodes representing operations, there is no type at this level.
}

char* getTypeOfExpression(node* expr) {
    if (expr == NULL) {
        return NULL;
    }

    /* If it's a binary operation, check the types of both operands */
    if (expr->left && expr->right) {
        // Check if it's array access operation
        if(strcmp(expr->token, "ARRAY_ELEMENT") == 0) {
            char* leftType = getNodeType(expr->left); // This should be the array identifier
            char* rightType = getNodeType(expr->right); // This should be the index
            if (leftType == NULL || rightType == NULL) {
                return NULL; // Error message would have been printed already
            }

            if (strcmp(leftType, "string") != 0 || strcmp(rightType, "int") != 0) {
                yyerror("Invalid array access: array type is %s and index type is %s", leftType, rightType);
                return NULL;
            }

            return "char*"; // The type of an element in a string array is "char"
        }
        else {
            // For other binary operations, proceed as before
            char* leftType = getNodeType(expr->left);
            char* rightType = getNodeType(expr->right);

            if (leftType == NULL || rightType == NULL) {
                return NULL; // Error message would have been printed already
            }

            if (strcmp(leftType, rightType) != 0) {
                yyerror("Type mismatch in binary operation: %s %s", leftType, rightType);
                return NULL;
            }
            // The type of the operation is dependent on the operator
            if (strcmp(expr->token, "<") == 0 || strcmp(expr->token, ">") == 0 ||
                strcmp(expr->token, "==") == 0 || strcmp(expr->token, "!=") == 0 ||
                strcmp(expr->token, ">=") == 0 || strcmp(expr->token, "<=") == 0) {
                return "bool";
            }
            return leftType; // for arithmetic operations, the type of the operation is the same as the operands
        }
    }
    /* If it's a unary operation or a simple identifier, just look up its type in the symbol table */
    else {
        // Check if it's a function call
        if (strcmp(expr->token, "call") == 0) {
            symbol_table_entry *entry = lookupSymbolTable(expr->left->token); // Assuming left node holds function name
            if (entry != NULL && strcmp(entry->type, "function") == 0) {
                return entry->return_type;
            } else {
                yyerror("Function %s is not declared in any scope\n", expr->left->token);
                return NULL;
            }
        }
        if (expr->token[0] == '&') {
            if(strcmp(expr->left->token, "ARRAY_ELEMENT") == 0) {
                symbol_table_entry* entry = lookupSymbolTable(expr->left->left->token);
                if (entry == NULL || strcmp(entry->type, "string") != 0) {
                    yyerror("Variable not defined or not a string array");
                    return NULL;
                }
                return "char*";
            } else {
                symbol_table_entry* entry = lookupSymbolTable(expr->left->token);
                if (entry == NULL) {
                    yyerror("Variable not defined");
                    return NULL;
                }
                char *pointer_type = malloc(strlen(entry->type) + 2);
                strcpy(pointer_type, entry->type);
                strcat(pointer_type, "*");

                return pointer_type;
            }
        } else if (expr->token[0] == '*') {
                symbol_table_entry* entry = lookupSymbolTable(expr->token + 1);
                if (entry == NULL || strlen(entry->type) < 2 || entry->type[strlen(entry->type) - 1] != '*') {
                    yyerror("Variable not defined or not a pointer");
                    return NULL;
                }

                // remove the last character (*) to get the dereferenced type
                char *dereferenced_type = strdup(entry->type);
                dereferenced_type[strlen(dereferenced_type) - 1] = '\0';
                return dereferenced_type;
        } else {
            return getNodeType(expr);
        }
    }
}

node* reverseTree(node* root)
{
    if(root == NULL || root->right == NULL)
    {
        return root;
    }

    node* newRoot = reverseTree(root->right);

    root->right->right = root;
    root->right = NULL;

    return newRoot;
}

void printStack(function_stack_node* stack)
{
    if (stack == NULL) {
        printf("Function stack is empty.\n");
        return;
    }

    printf("Function stack:\n");

    function_stack_node* current_node = stack;
    while (current_node != NULL) {
        printf("%s\n", current_node->function_name);
        current_node = current_node->next;
    }
}


void push(function_stack_node** stack, char* function_name)
{
    function_stack_node* new_node = malloc(sizeof(function_stack_node));
    new_node->function_name = strdup(function_name);
    new_node->next = *stack;
    *stack = new_node;
}

char* pop(function_stack_node** stack)
{
    if (*stack == NULL) {
        return NULL;
    }

    function_stack_node* top_node = *stack;
    char* function_name = top_node->function_name;
    *stack = top_node->next;
    free(top_node);

    return function_name;
}

char* peek(function_stack_node* stack)
{
    if (stack == NULL) {
        return NULL;
    }

    return stack->function_name;
}

unsigned long hash(char* str)
{
    unsigned long hash = 5381;  // magic number for better distribution
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; // hash * 33 + c
    return hash % HASH_TABLE_SIZE;
}

void addCalledFunction(char* functionName) {
    int index = hash(functionName);
    calledFunctions[index] = true;
}

bool isFunctionCalled(char* functionName) {
    int index = hash(functionName);
    return calledFunctions[index];
}

void gatherCalledFunctions(node* root) {
    if(root == NULL) {
        return;
    }
    if (strcmp(root->token, "call") == 0) {
        addCalledFunction(root->left->token);
    }

    gatherCalledFunctions(root->left);
    gatherCalledFunctions(root->right);
}

void addDeclaredVariable(char* varName) {
    int index = hash(varName);
    declaredVariables[index] = true;
    variableNames[index] = varName;
}

void useVariable(char* varName) {
    int index = hash(varName);
    usedVariables[index] = true;
}

void printHashTable() {
    for (int i = 0; i < HASH_TABLE_SIZE; i++) {
        if (calledFunctions[i])
            printf("Index %d: true\n", i);
    }
}

bool isDeadCode(symbol_table* table, function_stack_node* functionStack, node* root) {
    bool deadCodeFound = false;
    bool unusedVariableFound = false;
    function_stack_node* stackNode = functionStack;

    // Gather all called functions
    gatherCalledFunctions(root);

    // printHashTable();

    // Go through each function in the function stack
    while (stackNode != NULL) {
        // If a function in the stack is not found being called in the AST, it's dead code
        if (!isFunctionCalled(stackNode->function_name)) {
            printf("Unused Function found: Function '%s' is never called.\n", stackNode->function_name);
            deadCodeFound = true;
        }

        stackNode = stackNode->next;
    }

    // Go through the symbol table to find unused variables
    for (int i = 0; i < HASH_TABLE_SIZE; i++) {
        if (declaredVariables[i] && !usedVariables[i]) {
            printf("Unused variable found: Variable '%s' is declared but never used.\n", variableNames[i]);
            unusedVariableFound = true;
        }
    }

    return deadCodeFound || unusedVariableFound;
}

int isOperator2(char* token) {
    // add more operators based on language here
    if (strcmp(token, "+") == 0 || strcmp(token, "-") == 0 || strcmp(token, "*") == 0 || strcmp(token, "/") == 0) {
        return 1;
    }
    return 0;
}

unsigned int hashExpression(char* expr) {
    unsigned long hash = 5381;
    int c;

    while ((c = *expr++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash % EXPR_TABLE_SIZE;
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

    /* // If this is the "subroutines" node, don't print it and just continue with the children
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

    // If this is the "statements" node, don't print it and just continue with the children
    if (strcmp(tree->token, "statements") == 0) {
        printTree(tree->left);
        printTree(tree->right);
        return;
    } */


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

void printThreeAddressCode(node *tree, int indentLevel) {
    if (tree == NULL) return;

    if (strcmp(tree->token, "function") == 0) {

        if (!isFunctionCalled(tree->left->token)) {
            // If the function is not called, skip the generation of its code
            return;
        }

        // Print the function name
        indent(indentLevel);
        printf("\033[0;31m%s:\033[0m\n", tree->left->token);
        indent(indentLevel);
        printf("\tBeginFunc\n");

        // Fetch the function arguments node
        node *functionNode = tree->left;
        node *argsNode = NULL;

        if(functionNode != NULL){
            argsNode = functionNode->left; // Fetch the 'arguments' node
        }

        // Traverse to the body node
        node *bodyNode = tree->right;
        if (bodyNode != NULL) {
            // The child of body node is 'statements'
            node *statementsNode = bodyNode->left;
            if(statementsNode != NULL) {
                // Generate TAC for all statements in the body
                printThreeAddressCode(statementsNode, indentLevel + 2);
            }
        }
        printf("\tEndFunc\n");
    }
    else if (strcmp(tree->token, "return") == 0) {
        printThreeAddressCode(tree->left, indentLevel);
        indent(indentLevel - 1);
        printf("\tReturn %s\n", tree->left->tac);
    }
    else if (strcmp(tree->token, "procedure") == 0) {
        indent(indentLevel);
        printf("\033[0;31m%s:\033[0m\n", tree->left->token);
        indent(indentLevel);
        printf("\tBeginProc\n");
        printThreeAddressCode(tree->right, indentLevel + 2);
        indent(indentLevel + 1);
        printf("EndProc\n");
    }
    else if (strcmp(tree->token, "call") == 0) {
        // The functionNode's left is the function name and right is the 'arguments' node
        node *functionNode = tree->left;
        node *argsNode = NULL;

        if(functionNode != NULL){
            argsNode = functionNode->left; // Fetch the 'arguments' node
        }

        int argsCount = 0;  // Count number of arguments for later use

        // TAC generation for each argument and push operation
        if(argsNode != NULL && strcmp(argsNode->token, "arguments") == 0) {
            node *argumentNode = argsNode->left;
            while(argumentNode != NULL) {
                if (strcmp(argumentNode->token, "argument") == 0) {
                    printThreeAddressCode(argumentNode->left, indentLevel);
                    indent(indentLevel);
                    printf("PushParam %s\n", argumentNode->left->tac);
                    argsCount++;
                }
                argumentNode = argumentNode->right;
            }
        }

        // Create a new temporary variable for the return value of the function
        char *tempVar = (char*)malloc(10*sizeof(char));
        sprintf(tempVar, "t%d", tempVarCounter++);
        tree->tac = tempVar;

        // Print the function call
        indent(indentLevel);
        printf("%s = call %s\n", tempVar, functionNode->token);

        // Print pop operations after function call
        for (int i = 0; i < argsCount; i++) {
            indent(indentLevel);
            printf("PopParam\n");
        }
    }
    else if (strcmp(tree->token, "while") == 0) {
        // Three address code for 'while' condition
        printThreeAddressCode(tree->left, indentLevel);

        char *tempVar = (char*)malloc(10*sizeof(char));
        sprintf(tempVar, "t%d", tempVarCounter++);
        tree->left->tac = tempVar;
        int beginLabel = labelCounter++; // label before condition check
        int endLabel = labelCounter++; // label after the loop body

        // print the beginning label
        indent(indentLevel-1);
        printf("L%d: \n", beginLabel);

        // print the condition
        indent(indentLevel);
        printf("%s = %s %s %s\n", tree->left->tac, tree->left->left->tac, tree->left->token, tree->left->right->tac);

        indent(indentLevel);
        printf("if %s goto L%d\n", tree->left->tac, beginLabel + 1); // Jump to 'while' body if condition is true
        indent(indentLevel);
        printf("goto L%d\n", endLabel + 1); // Jump to end of while loop if condition is false

        // Three address code for 'while' body
        indent(indentLevel - 1);
        printf("L%d: \n", beginLabel + 1);
        printThreeAddressCode(tree->right, indentLevel);
        indent(indentLevel);
        printf("goto L%d\n", beginLabel); // jump back to condition check after executing loop body

        // print end label
        indent(indentLevel - 1);
        printf("L%d:\n", endLabel + 1);
    }
    else if (strcmp(tree->token, "do_while") == 0) {
        int startLabel = labelCounter++;
        int endLabel = labelCounter++;

        // Print the start label
        indent(indentLevel - 1);
        printf("L%d:\n", startLabel);

        // Generate TAC for the loop body
        printThreeAddressCode(tree->left, indentLevel);

        // Generate TAC for the condition
        printThreeAddressCode(tree->right, indentLevel);

        char *tempVar = (char*)malloc(10*sizeof(char));
        sprintf(tempVar, "t%d", tempVarCounter++);
        tree->right->tac = tempVar;

        // Condition test and jump
        indent(indentLevel);
        printf("%s = %s %s %s\n", tempVar, tree->right->left->tac, tree->right->token, tree->right->right->tac);
        indent(indentLevel);
        printf("if %s goto L%d\n", tempVar, startLabel);

        // Print the end label
        indent(indentLevel - 1);
        printf("L%d:\n", endLabel);
    }
    else if (strcmp(tree->token, "for_statement") == 0) {
        int startLabel = labelCounter++;
        int conditionLabel = labelCounter++;
        int bodyLabel = labelCounter++;
        int endLabel = labelCounter++;

        // Generate TAC for initialization
        indent(indentLevel - 2);
        printThreeAddressCode(tree->left->left->left, indentLevel);

        // Jump to condition check
        indent(indentLevel);
        printf("goto L%d\n", conditionLabel);

        // Print the body label
        indent(indentLevel - 1);
        printf("L%d: \n", bodyLabel);

        // Generate TAC for loop body
        printThreeAddressCode(tree->right, indentLevel);

        // After executing loop body, jump back to condition check
        indent(indentLevel);
        printf("goto L%d\n", conditionLabel);

        // Print the condition label
        indent(indentLevel - 1);
        printf("L%d: \n", conditionLabel);

        // Generate TAC for condition
        printThreeAddressCode(tree->left->right->left->left, indentLevel);

        char *tempVar = (char*)malloc(10*sizeof(char));
        sprintf(tempVar, "t%d", tempVarCounter++);
        tree->left->right->left->left->tac = tempVar;

        // Condition test and jump
        indent(indentLevel);
        printf("%s = %s %s %s\n", tempVar, tree->left->right->left->left->left->tac, tree->left->right->left->left->token, tree->left->right->left->left->right->tac);

        // If condition is true, execute loop body
        indent(indentLevel);
        printf("if %s goto L%d\n", tempVar, bodyLabel);

        // If condition is false, jump to end of loop
        indent(indentLevel);
        printf("goto L%d\n", endLabel);

        // Print the end label
        indent(indentLevel - 1);
        printf("L%d: \n", endLabel);
    }
    else if (strcmp(tree->token, "if") == 0) {
        int shortCircuitLabel, trueLabel, falseLabel;
        trueLabel = labelCounter++;

        if (strcmp(tree->left->token, "&&") == 0 || strcmp(tree->left->token, "||") == 0) {
            printThreeAddressCode(tree->left->left, indentLevel);

            char *tempVar1 = (char*)malloc(10*sizeof(char));
            sprintf(tempVar1, "t%d", tempVarCounter++);
            tree->left->left->tac = tempVar1;

            indent(indentLevel);
            printf("%s = %s %s %s\n", tempVar1, tree->left->left->left->tac, tree->left->left->token, tree->left->left->right->tac);

            // Left condition test and jump
            indent(indentLevel);
            printf("if %s goto L%d\n", tempVar1, trueLabel);

            // If the first condition is false, we go to the second condition
            shortCircuitLabel = labelCounter++;

            indent(indentLevel);
            printf("goto L%d\n", shortCircuitLabel);

            // Print label for the second condition
            indent(indentLevel - 1);
            printf("L%d:\n", shortCircuitLabel);

            // Second condition generation
            printThreeAddressCode(tree->left->right, indentLevel);

            char *tempVar2 = (char*)malloc(10*sizeof(char));
            sprintf(tempVar2, "t%d", tempVarCounter++);
            tree->left->right->tac = tempVar2;

            indent(indentLevel);
            printf("%s = %s %s %s\n", tempVar2, tree->left->right->left->tac, tree->left->right->token, tree->left->right->right->tac);

            // Right condition test and jump
            indent(indentLevel);
            printf("if %s goto L%d\n", tempVar2, trueLabel);

            // Label to skip the if body if both conditions are false
            falseLabel = labelCounter++;
            indent(indentLevel);
            printf("goto L%d\n", falseLabel);
        } else {
            // Three address code for 'if' condition for non logical operations
            printThreeAddressCode(tree->left, indentLevel);

            char *tempVar = (char*)malloc(10*sizeof(char));
            sprintf(tempVar, "t%d", tempVarCounter++);
            tree->left->tac = tempVar;

            indent(indentLevel);
            printf("%s = %s %s %s\n", tempVar, tree->left->left->tac, tree->left->token, tree->left->right->tac);

            // If condition test and jump
            indent(indentLevel);
            printf("if %s goto L%d\n", tempVar, trueLabel);

            // Label to skip the if body if condition is false
            falseLabel = labelCounter++;
            indent(indentLevel);
            printf("goto L%d\n", falseLabel);
        }

        // Three address code for 'if' body
        indent(indentLevel - 1);
        printf("L%d:\n", trueLabel);
        printThreeAddressCode(tree->right, indentLevel);

        // Label for the rest of the code after the 'if' body
        indent(indentLevel - 1);
        printf("L%d:\n", falseLabel);
    }
    else if (strcmp(tree->token, "if_else") == 0) {
        int shortCircuitLabel, trueLabel, falseLabel, endLabel;
        trueLabel = labelCounter++;

        if (strcmp(tree->left->token, "&&") == 0 || strcmp(tree->left->token, "||") == 0) {
            printThreeAddressCode(tree->left->left, indentLevel);

            char *tempVar1 = (char*)malloc(10*sizeof(char));
            sprintf(tempVar1, "t%d", tempVarCounter++);
            tree->left->left->tac = tempVar1;

            indent(indentLevel);
            printf("%s = %s %s %s\n", tempVar1, tree->left->left->left->tac, tree->left->left->token, tree->left->left->right->tac);

            // Left condition test and jump
            indent(indentLevel);
            printf("if %s goto L%d\n", tempVar1, trueLabel);

            // If the first condition is false, we go to the second condition
            shortCircuitLabel = labelCounter++;

            indent(indentLevel);
            printf("goto L%d\n", shortCircuitLabel);

            // Print label for the second condition
            indent(indentLevel - 1);
            printf("L%d:\n", shortCircuitLabel);

            // Second condition generation
            printThreeAddressCode(tree->left->right, indentLevel);

            char *tempVar2 = (char*)malloc(10*sizeof(char));
            sprintf(tempVar2, "t%d", tempVarCounter++);
            tree->left->right->tac = tempVar2;

            indent(indentLevel);
            printf("%s = %s %s %s\n", tempVar2, tree->left->right->left->tac, tree->left->right->token, tree->left->right->right->tac);

            // Right condition test and jump
            indent(indentLevel);
            printf("if %s goto L%d\n", tempVar2, trueLabel);

            // Label to skip the if body if both conditions are false
            falseLabel = labelCounter++;
            indent(indentLevel);
            printf("goto L%d\n", falseLabel);
        } else {
            // Three address code for 'if' condition for non logical operations
            printThreeAddressCode(tree->left, indentLevel);

            char *tempVar = (char*)malloc(10*sizeof(char));
            sprintf(tempVar, "t%d", tempVarCounter++);
            tree->left->tac = tempVar;

            indent(indentLevel);
            printf("%s = %s %s %s\n", tempVar, tree->left->left->tac, tree->left->token, tree->left->right->tac);

            // If condition test and jump
            indent(indentLevel);
            printf("if %s goto L%d\n", tempVar, trueLabel);

            // Label to skip the if body if condition is false
            falseLabel = labelCounter++;
            indent(indentLevel);
            printf("goto L%d\n", falseLabel);
        }

        // Three address code for 'if' body
        indent(indentLevel - 1);
        printf("L%d:\n", trueLabel);
        printThreeAddressCode(tree->right->left, indentLevel);

        // Label for the end of the 'if' block
        endLabel = labelCounter++;
        indent(indentLevel);
        printf("goto L%d\n", endLabel);

        // Label and three address code for 'else' body
        indent(indentLevel - 1);
        printf("L%d:\n", falseLabel);
        printThreeAddressCode(tree->right->right, indentLevel);

        // Print end label
        indent(indentLevel - 1);
        printf("L%d:\n", endLabel); // print label for end of 'if_else' structure
    }
    else {
            // Generate Three Address Code recursively in post-order
            printThreeAddressCode(tree->left, indentLevel);
            printThreeAddressCode(tree->right, indentLevel);

            if (strcmp(tree->token, "=") == 0) {
                indent(indentLevel);
                printf("%s = %s\n", tree->left->token, tree->right->tac ? tree->right->tac : tree->right->token);
            }
            else if (isOperator2(tree->token)) {
                if (tree->left != NULL && tree->right != NULL) {
                    char *tempVar;
                    // Allocate memory for the temporary variable
                    tempVar = (char*)malloc(10*sizeof(char));
                    // Generate the temporary variable name
                    sprintf(tempVar, "t%d", tempVarCounter++);

                    tree->tac = tempVar;

                    // Print the TAC
                    indent(indentLevel);
                    printf("%s = %s %s %s\n", tree->tac,
                                            tree->left->tac ? tree->left->tac : tree->left->token,
                                            tree->token,
                                            tree->right->tac ? tree->right->tac : tree->right->token);
                }
            }
            else {
                // Assign token itself as TAC for leaf nodes
                tree->tac = tree->token;
            }
        }
}

void freeNode(node* n) {
    if(n != NULL) {
        free(n->token);
        freeNode(n->left);
        freeNode(n->right);
        free(n);
    }
}

double performOperation(char* op, double leftVal, double rightVal) {
    if(strcmp(op, "+") == 0)
        return leftVal + rightVal;
    else if(strcmp(op, "-") == 0)
        return leftVal - rightVal;
    else if(strcmp(op, "*") == 0)
        return leftVal * rightVal;
    else if(strcmp(op, "/") == 0) {
        if (rightVal != 0.0)  // prevent division by zero
            return leftVal / rightVal;
        else {
            fprintf(stderr, "Error: Division by zero.\n");
            exit(EXIT_FAILURE);
        }
    }
    else if(strcmp(op, "==") == 0)
        return leftVal == rightVal;
    else if(strcmp(op, "!=") == 0)
        return leftVal != rightVal;
    else if(strcmp(op, "<") == 0)
        return leftVal < rightVal;
    else if(strcmp(op, ">") == 0)
        return leftVal > rightVal;
    else if(strcmp(op, "<=") == 0)
        return leftVal <= rightVal;
    else if(strcmp(op, ">=") == 0)
        return leftVal >= rightVal;
    else if(strcmp(op, "&&") == 0)
        return leftVal && rightVal;
    else if(strcmp(op, "||") == 0)
        return leftVal || rightVal;
    else {
        fprintf(stderr, "Error: Unknown operator.\n");
        exit(EXIT_FAILURE);
    }
}

char* convertResultToString(double result) {
    char* resultStr = (char*)malloc(20 * sizeof(char));
    if (resultStr == NULL) {
        fprintf(stderr, "Error: Memory allocation failed.\n");
        exit(EXIT_FAILURE);
    }
    // Check if result is boolean
    if(result == 0.0 || result == 1.0) {
        sprintf(resultStr, "%s", result == 1.0 ? "true" : "false");
    } else {
        sprintf(resultStr, "%.2f", result);
    }
    return resultStr;
}

void updateRootNode(node* root, char* resultStr) {
    free(root->token);
    root->token = resultStr;
    freeNode(root->left);
    freeNode(root->right);
    root->left = NULL;
    root->right = NULL;
}

node* constantFoldingOptimization(node* root) {
    if(root == NULL) {
        return NULL;
    }

    // Apply the optimization recursively first
    root->left = constantFoldingOptimization(root->left);
    root->right = constantFoldingOptimization(root->right);

    // Optimization for different node types
    if(isOperator2(root->token)) {
        double leftVal, rightVal, result;
        char* resultStr;

        // If the node's both left and right children are not NULL and both are numeric or real literals
        if(root->left != NULL && root->right != NULL &&
           (isNumericLiteral(root->left->token) || isRealLiteral(root->left->token)) &&
           (isNumericLiteral(root->right->token) || isRealLiteral(root->right->token))) {
            leftVal = atof(root->left->token);
            rightVal = atof(root->right->token);
            result = performOperation(root->token, leftVal, rightVal);
            resultStr = convertResultToString(result);
            updateRootNode(root, resultStr);
        }

        // Optimization for the case where the left child is an operator and right child is numeric or real literal
        if(root->left != NULL && root->right != NULL &&
           isOperator2(root->left->token) &&
           (isNumericLiteral(root->right->token) || isRealLiteral(root->right->token))) {
            root->left = constantFoldingOptimization(root->left);
        }

        // Optimization for the case where the right child is an operator and left child is numeric or real literal
        if(root->left != NULL && root->right != NULL &&
           (isNumericLiteral(root->left->token) || isRealLiteral(root->left->token)) &&
           isOperator2(root->right->token)) {
            root->right = constantFoldingOptimization(root->right);
        }
    }
    return root;
}

int yyerror(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "Error at line %d: ", yylineno);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");

    va_end(args);
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

    functionsStack = malloc(sizeof(function_stack_node *));
    *functionsStack = NULL;

    temporaryStack = malloc(sizeof(function_stack_node *));
    *temporaryStack = NULL;

    yyin = inputFile;
    if (yyparse() != 0) {
        fprintf(stderr, "Error: Parsing failed\n");
        fclose(inputFile);
        return -1;
    }

    // Constant Folding Optimization.
    root = constantFoldingOptimization(root);

    // Check for dead code after parsing and before generating 3AC
    if (!isDeadCode(current_table, *temporaryStack, root)) {
        printf("There is no dead code in the program.\n");
    }

    // 3AC Code Generation.
    printThreeAddressCode(root, 0);

    fclose(inputFile);

    return 0;
}