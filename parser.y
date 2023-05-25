%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "lex.yy.c"
    #include <stdbool.h>
    #include <stdarg.h>

    typedef struct node
    {
        char *token;
        struct node *left;
        struct node *right;
    } node;

    typedef struct argument_entry
    {
        char *name;
        char *type;
        struct argument_entry *next;
    } argument_entry;

    typedef struct symbol_table_entry
    {
        char *name;
        char *type;
        argument_entry *arguments;
        char *return_type;
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
    int yyerror(const char *fmt, ...);
    symbol_table *current_table = NULL; // this points to the top of the stack
    char* currentFunction = NULL;

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
        printf("Pushed symbol table. Current scope depth after push: %d\n", getSymbolTableDepth());
    }


    void popSymbolTable()
    {
        symbol_table *table = current_table;
        printf("Popping symbol table. Current scope depth before pop: %d\n", getSymbolTableDepth());
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

    int countArguments(symbol_table_entry *entry) {
        int count = 0;
        argument_entry *arg = entry->arguments;
        while(arg != NULL) {
            count++;
            arg = arg->next;
        }
        return count;
    }

    /*
    Tree structure needs fixing for generating arguments. this function works in the
    messed up structure, nesting is causing the problem.
    */
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

    char* checkBinaryOperationType(node *left, node *right, char *operation) {
        symbol_table_entry *leftEntry = lookupSymbolTable(left->token);
        symbol_table_entry *rightEntry = lookupSymbolTable(right->token);
        if (leftEntry == NULL || rightEntry == NULL) {
            yyerror("Error: Undefined variable\n");
            return NULL;
        }

        /* Handle arithmetic operations */
        if (strcmp(operation, "+") == 0 || strcmp(operation, "-") == 0 ||
            strcmp(operation, "*") == 0 || strcmp(operation, "/") == 0) {
            if((strcmp(leftEntry->type, "int") == 0 || strcmp(leftEntry->type, "real") == 0) &&
            (strcmp(rightEntry->type, "int") == 0 || strcmp(rightEntry->type, "real") == 0)) {
                if (strcmp(leftEntry->type, "int") == 0 && strcmp(rightEntry->type, "int") == 0) {
                    return "int";
                } else {
                    return "real";
                }
            } else {
                yyerror("Error: Invalid types for operation. Operands must be int or real");
                return NULL;
            }
        }

        /* Handle logical operations */
        if (strcmp(operation, "&&") == 0 || strcmp(operation, "||") == 0) {
            if (strcmp(leftEntry->type, "bool") == 0 && strcmp(rightEntry->type, "bool") == 0) {
                return "bool";
            } else {
                yyerror("Error: Invalid types for operation. Operands must be of type bool\n");
                return NULL;
            }
        }

        /* Handle comparison operations */
        if (strcmp(operation, ">") == 0 || strcmp(operation, "<") == 0 ||
            strcmp(operation, "<=") == 0 || strcmp(operation, ">=") == 0) {
            if ((strcmp(leftEntry->type, "int") == 0 || strcmp(leftEntry->type, "real") == 0) &&
                (strcmp(rightEntry->type, "int") == 0 || strcmp(rightEntry->type, "real") == 0)) {
                return "bool";
            } else {
                yyerror("Error: Invalid types for operation. Operands must be of type int or real\n");
                return NULL;
            }
        }
        /* For the operators (==,!=) the two operands can be two int, two bool, two real, two char, or two pointers to the same type. The result is bool. */
        if (strcmp(operation, "==") == 0 || strcmp(operation, "!=") == 0) {
            if (strcmp(leftEntry->type, rightEntry->type) == 0) {
                return "bool";
            } else {
                yyerror("Error: Invalid types for operation. Operands must be of the same type\n");
                return NULL;
            }
        }

        return NULL;
    }

    int checkUnaryOperationType(node *operand, char *operation) {
        symbol_table_entry *entry = lookupSymbolTable(operand->token);
        if (entry == NULL) {
            yyerror("Error: Undefined variable\n");
            return -1;
        }
        if (strcmp(operation, "!") == 0 && strcmp(entry->type, "bool") != 0) {
            yyerror("Error: Invalid type for operation '!'. Operand must be of type bool\n");
            return -1;
        }
        // Add more checks here to support more unary operations
        return 0;
    }

    char* checkExpressionType(node* expression) {
        if (strcmp(expression->token, "+") == 0 ||
            strcmp(expression->token, "-") == 0 ||
            strcmp(expression->token, "*") == 0 ||
            strcmp(expression->token, "/") == 0) {
            return checkBinaryOperationType(expression->left, expression->right, expression->token);
        }
        else {
            // Handle other cases (like identifiers, literals, etc.)
            // This is just a basic structure, will need to extend this to handle all cases in grammar
            symbol_table_entry* entry = lookupSymbolTable(expression->token);
            if (entry == NULL) {
                yyerror("Undeclared identifier: %s", expression->token);
                return NULL;
            }
            return entry->type;
        }
    }

    char* getTypeOfExpression(node* expr) {
        if (expr == NULL) {
            return NULL;
        }

        /* If it's a binary operation, check the types of both operands */
        if (expr->left && expr->right) {
            /* For binary operations, we should return the type of the result, not the type of the operands */
            char* operationType = checkBinaryOperationType(expr->left, expr->right, expr->token);
            if (operationType == NULL) {
                yyerror("Error: Invalid type for operation: %s\n", expr->token);
                return NULL;
            }
            return operationType;
        }
        /* If it's a unary operation or a simple identifier, just look up its type in the symbol table */
        else {
            symbol_table_entry* entry = lookupSymbolTable(expr->token);
            if (entry != NULL) {
                return entry->type;
            }
        }
        return NULL;
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
%token <string> BOOL CHAR INT REAL STRING VOID
%token <string> VAR ASSIGNMENT SEMICOLON COLON ARROW COMMA PIPE LBRACKET RBRACKET
%token <string> FUNCTION MAIN RETURN
%token <string> NULL_PTR POINTER_TYPE ADDRESS
%token <string> IF ELSE WHILE DO FOR
%token <string> GT GTE LT LTE NOT NEQ

%token LPAREN RPAREN LBRACE RBRACE

%type <node> statement statements_list expression function_call function_call_arguments
%type <node> subroutines subroutine main arguments arguments_list argument identifiers_list type
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
    FUNCTION IDENTIFIER LPAREN
        {
            /* Check if a function with the same name is already declared in the current scope */
            symbol_table_entry *existingEntry = lookupSymbolTableInCurrentScope($2);
            if (existingEntry != NULL) {
                yyerror("Error: Function with this name is already declared in the current scope");
                YYABORT;
            }

            /* Add the function name to the symbol table */
            addSymbolTableEntry($2, "function");

            /* Set currentFunction to the name of the function being parsed */
            currentFunction = strdup($2);

            /* When we start a new function, we enter a new scope.
            So we create a new symbol table and push it onto the stack. */
            symbol_table *newTable = createSymbolTable();
            if (newTable == NULL) {
                yyerror("Failed to create symbol table");
                YYABORT;
            }
            pushSymbolTable(newTable);

        }
    arguments RPAREN COLON type
        {
            /* Extract the return type */
            setFunctionReturnType(currentFunction, $8->token);
        }
    LBRACE statements_list RBRACE
        {
            /* When we're done with the function, we exit its scope,
            so we pop its symbol table off the stack. */
            popSymbolTable();

            /* Clear currentFunction since we're done parsing the function */
            free(currentFunction);
            currentFunction = NULL;

            $$ = createNode("function", createNode($2, createNode("arguments", $5, NULL), createNode("return_type", $8, NULL)), createNode("body", $11, NULL));
        }
    | FUNCTION IDENTIFIER LPAREN RPAREN COLON VOID
        {
            /* Check if a function with the same name is already declared in the current scope */
            symbol_table_entry *existingEntry = lookupSymbolTableInCurrentScope($2);
            if (existingEntry != NULL) {
                yyerror("Error: Function with this name is already declared in the current scope");
                YYABORT;
            }

            /* Add the function name to the symbol table */
            addSymbolTableEntry($2, "function");

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
        }
    LBRACE statements_list
        {
            /* When we're done with the function, we exit its scope,
            so we pop its symbol table off the stack. */
            popSymbolTable();
        }
    RBRACE
        {
            $$ = createNode("procedure", createNode($2, NULL, NULL), createNode("body", $9, NULL));
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
    arguments_list { $$ = $1; }  /* One or more arguments. */
    ;

/* Used to parse the list of arguments. */
arguments_list:
    argument { $$ = createNode("argument", $1, NULL); }  /* Single argument */
    | arguments_list SEMICOLON argument { $$ = createNode("argument", $1, $3); } /* Multiple arguments */
    ;


/* Used to parse individual arguments. */
argument:
    IDENTIFIER ARROW identifiers_list COLON type
        {
            /* Add argument to symbol table */
            char *argumentName = $1;
            struct node *argumentType = $5;

            // Process identifiers_list
            char* identifiers = strdup($3->token);
            char* identifier = strtok(identifiers, " ");

            while (identifier != NULL) {
                // check if identifier is already in the symbol table
                symbol_table_entry *existingEntry = lookupSymbolTableInCurrentScope(identifier);
                if (existingEntry != NULL) {
                    yyerror("Error: Identifier with this name is already declared in the current scope");
                    YYABORT;
                }

                // add identifier to the symbol table
                addSymbolTableEntry(identifier, argumentType->token);
                addArgumentToFunction(currentFunction, identifier, argumentType->token);
                identifier = strtok(NULL, " "); // get the next identifier
            }

            free(identifiers);

            $$ = createNode("argument", createNode(argumentType->token, NULL, NULL), $3);
        }
    ;

/* Used to parse lists of identifiers, separated by commas. */
identifiers_list:
    IDENTIFIER { $$ = createNode(strdup($1), NULL, NULL); }
    | IDENTIFIER COMMA identifiers_list { $$ = createNode(strdup($1), NULL, $3); }
;


/* Used to parse lists of statements. */
statements_list:
    statement  { $$ = createNode("statements", $1, NULL); }
    | statements_list statement  { $$ = createNode("statements", $1, $2); }
    ;


/* Used to parse individual statements, including variable declarations, variable assignments, return statements, and different types of control structures like if, while, do-while, and for loops. */
statement:
    // ! NOT WANTED
    VAR identifiers_list ASSIGNMENT expression COLON POINTER_TYPE SEMICOLON
    {
        $$ = createNode("declare_initialize_pointer", $2, createNode("declare_initialize_data", $4, createNode("pointer", NULL, NULL)));

        node* id_node = $2;
        while(id_node != NULL) {
            if (addSymbolTableEntry(id_node->token, "pointer") == -1) {
                yyerror("Variable redeclaration or memory allocation error");
                YYABORT;
            }
            id_node = id_node->left;
        }
    }
    // ! NOT WANTED
    | VAR identifiers_list ASSIGNMENT expression COLON type SEMICOLON
        {
            $$ = createNode("declare_initialize", $2, createNode("declare_initialize_data", $4, $6));

            node* id_node = $2;
            while(id_node != NULL) {
                if (addSymbolTableEntry(id_node->token, $6->token) == -1) {
                    yyerror("Variable redeclaration or memory allocation error");
                    YYABORT;
                }
                id_node = id_node->left;
            }
        }
    | VAR identifiers_list COLON POINTER_TYPE SEMICOLON
        {
            $$ = createNode("declare_pointer", $2, createNode("pointer", NULL, NULL));

            node* id_node = $2;
            while(id_node != NULL) {
                if (addSymbolTableEntry(id_node->token, "pointer") == -1) {
                    yyerror("Variable redeclaration or memory allocation error");
                    YYABORT;
                }
                id_node = id_node->left;
            }
        }
    | VAR identifiers_list COLON type SEMICOLON
        {
            $$ = createNode("declare", $2, $4);

            // Assuming $2 is a linked list of identifier names
            node* id_node = $2;
            while(id_node != NULL) {
                if (addSymbolTableEntry(id_node->token, $4->token) == -1) {
                    yyerror("Variable redeclaration or memory allocation error");
                    YYABORT;
                }
                id_node = id_node->right;
            }
        }
    | IDENTIFIER ASSIGNMENT expression SEMICOLON
        {
            $$ = createNode("=", createNode($1, NULL, NULL), $3);

            // Check if variable has been declared
            symbol_table_entry* entry = lookupSymbolTable($1);
            if (entry == NULL) {
                yyerror("Variable not defined");
                YYABORT;
            }

            // Check if the type of the expression matches the type of the variable
            char* expression_type = checkExpressionType($3);
            if (strcmp(entry->type, expression_type) != 0) {
                yyerror("Type mismatch in assignment");
                YYABORT;
            }
        }
    //TODO: MIGHT NEED CHANGING IN SYNTAX
    | type IDENTIFIER LBRACKET INT_LITERAL RBRACKET SEMICOLON
        {
            if (current_table == NULL) {
                yyerror("Error: Symbol table is not initialized\n");
                YYABORT;
            }
            if ($1 == NULL || $2 == NULL) {
                yyerror("Error: Null values provided\n");
                YYABORT;
            }

            symbol_table_entry* entry = lookupSymbolTableInCurrentScope($2);
            if (entry != NULL) {
                yyerror("Error: Variable %s already declared\n", $2);
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
            symbol_table_entry *functionEntry = lookupSymbolTable(currentFunction);
            if (functionEntry == NULL) {
                yyerror("Error: Function not found in the symbol table");
                YYABORT;
            }

            /* Get the expected return type of the function */
            char* expectedReturnType = functionEntry->return_type;

            /* Get the type of the actual return expression */
            char* actualReturnType = getTypeOfExpression($2);

            /* Compare the expected return type with the actual return type */
            if (strcmp(expectedReturnType, actualReturnType) != 0) {
                yyerror("Error: Type mismatch in return statement");
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
                yyerror("Error: Wrong number of arguments in function call");
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
                yyerror("Error: Wrong number of arguments in function call");
                YYABORT;
            }

            argument_entry *argument = existingEntry->arguments;
            node *callArgument = $3;

            //TODO: THE TYPES ARE BEING MATCHED ONLY IN REVERSE.
            while (argument != NULL && callArgument != NULL) {
                if (strcmp(argument->type, getTypeOfExpression(callArgument->left)) != 0) {
                    yyerror("Error: Argument type mismatch in function callz");
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
    LBRACE { pushSymbolTable(createSymbolTable()); } statements_list RBRACE
    {
        $$ = createNode("block", $3, NULL);
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
                yyerror("Error: Condition of an if statement must be of type bool");
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
            printf("\n REACHED %s \n", expressionType);
            if (strcmp(expressionType, "bool") != 0) {
                yyerror("Error: Condition of an if statement must be of type bool");
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
                yyerror("Error: Condition of a while statement must be of type bool");
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
                yyerror("Error: Condition of a do-while statement must be of type bool");
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
                yyerror("Error: Condition of a for statement must be of type bool");
                YYABORT;
            }

            /* Push a new symbol table for for loop scope */
            symbol_table *forTable = createSymbolTable();
            pushSymbolTable(forTable);

            node* initialization = $3;
            node* condition = $5;
            node* increment = $7;
            node* body = $10;

            $$ = createNode("for", initialization, createNode("for_body", condition, createNode("for_increment", increment, body)));

            popSymbolTable(); /* Pop the symbol table after exiting for loop scope */
        }
    ;

expression:
    /* Literals and Identifiers */
    IDENTIFIER
        {
            symbol_table_entry* entry = lookupSymbolTable($1);
            if (entry == NULL) {
                yyerror("Undeclared identifier: %s", $1);
                YYABORT;
            }
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
    | PIPE IDENTIFIER PIPE { $$ = createNode("length_of", createNode($2, NULL, NULL), NULL); }
    | IDENTIFIER LBRACKET expression RBRACKET { $$ = createNode("array_index", createNode($1, NULL, NULL), $3); }
    /* Binary Operations */
    | expression PLUS expression {
        if(checkBinaryOperationType($1, $3, "+") == NULL)
            YYABORT;
        $$ = createNode("+", $1, $3);
    }
    | expression MINUS expression {
        if(checkBinaryOperationType($1, $3, "-") == NULL)
            YYABORT;
        $$ = createNode("-", $1, $3);
    }
    | expression MULTI expression {
        if(checkBinaryOperationType($1, $3, "*") == NULL)
            YYABORT;
        $$ = createNode("*", $1, $3);
    }
    | expression DIVISION expression {
        if(checkBinaryOperationType($1, $3, "/") == NULL)
            YYABORT;
        $$ = createNode("/", $1, $3);
    }
    | expression AND expression {
        if(checkBinaryOperationType($1, $3, "&&") == NULL)
            YYABORT;
        $$ = createNode("&&", $1, $3);
    }
    | expression OR expression {
        if(checkBinaryOperationType($1, $3, "||") == NULL)
            YYABORT;
        $$ = createNode("||", $1, $3);
    }
    | expression EQUALS expression {
        if(checkBinaryOperationType($1, $3, "==") == NULL)
            YYABORT;
        $$ = createNode("==", $1, $3);
    }
    | expression NEQ expression {
        if(checkBinaryOperationType($1, $3, "!=") == NULL)
            YYABORT;
        $$ = createNode("!=", $1, $3);
    }
    | expression LT expression {
        if(checkBinaryOperationType($1, $3, "<") == NULL)
            YYABORT;
        $$ = createNode("<", $1, $3);
    }
    | expression GT expression {
        if(checkBinaryOperationType($1, $3, ">") == NULL)
            YYABORT;
        $$ = createNode(">", $1, $3);
    }
    | expression LTE expression {
        if(checkBinaryOperationType($1, $3, "<=") == NULL)
            YYABORT;
        $$ = createNode("<=", $1, $3);
    }
    | expression GTE expression {
        if(checkBinaryOperationType($1, $3, ">=") == NULL)
            YYABORT;
        $$ = createNode(">=", $1, $3);
    }
    /* Unary Operations */
    | NOT expression {
        if(checkUnaryOperationType($2, "!") == -1)
            YYABORT;
        $$ = createNode("!", $2, NULL);
    }
    /* Parenthesized Expression */
    | '(' expression ')' { $$ = $2; }
    /* Function Call */
    | function_call
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
    /* if (strcmp(tree->token, "subroutines") == 0) {
        printTree(tree->left);
        printTree(tree->right);
        return;
    } */

    // If this is the "arguments" node, don't print it and just continue with the children
    /* if (strcmp(tree->token, "arguments") == 0) {
        printTree(tree->left);
        printTree(tree->right);
        return;
    } */

    // If this is the "arguments_list" node, don't print it and just continue with the children
    /* if (strcmp(tree->token, "arguments_list") == 0) {
        printTree(tree->left);
        printTree(tree->right);
        return;
    } */

    // If this is the "if_else_wrapper" node, don't print it and just continue with the children
    /* if (strcmp(tree->token, "if_else_wrapper") == 0) {
        printTree(tree->left);
        printTree(tree->right);
        return;
    } */

    // If this is the "statements_list" node, don't print it and just continue with the children
    /* if (strcmp(tree->token, "statements_list") == 0) {
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


    yyin = inputFile;
    if (yyparse() != 0) {
        fprintf(stderr, "Error: Parsing failed\n");
        fclose(inputFile);
        return -1;
    }

    fclose(inputFile);

    return 0;
}