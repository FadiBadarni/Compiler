%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "lex.yy.c"
    #include <stdbool.h>
    #include <stdarg.h>
    #include <ctype.h>

    typedef struct node
    {
        char *token;
        struct node *left;
        struct node *right;
        char *tac;
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
        addArgumentToFunction(currentFunction, idList->token, argumentType);
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

    int isNumericLiteral(const char* str) {
        // iterate over the string
        for(int i = 0; str[i] != '\0'; i++) {
            // if a character is not a digit, return false
            if (!isdigit(str[i])) {
                return 0;
            }
        }

        // if all characters were digits, return true
        return 1;
    }

    int isCharLiteral(const char* str) {
        // a char literal is usually enclosed in single quotes (like 'a') so we expect the string to be 3 characters long
        // str[0] should be a single quote, str[1] any character and str[2] a single quote again
        if (strlen(str) == 3 && str[0] == '\'' && str[2] == '\'') {
            return 1;
        }

        // if the string does not match this pattern, it is not a char literal
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

    int isUnaryOperator(const char* token) {
        // Currently only support address-of operator
        return strcmp(token, "&") == 0 || strcmp(token, "*") == 0 || strcmp(token, "ARRAY_ELEMENT") == 0;
    }

    char* getNodeType(node *n);

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
                yyerror("Error: Invalid types for operation. Operands must be int or real");
                return NULL;
            }
        }

        // Handle logical operations
        if (strcmp(operation, "&&") == 0 || strcmp(operation, "||") == 0) {
            if (strcmp(leftType, "bool") == 0 && strcmp(rightType, "bool") == 0) {
                return "bool";
            } else {
                yyerror("Error: Invalid types for operation. Operands must be of type bool %s %s", leftType, rightType);
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
                yyerror("Error: Invalid types for operation. Operands must be of type int or real");
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

                yyerror("Error: Invalid types for operation. Operands must be of the same type or compatible pointer types");
                return NULL;
            }
        }

        // Handle array indexing
        if (strcmp(operation, "array_index") == 0) {
            if (strcmp(leftType, "string") == 0 && strcmp(rightType, "int") == 0) {
                return "char";
            } else {
                yyerror("Error: Invalid types for array indexing. Left operand must be of type string and right operand must be of type int");
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
            yyerror("Error: Undefined variable\n");
            return NULL;
        }
        if (strcmp(operation, "!") == 0 && strcmp(entry->type, "bool") != 0) {
            yyerror("Error: Invalid type for operation '!'. Operand must be of type bool\n");
            return NULL;
        }
        if (strcmp(operation, "abs") == 0 && !(strcmp(entry->type, "int") == 0 || strcmp(entry->type, "real") == 0)) {
            yyerror("Error: Invalid type for operation 'abs'. Operand must be of type int or real\n");
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
                yyerror("Error: Invalid type for dereference operation. Operand must be of pointer type\n");
                return NULL;
            }

            // remove the last character (*) to get the dereferenced type
            char *dereferenced_type = strdup(entry->type);
            dereferenced_type[strlen(dereferenced_type) - 1] = '\0';
            return dereferenced_type;
        }
        return entry->type;
    }

    /* A helper function to check whether a token is an operator */
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
            yyerror("Null node in getNodeType");
            return NULL;
        }
        if (n->left == NULL && n->right == NULL) { // Identifier or literal
            if (isOperator(n->token)) {
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
            symbol_table_entry* entry = lookupSymbolTable(n->token);
            if (entry == NULL) {
                yyerror("Undeclared identifier: %s", n->token);
                return NULL;
            }
            return entry->type;
        }
        else if (n->right == NULL) { // Unary operation
            if (!isUnaryOperator(n->token)) {
                yyerror("Unexpected operator in unary operation");
                return NULL;
            }
            return checkUnaryOperationType(n->left, n->token);
        }
        else { // Binary operation
            if (n->left == NULL || n->right == NULL) {
                yyerror("Missing operand in binary operation");
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
                    strcmp(expr->token, "==") == 0 || strcmp(expr->token, "!=") == 0) {
                    return "bool";
                }

                return leftType; // for arithmetic operations, the type of the operation is the same as the operands
            }
        }
        /* If it's a unary operation or a simple identifier, just look up its type in the symbol table */
        else {
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
%type <node> program return_type
%type <node> code_block if_statement while_statement do_while_statement for_statement factor term unary atom

%left OR
%left AND
%left NEQ EQUALS
%left LT GT LTE GTE
%left PLUS MINUS
%left MULTI DIVISION

%nonassoc NOT

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
    FUNCTION IDENTIFIER LPAREN RPAREN COLON return_type
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
            /* Clear currentFunction since we're done parsing the function */
            free(currentFunction);
            currentFunction = NULL;
            $$ = createNode("procedure", createNode($2, NULL, NULL), createNode("body", $9, NULL));
        }
    | FUNCTION IDENTIFIER LPAREN
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
    arguments RPAREN COLON return_type
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
            processIdentifiers($3, argumentType->token);

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
     VAR identifiers_list COLON POINTER_TYPE SEMICOLON
        {
            $$ = createNode("declare_pointer", $2, createNode($4, NULL, NULL));

            node* id_node = $2;
            while(id_node != NULL) {
                char *type = malloc(strlen($4) + 2);
                strcat(type, $4);
                if (addSymbolTableEntry(id_node->token, type) == -1) {
                    yyerror("Variable redeclaration or memory allocation error");
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
                    yyerror("Variable redeclaration or memory allocation error");
                    YYABORT;
                }
                id_node = id_node->right;
            }
        }
    | IDENTIFIER ASSIGNMENT function_call SEMICOLON
        {
            // Check if variable has been declared
            symbol_table_entry* entry = lookupSymbolTable($1);
            if (entry == NULL) {
                yyerror("Variable not defined");
                YYABORT;
            }

            // /* Get the return type of the function call */
            // char* returnType = getReturnTypeOfFunctionCall($3);

            // /* Compare the expected return type with the actual return type */
            // if (strcmp(entry->type, returnType) != 0) {
            //     yyerror("Type mismatch in assignment. Expected: %s, Found: %s", entry->type, returnType);
            //     YYABORT;
            // }

            $$ = createNode("=", createNode($1, NULL, NULL), $3);
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
            char* expression_type = getTypeOfExpression($3);
            if (expression_type == NULL) {
                // An error message would have been printed by getTypeOfExpression
                YYABORT;
            }
            if (strcmp(entry->type, expression_type) != 0) {
                yyerror("Type mismatch in assignment. Expected: %s, Found: %s", entry->type, expression_type);
                YYABORT;
            }
        }
    | MULTI IDENTIFIER ASSIGNMENT expression SEMICOLON
        {
            // Check if variable has been declared
            symbol_table_entry* entry = lookupSymbolTable($2);
            if (entry == NULL) {
                yyerror("Variable not defined");
                YYABORT;
            }
            if (entry->type[strlen(entry->type) - 1] != '*') {
                yyerror("Variable is not a pointer");
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
                yyerror("Type mismatch in assignment. Expected: %s, Found: %s", pointed_type, expression_type);
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

            callArgument = reverseTree(callArgument);

            while (argument != NULL && callArgument != NULL) {
                if (strcmp(argument->type, getTypeOfExpression(callArgument->left)) != 0) {
                    yyerror("Error: Argument type mismatch in function call");
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
    statements_list // Here's where your variable declarations would be
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
    /* Logical and relational operations */
    expression AND term {
        if(checkBinaryOperationType($1, $3, "&&") == NULL)
            YYABORT;
        $$ = createNode("&&", $1, $3);
    }
    | expression OR term {
        if(checkBinaryOperationType($1, $3, "||") == NULL)
            YYABORT;
        $$ = createNode("||", $1, $3);
    }
    | expression EQUALS term {
        if(checkBinaryOperationType($1, $3, "==") == NULL)
            YYABORT;
        $$ = createNode("==", $1, $3);
    }
    | expression NEQ term {
        if(checkBinaryOperationType($1, $3, "!=") == NULL)
            YYABORT;
        $$ = createNode("!=", $1, $3);
    }
    | expression LT term {
        if(checkBinaryOperationType($1, $3, "<") == NULL)
            YYABORT;
        $$ = createNode("<", $1, $3);
    }
    | expression GT term {
        if(checkBinaryOperationType($1, $3, ">") == NULL)
            YYABORT;
        $$ = createNode(">", $1, $3);
    }
    | expression LTE term {
        if(checkBinaryOperationType($1, $3, "<=") == NULL)
            YYABORT;
        $$ = createNode("<=", $1, $3);
    }
    | expression GTE term {
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
            yyerror("Expression type error for 'abs' operation");
            YYABORT;
        }
        if(strcmp(exprType, "int") != 0 && strcmp(exprType, "real") != 0) {
            yyerror("Invalid type for 'abs' operation. Operand must be of type int or real");
            YYABORT;
        }
        $$ = createNode("abs", $2, NULL);
    }
    | MULTI IDENTIFIER {
        // Check if variable has been declared
        symbol_table_entry* entry = lookupSymbolTable($2);
        if (entry == NULL) {
            yyerror("Variable not defined");
            YYABORT;
        }
        if (entry->type[strlen(entry->type) - 1] != '*') {
            yyerror("Variable is not a pointer");
            YYABORT;
        }

        $$ = createNode("*", createNode($2, NULL, NULL), NULL);
    }
    | MULTI LPAREN expression RPAREN {
        // Check if the expression is of a pointer type
        char* exprType = getTypeOfExpression($3);
        if (exprType == NULL || exprType[strlen(exprType) - 1] != '*') {
            yyerror("Expression is not of pointer type for dereferencing operation");
            YYABORT;
        }
        $$ = createNode("*", $3, NULL);
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
            yyerror("Undeclared identifier: %s", $2);
            YYABORT;
        }
        if (strcmp(entry->type, "string") != 0) {
            yyerror("Variable is not a string array");
            YYABORT;
        }
        $$ = createNode("ARRAY_ELEMENT", createNode($2, NULL, NULL), $4);
    }
    | IDENTIFIER LBRACKET expression RBRACKET { $$ = createNode("array_index", createNode($1, NULL, NULL), $3); }
    /* Parenthesized Expression */
    | LPAREN expression RPAREN { $$ = $2; }
    ;

return_type:
    VOID { $$ = createNode("void", NULL, NULL); }
    | type
    ;

type:
    BOOL { $$ = createNode("bool", NULL, NULL); }
    | CHAR { $$ = createNode("char", NULL, NULL); }
    | INT { $$ = createNode("int", NULL, NULL); }
    | REAL { $$ = createNode("real", NULL, NULL); }
    | STRING { $$ = createNode("string", NULL, NULL); }
    ;




%%

int tempVarCount = 0;

int isOperatorz(char* token) {
    // You can add more operators based on your language here
    if (strcmp(token, "+") == 0 || strcmp(token, "-") == 0 || strcmp(token, "*") == 0 || strcmp(token, "/") == 0) {
        return 1;
    }
    return 0;
}

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

    if (isOperatorz(token)) {
        // Allocate memory for the temporary variable
        char *tempVar = (char*)malloc(10*sizeof(char));
        // Generate the temporary variable name
        sprintf(tempVar, "t%d", tempVarCount++);

        // Allocate memory for the TAC
        char *newTac = (char*)malloc(100*sizeof(char));

        // Create the TAC for the operation
        sprintf(newTac, "%s = %s %s %s", tempVar, left->tac, token, right->tac);

        newNode->tac = newTac;
    } else {
        // for leaf nodes, TAC code is the token itself
        newNode->tac = strdup(token);
    }

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

void printThreeAddressCode(node *tree) {
    if (tree == NULL) return;

    if (strcmp(tree->token, "procedure") == 0) {
        printf("%s:\n", tree->left->token); // function name
        printf("BeginFunc\n");
        printThreeAddressCode(tree->right); // function body
        printf("EndFunc\n");
    }

    // more else-if branches for other types of nodes...

    // Traverse the tree
    if (tree->left != NULL) {
        printThreeAddressCode(tree->left);
    }

    if (tree->right != NULL) {
        printThreeAddressCode(tree->right);
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

    printf("Three Address Code:\n");
    printThreeAddressCode(root);

    fclose(inputFile);

    return 0;
}