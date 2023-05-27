# Compiler

This project is an advanced compiler built with Lex and Yacc. The compiler parses a programming language that resembles a similar syntax to that of the C programming language, integrating the implementation of symbol tables, function stack tables, and different scopes to better manage and handle variables and functions.

## Structure
The project is divided into two core parts:

1. **Lexical Analyzer (scanner.l)**: This component tokenizes the input, breaking it down into recognizable lexemes which are passed on to the Syntax Analyzer.
2. **Syntax Analyzer (parser.y)**: This component uses the tokens from the Lexical Analyzer to parse the input according to our defined grammar rules.

The parser uses a binary tree to store and process the parsed syntax. The nodes of this tree are structured to hold a token and two pointers that point to the left and right child nodes respectively. 
```
typedef struct node
{
    char *token;
    struct node *left;
    struct node *right;
} node;
```

## Symbol Tables, Function Stack Tables, and Scopes
The compiler implements symbol tables to keep track of identifiers and their associated information, such as type and scope, which are critical for semantic analysis and error checking.
It also utilizes function stack tables that manage function calls and returns, ensuring the correct execution flow of the program.
Different scopes (global, local) are handled efficiently through the use of scoped symbol tables, thereby preventing conflicts among variables and allowing the reuse of identifiers in different scopes.

## Compilation and Execution
To compile the Lexer and Parser, use the following commands:
```
flex lexer.l
yacc -d parser.y
cc -o compiler y.tab.c -lfl -Ly
```
To run the compiler with an input file, use the following command:
``` ./compiler <inputfile> ```

## Features

The compiler includes an extensive set of features to handle various programming constructs efficiently:
* **Function and Procedure Definitions**: It supports the declaration of functions and procedures, effectively managing local scopes to prevent naming conflicts and allow variable reuse.
* **Main Function Definition**: The main entry point of the program is well handled, paving the way for the sequential execution of the code.
* **Data Types**: It supports a variety of data types including `int`, `char`, `bool`, `real`, `string`, and `void`.
* **Declaration and Initialization of Variables**: Variables can be declared and initialized within global and local scopes. The compiler uses symbol tables to track these variables and their attributes effectively.
* **Control Structures**: It implements control structures such as `if`, `while`, `do-while`, and `for` statements, enabling more complex and conditional program flows.
* **Function Calls**: It efficiently manages function calls and returns via function stack tables, ensuring accurate program execution flow.
* **Arrays and Pointers**: It supports arrays and pointers. This includes handling array indexing and pointer dereferencing.
* **Expression Evaluation**: It can evaluate expressions that include basic arithmetic and boolean operations.





