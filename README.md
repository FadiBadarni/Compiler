# Compiler

This project is a compiler built with Lex and Yacc. It contains a parser written in C that can parse a programming language with a C-similar syntax.

## Structure
The project consists of two main parts:

* The Lexical Analyzer (scanner.l) which tokenizes the input.
* The Syntax Analyzer (parser.y) which parses the tokenized input according to our grammar rules.

The parser makes use of a binary tree to store and handle the parsed syntax. The tree's nodes are created in a specific structure which consists of a token and two pointers to the left and right child nodes respectively.
```
typedef struct node
{
    char *token;
    struct node *left;
    struct node *right;
} node;
```

## Compilation and Execution
First, you need to compile the Lexer and Parser:
```
flex lexer.l
yacc -d parser.y
cc -o compiler y.tab.c -lfl -Ly
```
To run the compiler with an input file:
``` ./compiler <inputfile> ```

## Features
The parser can handle the following constructs:

* Function and Procedure definitions.
* Main function definition.
* Data types: int, char, bool, real, string, void.
* Declaration and initialization of variables.
* If, While, Do-While and For statements.
* Function calls.
* Arrays and pointers.
* Expression evaluation including basic arithmetic and boolean operations.
