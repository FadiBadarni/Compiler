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

	node* mknode(char* token, node *left, node *right);
	void Printtree(node *tree);
	void printTabs(int n);
	int yylex();
	int yyerror(const char *e);
	
	int printlevel=0;
%}

%union
{
    struct node *node;
    char *string;
}

%token <string> DIVISION PLUS MINUS MULTI IDENTIFIER

%type <node> expr

%left PLUS MINUS
%left MULTI DIVISION

%nonassoc IDENTIFIER 

%%

prog: expr { Printtree($1); };

expr:
      IDENTIFIER { $$ = mknode($1, NULL, NULL); }
    | expr PLUS expr     { $$ = mknode("+", $1, $3); }
    | expr MINUS expr    { $$ = mknode("-", $1, $3); }
    | expr MULTI expr    { $$ = mknode("*", $1, $3); }
    | expr DIVISION expr { $$ = mknode("/", $1, $3); }
    | '(' expr ')'       { $$ = $2; }
    ;

%%

node* mknode(char* token, node *left, node *right)
{
	node *newNode = (node*)malloc(sizeof(node));
	newNode->token = strdup(token);
	newNode->left = left;
	newNode->right = right;
	return newNode;
}

void printTabs(int n)
{
	for(int i=0; i<n; i++)
		printf("\t");
}

void Printtree(node *tree)
{
    if (tree == NULL) return;

    // Open a bracket before printing the operation
    if (tree->left != NULL && tree->right != NULL) {
        printTabs(printlevel);
        printf("(\n");
        printlevel++;
    }

    // Recurse for the left child
    if (tree->left != NULL) {
        Printtree(tree->left);
    }

    // Print the current token
    printTabs(printlevel - 1);
    printf("%s\n", tree->token);

    // Recurse for the right child
    if (tree->right != NULL) {
        Printtree(tree->right);
    }

    // Close a bracket after printing the operation
    if (tree->left != NULL && tree->right != NULL) {
        printlevel--;
        printTabs(printlevel);
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
