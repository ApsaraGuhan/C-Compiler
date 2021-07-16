%{
        #include<stdio.h>
        #include<stdlib.h>
        #include "symbolTable.h"
        #include<limits.h>
        #include<ctype.h>
        #include<string.h>

        extern int yylineno;
        extern char* yytext;
        entry **SymbolTable = NULL;
        entry **ConstantTable=NULL;

        mainsymtable symbolTableList[MAXSCOPE];

        int yylex(void);
        int yyerror(char *msg);

        char* curDataType;
        char* functionDataType;
        int LOOP=0,DECLARATION=0,FUNCTION=0,CONSTANT=0,RHS=0,RETURNSET=0;
        char *parameterList[10];
        char *argumentList[10];
        int pListIndex=0,aListIndex=0;
        int functionScope=0;

%}

%union{
        entry *tbEntry;
        char* datatype;
}

%token ADD SUB MUL DIV ASSIGN PLUSEQ MINUSEQ MULEQ DIVEQ  MOD
%token GT LT LET GET NEQ EQ
%token VOID IF ELSE FOR DO WHILE BREAK CONTINUE RETURN STATIC STRUCT PRINT SCAN
%token INT SHORT LONG CHAR FLOAT SIGNED UNSIGNED
%token OR AND NOT
%token DECR INCR HEADER

%token <tbEntry> HEXCONST DECCONST INTCONST IDENTIFIER

%type <datatype> arrayAssignment
%type <datatype> expressionType
%type <datatype> assignmentExpression
%type <datatype> arithmeticExpression                                                                                                                                                                              %type <datatype> postPreExpression
%type <datatype> idOrArray
%type <datatype> functionCall

%type <tbEntry> arrayIndex
%type <tbEntry> constantType
%type <tbEntry> identifier

%%

program : HEADER start|start;

start :start declaration|declaration;


declaration : variable|function;
function : typeDeclaration identifier {functionDataType=curDataType; DECLARATION=1;currentscope=createscope(); } '('parameterList')' {DECLARATION = 0;
                                                                                                        fillParameterList($2,parameterList,pListIndex);
                                                                                                        pListIndex = 0;
                                                                                                        checkAndSetFunction(SymbolTable,$2->idName);
                                                                                                        FUNCTION=1;
                                                                                                        functionScope=1;} blockOfStmts{if(strcmp(functionDataType,"VOID")!=0)
                                                                                                                                {
                                                                                                                                        if(!RETURNSET)
                                                                                                                                yyerror("return statement not matched");
                                                                                                                                }FUNCTION=0;RETURNSET=0;};
typeDeclaration : typeSpecifier pointer{DECLARATION=1;}|typeSpecifier{DECLARATION=1;};

typeSpecifier : signSpecifier dataType|dataType;

signSpecifier : SIGNED|UNSIGNED|STATIC;

dataType  : INT {curDataType="INT";  DECLARATION=1;}|SHORT {curDataType="SHORT"; DECLARATION=1;}|LONG {curDataType="LONG";DECLARATION =1;}|
            VOID {curDataType="VOID";  DECLARATION=1; }|CHAR {curDataType = "CHAR";  DECLARATION = 1;}|FLOAT {curDataType = "FLOAT";  DECLARATION = 1;};

parameterList : parameters| ;

parameters : param|parameters ',' param;

param : typeSpecifier identifier{parameterList[pListIndex] = (char *)malloc(sizeof(curDataType));
                                              strcpy(parameterList[pListIndex++],$2->datatype);
                                              DECLARATION = 0;};
functionCall : identifier '(' args ')' {
                                      if(FunctionCheck(SymbolTable,$1->idName)==0){yyerror("Function error");};                                                                                                                                          $$ = $1->datatype;
                                      checkParameterList($1,argumentList,aListIndex);
                                      aListIndex = 0;};
args : argList | ;

argList : argList ',' arg| arg ;

arg : expressionType     {
                            argumentList[aListIndex] = (char *)malloc(sizeof($1));
                            strcpy(argumentList[aListIndex++],$1);
                        };

statementType : blockOfStmts|singleStmt;
blockOfStmts : '{'{if(!functionScope)
                        currentscope=createscope();
                   else
                        functionScope=0;
                  }
                statements
                '}'{currentscope=exitthescope();};

statements : statements statementType | ;


singleStmt : variable|whileLoop|doWhileLoop|ifStmt|forLoop|keywordStmts|PRINT';'|SCAN';';

variable : typeDeclaration varDeclList';'{DECLARATION = 0;}|varDeclList';'|postPreExpression';' ;

varDeclList : varDeclList ',' varDeclInitialize | varDeclInitialize;

varDeclInitialize : assignmentExpression|arrayAssignment|arithmeticExpression ;
keywordStmts :BREAK';'{if(!LOOP) yyerror("Break statement invalid");exit(0);}|CONTINUE';'{if(!LOOP) yyerror("Continue statement invalid");exit(0);}|
              RETURN';'{if(FUNCTION)
                                {if(functionDataType!="void")
                                        yyerror("return statement not matched");
                                }
                        }
              |RETURN expressionType';'{RETURNSET=1;if(FUNCTION)
                                                {if(strcmp(functionDataType,$2)!=0)
                                                        {yyerror("return statement not matched");}
                                        }
                                        else{yyerror("return statement invalid");}};

whileLoop : WHILE'('conditionExpression')'{LOOP=1;} statementType{LOOP=0;};                                                                                                                                        
doWhileLoop : DO whileLoop;

ifStmt : IF'('conditionExpression')'statementType|IF'('conditionExpression')'statementType ELSE statementType;

forLoop : FOR'('conditionExpression';'conditionExpression';'conditionExpression')'{LOOP=1;} statementType{LOOP=0;};


conditionExpression : conditionExpression','expressionType|expressionType|';';

expressionType: expressionType GT expressionType{$$=$1;DataTypeCheck($1,$3);}| expressionType LT expressionType{$$=$1;DataTypeCheck($1,$3);}| expressionType GET expressionType{$$=$1;DataTypeCheck($1,$3);}|
                expressionType LET expressionType{$$=$1;DataTypeCheck($1,$3);}|expressionType NEQ expressionType{$$=$1;DataTypeCheck($1,$3);}|expressionType EQ expressionType{$$=$1;DataTypeCheck($1,$3);}|
                expressionType AND expressionType{$$=$1;DataTypeCheck($1,$3);}|expressionType OR expressionType{$$=$1;DataTypeCheck($1,$3);}|arithmeticExpression{$$=$1;}|assignmentExpression{$$=$1;}|
                postPreExpression{$$=$1;};


assignmentExpression : idOrArray assignmentOperators arithmeticExpression{DataTypeCheck($1,$3);RHS=0;$$=$3;}|idOrArray assignmentOperators arrayAssignment{DataTypeCheck($1,$3);RHS=0;$$=$3;}|
                       idOrArray assignmentOperators functionCall{DataTypeCheck($1,$3);RHS=0;$$=$3;}|idOrArray assignmentOperators postPreExpression{DataTypeCheck($1,$3);RHS=0;$$=$3;};|
                       postPreExpression assignmentOperators postPreExpression{DataTypeCheck($1,$3);RHS=0;$$=$3;};
                       
idOrArray : identifier{$$=$1->datatype;}|arrayAssignment{$$=$1;};


arithmeticExpression : arithmeticExpression ADD arithmeticExpression{DataTypeCheck($1,$3);}|arithmeticExpression SUB arithmeticExpression{DataTypeCheck($1,$3);}|
                       arithmeticExpression MUL arithmeticExpression{DataTypeCheck($1,$3);}|arithmeticExpression DIV arithmeticExpression{DataTypeCheck($1,$3);}|
                       arithmeticExpression MOD arithmeticExpression|'('arithmeticExpression')'{$$=$2;}|
                       identifier{$$=$1->datatype;}|constantType{$$=$1->datatype;};

assignmentOperators : EQ{RHS=1;}|ASSIGN{RHS=1;}|PLUSEQ{RHS=1;}|MINUSEQ{RHS=1;}|MULEQ{RHS=1;}|DIVEQ{RHS=1;};

postPreExpression : DECR identifier{$$=$2->datatype;}|INCR identifier{$$=$2->datatype;}|identifier DECR{$$=$1->datatype;}|identifier INCR{$$=$1->datatype;};
arrayAssignment : identifier '['arrayIndex']'
                  {if(DECLARATION)
                   {
                        if($3->value < 1)
                                yyerror("Arrays can't have dimension lesser than 1");
                        else
                        {
                                if(CONSTANT)
                                {
                                        $1->arrayDim=$3->value;
                                        setArrayDim(SymbolTable,$1->arrayDim,$1->idName);
                                        CONSTANT=0;
                                }
                                if($3->value>$1->arrayDim)
                                        yyerror("Array index out of bound");                                                                                                                                      $                  }
                  else if(CONSTANT)
                  {
                        if($3->value>$1->arrayDim)
                                yyerror("Array index out of bound");
                        if($3->value<0)
                                yyerror("Array index cannot be negative");
                        CONSTANT=0;
                  }
                 };

arrayIndex : identifier{$$=$1;}|constantType{$$=$1;};

constantType :INTCONST{CONSTANT=1;}|DECCONST{CONSTANT=1;}|HEXCONST{CONSTANT=1;};

pointer : '*' pointer | '*';
identifier : IDENTIFIER {if(DECLARATION && !RHS){
                                $1 = InsertNew(SymbolTable,yylineno,yytext,INT_MAX,curDataType);
                                $1 = InsertNew(symbolTableList[currentscope].symtable,yylineno,yytext,INT_MAX,curDataType);
                                if($1==NULL)
                                        yyerror("Redeclaration of varriable");
                                }
                          else
                          {
                                $1 = searchRecursive(yytext);
                                if($1 == NULL)
                                        yyerror("Variable Not Declared");
                          }
                          $$=$1;};
%%
#include "lex.yy.c"
int main(int argc , char *argv[]){

        SymbolTable = createTable();
        ConstantTable=createTable();
        int i;
        for(i=0; i<MAXSCOPE;i++)
        {
                symbolTableList[i].symtable = NULL;
                symbolTableList[i].previous = -1;
        }
        symbolTableList[0].symtable = createTable();
        yyin = fopen(argv[1], "r");
         if(!yyparse())
        {
                printf("\nParsing complete.\n");                                                                                                                                                                                   printf("\n********************SYMBOL TABLE*******************\n");
                Display(SymbolTable,1);
                printf("\n********************CONSTANT TABLE*******************\n");
                Display(ConstantTable,2);
        }
        else
        {
                printf("\nParsing failed.\n");
        }
        fclose(yyin);
        return 0;
}

int yyerror(char *msg)
{
        printf("Line no: %d Error message: %s Token: %s\n", yylineno, msg, yytext);
        exit(0);
        return 0;
}
