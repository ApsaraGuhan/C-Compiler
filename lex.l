%{
    #include<stdlib.h>
    #include<stdio.h>
    #include "y.tab.h"
%}

letter     [a-zA-A]
digit      [0-9]
hexadecimal [0-9a-eA-F]
exp         ((e|E)("+"|"-")?({digit}+))
identifier  ({letter}|"_")({letter}|{digit}|"_")*
whitespace   [ \t\r\f\v]+


%%
"#include<".*">" {return HEADER;}
"printf(".*")" {return PRINT;}
"scanf(".*")"  {return SCAN;}
"int"        {return INT;}
"float"        {return FLOAT;}
"char"        {return CHAR;}
"long"        {return LONG;}
"short"        {return SHORT;}
"void"        {return VOID;}
"signed"      {return SIGNED;}
"unsigned"    {return UNSIGNED;}
"return"        {return RETURN;}
"continue"        {return CONTINUE;}
"if"        {return IF;}
"else"        {return ELSE;}
"for"        {return FOR;}
"while"      {return WHILE;}
"break"        {return BREAK;}
"static"        {return STATIC;}
"struct"        {return STRUCT;}
"do"        {return DO;}

{identifier} {
  if(strlen(yytext) <= 32)
        return IDENTIFIER;
  else
        printf("Error %d: Identifier too long,must be between 1 to 32 characters\n", yylineno);

}
[0][x|X]{hexadecimal}+                      {yylval.tbEntry = InsertNew(ConstantTable,yylineno, yytext , (int)strtol(yytext, NULL, 16),"HEX");return HEXCONST;}
("+"|"-")?({digit}+)                     {yylval.tbEntry = InsertNew(ConstantTable,yylineno, yytext ,atoi(yytext),"INT");return INTCONST;}
({digit}*)["."]({digit}+)     {yylval.tbEntry = InsertNew(ConstantTable,yylineno, yytext , atof(yytext),"FLOAT");return DECCONST;}
({digit}+)["."]({digit}*)     {yylval.tbEntry = InsertNew(ConstantTable,yylineno,yytext , atof(yytext),"FLOAT");return DECCONST;}
{whitespace} ;
">"         {return GT;}
"<"         {return LT;}
">="         {return GET;}
"<="         {return LET;}
"=="         {return EQ;}
"+"         {return ADD;}
"-"         {return SUB;}
"*"         {return MUL;}
"/"         {return DIV;}
"%"         {return MOD;}
"="         {return ASSIGN;}
"--"         {return DECR;}
"++"         {return INCR;}
"+="         {return PLUSEQ;}
"-="         {return MINUSEQ;}
"*="         {return MULEQ;}
"/="         {return DIVEQ;}
"!="         {return NEQ;}
"||"         {return OR;}
"&&"         {return AND;}
"!"          {return NOT;}
"\n"         {yylineno++;}
.            {return yytext[0];}

%%
