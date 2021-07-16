#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int yyerror(char *msg);
extern int yylineno;

#define MAXSCOPE 20
#define SIZE 100

int  symlistIndex=0;
int currentscope=0;

struct table_entry
{
        int lineNo;
        char *idName;
        double value;
        int functionSet,noOfParameters;
        char* parameterlist[15];
        int nestLevel;
        int arraySet,arrayDim;
        struct table_entry *next;
        char* datatype;
};
typedef struct table_entry entry;                                                                                                                                                                                                                                                                                                                                                                                                     struct  tablefornesting                                                                                                                                                                                            {
        entry** symtable;
        int previous;
};
typedef struct tablefornesting mainsymtable;

extern  mainsymtable symbolTableList[MAXSCOPE];

int hashfun(char *idName)
{
        int sum=0;
        int hashvalue=0;
        for(int i=0; i < strlen(idName);i++)
        {
                sum=sum+idName[i];
        }
        hashvalue=sum%100;
        return hashvalue;
}



entry** createTable()
{
        entry **newtable = NULL;
        newtable = malloc(sizeof(entry*)*100);
        for(int i=0;i<SIZE;i++)
        {
                newtable[i] = NULL;
        }
        return newtable;
}

int createscope()
{
        symlistIndex++;
        symbolTableList[symlistIndex].symtable=createTable();
        symbolTableList[symlistIndex].previous=currentscope;
        return symlistIndex;
}                                                                                                                                                                                                                  
int exitthescope()
{
        int exitscope=symbolTableList[currentscope].previous;                                                                                                                                                              return exitscope;                                                                                                                                                                                          }                                                                                                                                                                                                                  
entry* search(entry** hashTable, char* idName)
{
        int hashvalue = hashfun(idName);
        entry* head=hashTable[hashvalue];
        while( head != NULL && strcmp(idName, head->idName ) != 0 )
                head= head->next;
        if(head == NULL)
                return NULL;
        return head;

}
entry* searchRecursive(char* idName)
{
        int scope = currentscope;
        entry* head = NULL;
        while(scope!=-1)                                                                                                                                                                                                   {
                entry** temp=symbolTableList[scope].symtable;
                head = search(symbolTableList[scope].symtable,idName);
                if(head != NULL)
                        return head;
                 scope = symbolTableList[scope].previous;                                                                                                                                                                  }                                                                                                                                                                                                                  return head;
} 
entry* searchForFunction(entry** TablePointer,char *lexeme)
{                                                                                                                                                                                                                          int hashvalue = hashfun(lexeme);
        entry *head = NULL;                                                                                                                                                                                                head = TablePointer[hashvalue];
        while(head != NULL)
        {
                if(strcmp(head->idName,lexeme) == 0 && head->functionSet == 1)
                {
                        return head;
                }
                else                                                                                                                                                                                                                       head = head->next;
        }
        if(head == NULL)
                return NULL;
        return head;
}

void checkAndSetFunction(entry** TablePointer, char *idName)
{
        entry* temp=searchForFunction(TablePointer,idName);
        if(temp==NULL)
        {
                entry* Entry = search(TablePointer,idName);
                if(Entry!=NULL)
                        Entry->functionSet = 1;
        }
        else
        {
                yyerror("Duplicate functions not allowed");
        }
}

void setArrayDim(entry** TablePointer,int arrayDimension,char *idName)
{
        entry* Entry = search(TablePointer,idName);
        if(Entry!=NULL)
          Entry->arrayDim = arrayDimension;
}
entry* InsertNew(entry** entryptr,int lineNo, char *idName,double value,char* dataType)
{
        int flag=0,hashvalue;
        entry* Entry=search(entryptr,idName );
        if(Entry!= NULL)
        {
          if(searchRecursive(idName)==NULL)
                flag=0;
          else
          {
                flag=1;
                yyerror("Redeclaration of variable");                                                                                                                                                                              return NULL;
          }
        }
        if(flag==0)
        {
                hashvalue=hashfun(idName);
                entry *head = NULL;
                head = entryptr[hashvalue];
                entry *temp = NULL;                                                                                                                                                                                                temp = malloc(sizeof(entry));
                temp->idName = strdup(idName);
                temp->value = value;
                temp->datatype = strdup(dataType);
                temp->lineNo = lineNo;
                temp->next = NULL;
                if (head== NULL)
                {
                        entryptr[hashvalue] = temp;
                }
                else
                {
                        temp->next = entryptr[hashvalue];
                        entryptr[hashvalue] = temp;
                }
        }
        return entryptr[hashvalue];

}

int DataTypeCheck(char *dataType1,char *dataType2)
{
        if(strcmp(dataType1,dataType2)!=0){
              yyerror("Type Mismatch");
              exit(0);
        }
        return 1;
}
int FunctionCheck(entry** SymbolTable,char* idName)
{
        entry *res = searchForFunction(SymbolTable,idName);
        if(res != NULL){
                res = search(symbolTableList[currentscope].symtable,idName);
                if(res != NULL){
                        yyerror("Defined as variable in this scope, calling not allowed");
                        return 0;
                  }
                  else
                    return 1;
         }
        else
        {
                yyerror("No such declaration");
                return 0;
        } 
}
void fillParameterList(entry* tableEntry, char **list, int n)
{
        tableEntry->noOfParameters = n;
        printf("\nFunction Name : %s \nNo of parametes: %d\n",tableEntry->idName,tableEntry->noOfParameters);
        if(n>0)
                printf("Data type of Parameters:");
        for(int i=0;i<n;i++)
        {
                tableEntry->parameterlist[i] = (char *)malloc(sizeof(char)); 
                if(strcmp(list[i],"VOID") == 0)
                {
                yyerror("Parameters of type void not allowed\n");
                return;
                }
                strcpy(tableEntry->parameterlist[i],list[i]);
                printf("%s\t",tableEntry->parameterlist[i]);
        }
        printf("\n");
}
void printDashes(int n)
{
        printf("\n");
        int i;
        for(i=0; i< n; i++) 
          printf("=");
         printf("\n");
}
int checkParameterList(entry* tableEntry, char** parlist, int n)
{
        char** list=tableEntry->parameterlist;
        if(n != tableEntry->noOfParameters)
        {
                yyerror("Number of parameters and arguments do not match"); 
                exit(0);
        }
        int i;
        for(i=0; i<n; i++)
        {
                if( strcmp(parlist[i],list[i]) !=0 )
                {
                        yyerror("Parameter and argument types do not match");
                        exit(0);
                }
        }
        return 1;
}
void Display(entry** TablePointer,int flag)
{
        int i;
        entry *temp;
        if(flag==1)
        {
                printDashes(100);
                printf("Token Name\t|Datatype\t|Line Number\t|isArray\t|ArrayDimensions\t|isFunction\n");
                printDashes(100);
                for( i=0; i < SIZE; i++)
                {
                        temp= TablePointer[i];
                        while( temp != NULL)
                        {
                                printf("%6s\t\t%s\t\t%d\t\t%d\t\t%d\t\t\t%d   \n",temp->idName,temp->datatype,temp->lineNo,temp->arraySet,temp->arrayDim,temp->functionSet);
                                temp = temp->next;
                        }
                }
                printDashes(100);
        }
        else
                printDashes(50);
                printf("Constant\t|Datatype\t|Line Number\n");
                printDashes(50);
                for( i=0; i < SIZE; i++)
                {
                        temp= TablePointer[i];
                        while( temp != NULL)
                        {
                                printf("%6s\t\t%s\t\t%d\n",temp->idName,temp->datatype,temp->lineNo);
                                temp = temp->next;
                        }
                 }
                printDashes(50);
                }
}
