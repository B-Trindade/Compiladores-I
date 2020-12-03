%{
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <vector>
#include <map>
#include <regex>

using namespace std;

struct Atributos {
  vector <string> c;
  int size = 0;
};

vector<string> novo;
vector<string> vazio;
vector<string> funcoes;

map<vector<string>,int> declaracoes;
int linha = 1;
int coluna = 1;
int n_elementos;

string then = "";
string end_if = "";
string end_while = "";
string start_func = "";

#define YYSTYPE Atributos

extern "C" int yylex();
int yyparse();
void yyerror(const char *);

vector<string> concatena( vector<string> a, vector<string> b );
vector<string> operator + ( vector<string> a, string b );
vector<string> operator + ( vector<string> a, vector<string> b );

string gera_label( string prefixo );
vector<string> resolve_enderecos( vector<string> entrada );
string trim(string s);
vector<string> split(string str);
int retorna( int tk );

void print ( vector<string> codigo );

void declara_funcao( string label, vector<string> bloco );
void declara_lambda( string label, vector<string> bloco );
vector<string> trata_param_default( vector<string> id, vector<string> default_param );
vector<string> checa_declaracao( vector<string> var );
vector<string> checa_duplo_let( vector<string> var );


%}

%token NUM ID LET STRING IF ELSE EQ MAEQ MEEQ DIF WHILE FOR FUNCTION RETURN ASM _TRUE _FALSE ARROW LA_PARENTESES


%right '='
%nonassoc '<' '>' EQ MAEQ MEEQ DIF
%left ','
%left '+' '-'
%left '*' '/'
%left '.'
%nonassoc '%'

// Start indica o símbolo inicial da gramática
%start S

%%

S : CMDs { print( resolve_enderecos( $1.c + "." + funcoes ) ); }
  ;

CMDs : CMD CMDs   { $$.c = $1.c + $2.c; }
     | { $$.c = novo; }
     ;

CMD : ATR ';'               { $$.c = $1.c + "^"; }
    | PROP_ATR ';'          { $$.c = $1.c + "^"; }
    | LET DECLVARs ';'      { $$.c = $2.c; }
    | RET ';'
    | R ASM ';'             { $$.c = $1.c + $2.c + "^"; }
    | IFs
    | W
    | F
    | DECLFUNCs
    | CALL_FUNC
    ;





IFs :IF '(' R ')' STM ELSEs    
    {
      then = gera_label( "then" );
      end_if = gera_label( "end_if" );

      $$.c = $3.c + then + "?" + $6.c + end_if + "#" + (":" + then) + $5.c + (":" + end_if);
    }
  ;

ELSEs : ELSE STM            { $$.c = $2.c; }
      |                     { $$.c = novo; }
      ;





STM : CMD                   { $$.c = $1.c; }
    | BLOCO
    | BLOCO_VAZIO
    ;

BLOCO : '{' CMDs '}'        { $$.c = $2.c; }
      ;

BLOCO_VAZIO : '{' '}'       { $$.c = novo; }
            ;




W : WHILE '(' R ')' STM
    { 
        then = gera_label( "then" );
        end_while = gera_label( "end_while" );
        $$.c = vazio + ( ":" + then) + $3.c + "!" + end_while + "?" + $5.c + then + "#" + (":" + end_while); 
    }
  ;

F : FOR '(' EXPR_FOR ';' R ';' EXPR_FOR ')' STM
    {
      then = gera_label( "then" );
      end_while = gera_label( "end_while" );
      $$.c = $3.c + (":" + then) + $5.c + "!" + end_while + "?" + $9.c + $7.c + then + "#" + (":" + end_while);
    }
  ;

EXPR_FOR : ATR               { $$.c = $1.c + "^"; }
         | PROP_ATR          { $$.c = $1.c + "^"; }
         | LET DECLVARs      { $$.c = $2.c; }
         ;





DECLVAR : ID '=' R { $$.c = /*checa_duplo_let($1.c)*/ $1.c + "&" + $1.c + $3.c + "=" + "^"; declaracoes[$1.c] = linha; } 
        | ID       { $$.c = /*checa_duplo_let($1.c)*/ $1.c + "&"; declaracoes[$1.c] = linha; }
        ;

DECLVARs : DECLVAR ',' DECLVARs { $$.c = $1.c + $3.c; }
         | DECLVAR
         ;

ATR : ID '=' ATR  { $$.c = $1.c + $3.c + "="; }
    | PROP_ATR
    | R
    ;

PROP_ATR : ID PROP '=' PROP_ATR           { $$.c = $1.c + $2.c + $4.c + "[=]"; }
         | ID PROP PROP2 '=' PROP_ATR     { $$.c = $1.c + $2.c + $3.c + $5.c + "[=]"; }
         | R
         ; 

OBJ_LITERAL : '{' CAMPOs '}'    { $$.c = novo + "{}" + $2.c; }
            ;

CAMPOs : DECL_CAMPO ',' CAMPOs  { $$.c = $1.c + $3.c; }
       | DECL_CAMPO
       ;

DECL_CAMPO : ID ':' R           { $$.c = $1.c + $3.c + "[<=]"; }
           ;




PROP : '.' ID INDEX2                { $$.c = vazio + "@" + $2.c + $3.c; }
     | INDEX                        { $$.c = vazio + "@" + $1.c; }
     ; 

INDEX : '[' A ']' INDEX2      { $$.c = $2.c + $4.c; }   
      ;

INDEX2 : '[' A ']' INDEX2     { $$.c = vazio + "[@]" + $2.c + $4.c; }
       |                      { $$.c = novo; }
       ; 

PROP2 : '.' ID INDEX2                { $$.c = vazio + "[@]" + $2.c + $3.c; }
     | INDEX                        { $$.c = vazio + "[@]" + $1.c; }
     ; 





DECL_ANON_FUNC : FUNCTION '(' PARAMs ')' BLOCO
               {
                  start_func = gera_label( "start_func" );
                  $$.c = vazio + "{}" + "'&funcao'" + start_func + "[<=]";
                  declara_funcao( (":" + start_func), $3.c + $5.c);
               }
               ;

DECLFUNCs : FUNCTION ID '(' PARAMs ')' BLOCO  
          {
            start_func = gera_label( "start_func" );
            $$.c = $2.c + "&" + $2.c + "{}" + "=" + "'&funcao'" + start_func + "[=]" + "^";
            declara_funcao( (":" + start_func), $4.c + $6.c);
          }
          | FUNCTION ID '(' ')' BLOCO
          {
            start_func = gera_label( "start_func" );
            $$.c = $2.c + "&" + $2.c + "{}" + "=" + "'&funcao'" + start_func + "[=]" + "^";
            declara_funcao( (":" + start_func), $5.c);
          }
          ;

PARAMs : ID                     { $$.size++; $$.c = $1.c + "&" + $1.c + "arguments" + "@" + ":arguments:" + "[@]" + "=" + "^"; declaracoes[$1.c] = linha; }
       | ID ',' PARAMs          { $$.size++; $$.c = $1.c + "&" + $1.c + "arguments" + "@" + ":arguments:" + "[@]" + "=" + "^" + $3.c; declaracoes[$1.c] = linha; }
       | ID '=' R               { $$.c = trata_param_default( $1.c, $3.c ); }
       | ID '=' R ',' PARAMs    { $$.c = trata_param_default( $1.c, $3.c ) + $5.c; }
       ;

RET : RETURN A  { $$.c = $2.c + "'&retorno'" + "@" + "~"; }
    ;

CALL_FUNC : ID INVOKE                 { $$.c = $2.c + $1.c + "@" + "$"; }
          | ID PROP INVOKE            { $$.c = $3.c + $1.c + $2.c + "[@]" + "$"; }
          | ID PROP PROP2 INVOKE      { $$.c = $4.c + $1.c + $2.c + $3.c + "[@]" + "$"; }
          ;

INVOKE : '(' ')'                { $$.c = vazio + "0"; }
       | '(' ARGs ')'           { $$.c = $2.c + std::to_string( $2.size ); }

ARGs : A              { $$.size = $1.size + 1; $$.c = $1.c; }
     | A ',' ARGs     { $$.size = $3.size + 1; $$.c = $1.c + $3.c; }
     ;





LAMBDA : PARAMs_LAMBDA ARROW BLOCO_LAMBDA
       {
        start_func = gera_label("start_func");
        $$.c = vazio + "{}" + "'&funcao'" + start_func + "[<=]";
        declara_lambda( (":" + start_func), $1.c + $3.c);
       }
       ;

PARAMs_LAMBDA : LA_PARENTESES ARGs_LAMBDA ')' { $$.c = $2.c; }
              | '(' ')'                       { $$.c = novo; }
              | ID  { $$.c = $1.c + "&" + $1.c + "arguments" + "@" + ":arguments:" + "[@]" + "=" + "^"; }
              ;

ARGs_LAMBDA : ID ',' ARGs_LAMBDA  { $$.c = $1.c + "&" + $1.c + "arguments" + "@" + ":arguments:" + "[@]" + "=" + "^" + $3.c; }
            | ID                  { $$.c = $1.c + "&" + $1.c + "arguments" + "@" + ":arguments:" + "[@]" + "=" + "^"; }
            ;

BLOCO_LAMBDA : A
             | '(' CMDs ')'       { $2.c.pop_back(); $$.c = $2.c; }   
             ;





ARRAY :'[' ELEMENTOs ']' { n_elementos = 0; $$.c = vazio + "[]" + $2.c; }
      ;

ELEMENTOs : ELEMENTO ',' ELEMENTOs  { $$.c =  $1.c + $3.c; }
          | ELEMENTO
          ;

ELEMENTO : A        { $$.c = vazio + std::to_string(n_elementos++) + $1.c + "[<=]"; }
        ;






A : ATR
  | PROP_ATR
  | R
  ;

R : R '<' R     { $$.c = $1.c + $3.c + "<"; }
  | R '>' R     { $$.c = $1.c + $3.c + ">"; }
  | R EQ R      { $$.c = $1.c + $3.c + "=="; }
  | R MEEQ R    { $$.c = $1.c + $3.c + "<="; }
  | R MAEQ R    { $$.c = $1.c + $3.c + ">="; }
  | R DIF R     { $$.c = $1.c + $3.c + "!="; }
  | R '+' R     { $$.c = $1.c + $3.c + "+"; }
  | R '-' R     { $$.c = $1.c + $3.c + "-"; }
  | R '*' R     { $$.c = $1.c + $3.c + "*"; }
  | R '/' R     { $$.c = $1.c + $3.c + "/"; }
  | R '%' R     { $$.c = $1.c + $3.c + "%"; }
  | G
  ;

G : ID              { $$.c = $1.c + "@"; }
  | ID PROP         { $$.c = $1.c + $2.c + "[@]"; }
  | ID PROP PROP2   { $$.c = $1.c + $2.c + $3.c + "[@]"; }
  | NUM             
  | '-' NUM         { $$.c = vazio + "0" + $2.c + "-"; }
  | STRING          
  | _TRUE
  | _FALSE
  | '(' R ')'       { $$.c = $2.c; }
  | '{' '}'         { $$.c = novo + "{}"; }
  | '[' ']'         { $$.c = novo + "[]"; }
  | CALL_FUNC
  | DECL_ANON_FUNC
  | LAMBDA
  | OBJ_LITERAL 
  | ARRAY
  ;

%%

#include "lex.yy.c"

void yyerror( const char* st ) {
   puts( st ); 
   printf( "Proximo a: %s\n", yytext );
   exit( 1 );
}

string gera_label( string prefixo ) {
  static int n = 0;
  return (prefixo + "_" + to_string( ++n ) + ":");
}

vector<string> resolve_enderecos( vector<string> entrada ) {
  map<string,int> label;
  vector<string> saida;
  int indice_args = 0;

  for( int i = 0; i < entrada.size(); i++ ) 
    if( entrada[i][0] == ':' ) {
      if( entrada[i].substr(1,9) == "arguments" ) {
        if( entrada[i][10] == '_' ) 
          saida.push_back( std::to_string(indice_args-1) );
        else {
          saida.push_back( std::to_string(indice_args) );
          ++indice_args;
        }
      } else {
        label[entrada[i].substr(1)] = saida.size();
      }
    } else if (entrada[i] == "'&retorno'") {
      indice_args = 0;
      saida.push_back( entrada[i] );
    } else {
      saida.push_back( entrada[i] );
    }      
  
  for( int i = 0; i < saida.size(); i++ ) 
    if( label.count( saida[i] ) > 0 )
        saida[i] = to_string(label[saida[i]]);
    
  return saida;
}

void print ( vector<string> codigo ) {
    for( auto l : codigo )
        std::cout << l << std::endl;
    
    std::cout << "." << std::endl;
}

vector<string> concatena( vector<string> a, vector<string> b ) {
  a.insert( a.end(), b.begin(), b.end() );
  return a;
}

vector<string> operator+( vector<string> a, vector<string> b ) {
  return concatena( a, b );
}

vector<string> operator+( vector<string> a, string b ) {
  a.push_back( b );
  return a;
}

vector<string> operator+( string b, vector<string> a ) {
  a.push_back( b );
  return a;
}

//Busca por uma primeira declaracao
vector<string> checa_declaracao ( vector<string> var ) {
  auto busca = declaracoes.find(var);
  if( busca == declaracoes.end() ) {
    std::cout << "Erro: a variável '" << var[0] << "' não foi declarada." << std::endl;
    exit(1);
  } 
  return var;
}

//Busca por uma declaracao
vector<string> checa_duplo_let ( vector<string> var ) {
  auto busca = declaracoes.find(var);
  if( busca != declaracoes.end() ) {
    std::cout << "Erro: a variável '" << var[0] << "' já foi declarada na linha " << busca->second << "." << std::endl;
    exit(1);
  } 
  return var;
}

void declara_funcao( string label, vector<string> bloco ) {
  funcoes = funcoes + label + bloco + "undefined" + "@" + "'&retorno'" + "@" + "~";
}

void declara_lambda( string label, vector<string> bloco ) {
  funcoes = funcoes + label + bloco + "'&retorno'" + "@" + "~";
}

vector<string> trata_param_default( vector<string> id, vector<string> default_param ) {
  vector<string> ret{ "undefined" };
  then = gera_label( "then" );
  end_if = gera_label( "end_if" );

  //Caso id tenha sido passado como != de undefined, empilhamos o valor e fazemos set + pop
  //Caso contrario, id == undefined, portanto recebe o parametro default.
  ret = ret + "@" + "arguments" + "@" + ":arguments:" + "[@]" + "==" + then + "?" 
  + id + "&" + id + "arguments" + "@" + ":arguments_:" + "[@]" + "=" + "^" + end_if + "#"
  + (":" + then) + id + "&" + id + default_param + "=" + "^" + (":" + end_if); 

  return ret;
}

int retorna( int tk ) {  
  vector<string> v{yytext};
  yylval.c = v; 
  coluna += strlen( yytext ); 

  return tk;
}

std::string trim(string s) {
  const regex r("(asm\\{)|(\\})");
  
  std::stringstream res;
  std::regex_replace( std::ostream_iterator<char>(res), s.begin(), s.end(), r, "" );
  return res.str();
}

vector<string> split(string str) {
  vector<string> ret;
  size_t pos = 0;
  string token;
  string delimiter = " ";

  while ( (pos = str.find(delimiter)) != string::npos ) {
    token = str.substr(0, pos);
    ret.push_back(token);
    str.erase( 0, pos + delimiter.length() );
  }

  token = str.substr(0, pos);
  ret.push_back(token);
  return ret;
} 

int main( int argc, char* argv[] ) {
  yyparse();
  
  return 0;
}