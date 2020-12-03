ASM     "asm{".*"}"
DELIM   [\t\n ]
DIGITO  [0-9]
LETRA   [A-Za-z_]
ASPAS_INTERNAS ("\\\"")|(\"\")
STRING \"({ASPAS_INTERNAS}|[^\".*])*\"|('(''|\\'|[^'\n*])*')
DOUBLE  {DIGITO}+("."{DIGITO}+)?
ID      {LETRA}({LETRA}|{DIGITO})*
ARG     ({DELIM})*{ID}({DELIM})*
ARGS_ARROW {ARG}(","{ARG})*")"({DELIM})*"=>"

%%

"\t"       { coluna += 4; }
" "        { coluna++; }
"\n"       { linha++; coluna = 1; }
             
{DOUBLE}   { yylval.c = novo + yytext; return NUM; }

"let"	   { yylval.c = novo + "erro let"; return LET; }

"if"       { yylval.c = novo + "erro if"; return IF; }

"else"     { yylval.c = novo + "erro if"; return ELSE; }

"while"    { yylval.c = novo + "erro while"; return WHILE; }

"for"      { yylval.c = novo + "erro for"; return FOR; }

"=="       { yylval.c = novo + "erro eq"; return EQ; }

"!="       { yylval.c = novo + "erro dif"; return DIF; }

"<="       { yylval.c = novo + "erro meeq"; return MEEQ; }

">="       { yylval.c = novo + "erro maeq"; return MAEQ; }

"true"     { yylval.c = novo + yytext; return _TRUE; }

"false"    { yylval.c = novo + yytext; return _FALSE; }

"function" { yylval.c = novo + "erro function"; return FUNCTION; }

"return"   { yylval.c = novo + "erro return"; return RETURN; }

{ASM}      { string lexema = trim( yytext ); 
             yylval.c = split( lexema );
             coluna += strlen( yytext ); 
             return ASM; }

"=>"       { yylval.c = novo + "erro arrow"; return ARROW; }

"("/{ARGS_ARROW}    { yylval.c = novo + "erro look_ahead_parenteses"; return LA_PARENTESES; }

{STRING}    { yylval.c = novo + yytext; return STRING; }

{ID}       { yylval.c = novo + yytext; return ID; }

.          { yylval.c = novo + yytext; return yytext[0]; }

%%