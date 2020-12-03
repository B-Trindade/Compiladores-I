all: compilador entrada.txt
	./mini_js< entrada.txt
	
interpretador: lex.yy.mdp.o var_object.cc mdp.h mdp.cc
	 g++-7 -Wall -std=c++17 lex.yy.mdp.o mdp.cc -ll -lfl -o interpretador 
		
lex.yy.mdp.o: lex.yy.mdp.c 
	g++-7 -Wall -std=c++14 -c lex.yy.mdp.c 
	
lex.yy.mdp.c: mdp.l 
	flex -o lex.yy.mdp.c mdp.l 
	
executar: mini_js entrada.txt interpretador
	./mini_js < entrada.txt | ./interpretador

compilador: lex.yy.c y.tab.c
	g++-7 y.tab.c -o mini_js -lfl	
	
lex.yy.c: mini_js.l
	flex mini_js.l
	
y.tab.c: mini_js.y
	bison -o"y.tab.c" mini_js.y

clean: 
	rm -f lex.yy.c y.tab.c mini_js
