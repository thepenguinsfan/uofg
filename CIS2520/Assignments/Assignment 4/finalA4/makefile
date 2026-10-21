a4: graph_functions.o main.o
	gcc graph_functions.o main.o -o a4

graph_functions.o: graph_functions.c graph.h
	gcc -Wall -std=c99 -pedantic -c graph_functions.c

main.o: main.c graph.h
	gcc -Wall -std=c99 -pedantic -c main.c

clean:
	rm -f *.o a4