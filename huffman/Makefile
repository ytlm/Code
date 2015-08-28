FLAGS = -Wall -g

all:main

main:main.o huffman.o
	gcc $(FLAGS) -o $@ $^

main.o:main.c
	gcc $(FLAGS) -o $@ -c $^

huffman.o:huffman.c
	gcc $(FLAGS) -o $@ -c $^

clean:
	rm *.o main
