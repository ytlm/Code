FLAGS = -Wall -g

all:test

test:test.o huffman.o chain.o
	gcc $(FLAGS) -o $@ $^

test.o:test.c
	gcc $(FLAGS) -o $@ -c $^

huffman.o:huffman.c
	gcc $(FLAGS) -o $@ -c $^

chain.o:chain.c
	gcc $(FLAGS) -o $@ -c $^

clean:
	rm *.o test
