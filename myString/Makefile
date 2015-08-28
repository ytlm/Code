OBJECT = main.o mystring.o
FLAGS = -Wall -g


all:main

main:$(OBJECT)
	gcc $(FLAGS) -o $@ $^

main.o:main.c
	gcc $(FLAGS) -o $@ -c $^

mystring.o:mystring.c
	gcc $(FLAGS) -o $@ -c $^

clean:
	rm $(OBJECT) main
