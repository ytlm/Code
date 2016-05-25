CC = gcc
CFLAGS = -Wall -g
LIB = -lm

OBJECT = main.o cjson.o readFile.o myerror.o

all:main

main:$(OBJECT)
	$(CC) $(CFLAGS) -o $@ $^ $(LIB)

.c.o:
	$(CC) $(CFLAGS) -o $@ -c $^ $(LIB)

clean:
	rm -rf main $(OBJECT)
