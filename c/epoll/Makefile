CC = gcc
CFLAGS = -Wall -g

all:main

main:main.o
	$(CC) $(CFLAGS) -o $@ $^ $(LN)

main.o:main.c
	$(CC) $(CFLAGS) -o $@ -c $^ $(LN)

.PHONY:
clean:
	rm main *.o
