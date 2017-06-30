CFLAGS = -Wall -g
LN = -lapue

all:main

main:main.o
	gcc $(CFLAGS) -o $@ $^ $(LN)

main.o:main.c
	gcc $(CFLAGS) -o $@ -c $^ $(LN)

clean:
	rm main *.o
