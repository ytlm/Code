CFLAGS = -Wall -g
LIBS   = -lcrypto

all:test

test:test.o
	gcc $(CFLAGS) -o $@ $^ $(LIBS)

test.o:test.c
	gcc $(CFLAGS) -o $@ -c $^ $(LIBS)

clean:
	rm *.o test
