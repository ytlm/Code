CFLAGS = -Wall -g
LN = -lreadline -ltermcap
all:test

test:test.o
	gcc $(CFLAGS) -o $@ $^ $(LN)

test.o:test.c
	gcc $(CFLAGS) -o $@ -c $^ $(LN)

clean:
	rm *.o test
