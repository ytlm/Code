CFLAGS = -Wall -g

OBJECT = main.o cjson.o readFile.o

all:main

main:$(OBJECT)
	gcc $(CFLAGS) -o $@ $^

main.o:main.c
	gcc $(CFLAGS) -o $@ -c $^

cjson.o:cjson.c
	gcc $(CFLAGS) -o $@ -c $^

readFile.o:readFile.c
	gcc $(CFLAGS) -o $@ -c $^

clean:
	rm main $(OBJECT)
