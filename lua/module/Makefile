CC = gcc
CFLAG = -Wall -g -O0

target= mt.so main

# multiple target
all: $(target)

mt.o: mt.c
	$(CC) $(CFLAG) -c $< -o $@ -fPIC

mt.so: mt.o
	$(CC) $(CFLAG) -shared -o $@ $<

main.o: main.c
	$(CC) $(CFLAG) -o $@ -c $<

main: main.o
	$(CC) $(CFLAG) -o $@ $< -llua -lm

.PHONY:clean

clean:
	rm -rf $(target) *.o


#
# lua main.lua " lua call C function "
#
# ./main " C function call lua code "
#
