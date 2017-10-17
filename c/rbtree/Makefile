CC = gcc
CFLAG = -Wall -g -O0

# multiple target
all: main

target = main

objects = main.o rbtree.o

$(target): $(objects)
	$(CC) $(CFLAG) $(objects) -o $@

.c.o:
	$(CC) $(CFLAG) -c $< -o $@

.PHONY:clean

clean:
	rm -rf $(objects) $(target) *.i *.s core.*


