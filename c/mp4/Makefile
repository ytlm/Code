CC = gcc
CFLAG = -Wall -g -O0

# multiple target
all: main

target = main

objects = mp4.o

$(target): $(objects)
	$(CC) $(CFLAG) $(objects) -o $@

.c.o:
	$(CC) $(CFLAG) -c $< -o $@

.PHONY:clean

clean:
	rm -rf $(objects) $(target) *.i *.s core.*


