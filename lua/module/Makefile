CC = gcc
CFLAG = -Wall -g -O0

MT_SO= mt.so

# multiple target
all: $(MT_SO)

CORE_LIB= mt.o

.c.o:
	$(CC) $(CFLAG) -c $< -o $@ -fPIC

$(MT_SO): $(CORE_LIB)
	$(CC) $(CFLAG) -shared -o $@ $<

.PHONY:clean

clean:
	rm -rf $(CORE_LIB) $(MT_SO)


