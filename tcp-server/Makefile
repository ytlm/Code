all:serverTcp

serverTcp:serverTcp.o conf.o ini.o map.o
	gcc -Wall -g -o $@ $^

serverTcp.o:serverTcp.c
	gcc -Wall -g -o $@ -c $^

conf.o:conf.c conf.h
	gcc -Wall -g -o $@ -c $<

ini.o:ini.c ini.h
	gcc -Wall -g -o $@ -c $<

clean:
	rm *.o serverTcp
