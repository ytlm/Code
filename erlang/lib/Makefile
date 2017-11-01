.SUFFIXES: .erl .beam

ERLC = erlc +debuginfo

.erl.beam:
	${ERLC} -W $<

.yrl.erl:
	${ERLC} -W $<

MODS = lib_files_find

all:compile

compile: ${MODS:%=%.beam} subdirs

subdirs:
	cd lib_chan; make
	cd lib_misc; make

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump
	cd lib_chan; make clean
	cd lib_misc; make clean
