.SUFFIXES: .erl .beam

ERLC = erlc +debuginfo

.erl.beam:
	${ERLC} -W $<

.yrl.erl:
	${ERLC} -W $<

ERL = erl -boot start_clean

MODS = area clock color endemo exit listmax listreverse max \
	   messenger ping temps timer

all:compile

compile:${MODS:%=%.beam} subdirs

subdirs:
	cd app; make
	cd chat; make
	cd file; make
	cd lib_chan; make
	cd otp; make
	cd ports; make
	cd shout; make
	cd table; make
	cd tcp; make
	cd udp; make

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump
	cd app; make clean
	cd chat; make clean
	cd file; make clean
	cd lib_chan; make clean
	cd otp; make clean
	cd ports; make clean
	cd shout; make clean
	cd table; make clean
	cd tcp; make clean
	cd udp; make clean
