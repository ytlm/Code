.SUFFIXES: .erl .beam .yrl

all: compile

ERLC = erlc

ERL = erl -boot start_clean

MODS = area_server event_handler lib_lin lib_primes \
	   motor_controller my_alarm_handler prime_server \
	   sellaprime_app sellaprime_supervisor

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump


#
# erl -boot start_sasl -config elog4.config
# application:start(sellaprime).
# lib_primes:make_prime(10).
#
