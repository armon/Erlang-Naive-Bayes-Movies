all: compile
	
compile: clear
	mkdir ebin
	erlc -pa ebin/ -o ebin/ src/*.erl
	
compile_test: compile
	erlc -pa ebin/ -o ebin/ test/*.erl

train:
	erl -pa ebin/ -run train main priv/training -run init stop -noshell

test:
	erl -pa ebin/ -run test main priv/test -run init stop -noshell

clear:
	rm -f ebin/*.beam
	rm -f erl_crash.dump
