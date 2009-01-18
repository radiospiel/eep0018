
BEAM = \
	compare.beam \
	mochijson2.beam \
	mochinum.beam \
	rabbitmq.beam \
	runner.beam \
	testcase.beam

.SUFFIXES: .erl .beam

.erl.beam:
	erlc $^

all: $(BEAM)

clean: 
	rm -f *.so *.beam

copy:
	cp ../src/*.so ./
	cp ../src/*.beam ./

check: copy $(BEAM)
	erl -noshell -s runner main

