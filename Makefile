
CFLAGS=-Wall -Wpossible-overlap -Wimplicit-define -Wcolumn-overflow -Wpossible-truncate -Wunreachable -fdump=ALL -ftraceall

./%: ./%.cbl
	echo `date` $< >>build.log
	cobc $(CFLAGS) -t $@.lst -x -o $@ $<

all: cs01a cs01b cs02a cs02b cs03a cs03b

.PHONY: all test01a run01a test01b run01b test02a run02a test02b run02b test03a run03a test03b run03b

test01a:
	./cs01a < ./data/01a-test.data

run01a:
	./cs01a < ./data/01a.data

test01b:
	./cs01b test < ./data/01a-test.data

run01b:
	./cs01b < ./data/01a.data

test02a:
	./cs02a test < ./data/02a-test.data

run02a:
	./cs02a < ./data/02a.data

test02b:
	./cs02b test < ./data/02a-test.data

run02b:
	./cs02b < ./data/02a.data

test03a:
	./cs03a test < ./data/03a-test.data

run03a:
	./cs03a < ./data/03a.data

test03b:
	./cs03b test < ./data/03a-test.data

run03b:
	./cs03b < ./data/03a.data


