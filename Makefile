
CFLAGS=-Wall -Wpossible-overlap -Wimplicit-define -Wcolumn-overflow -Wpossible-truncate -Wunreachable -fdump=ALL -ftraceall

./%: ./%.cbl
	echo `date` $< >>build.log
	cobc $(CFLAGS) -t $@.lst -x -o $@ $<

all: cs01a cs01b

.PHONY: all

test01a:
	./cs01a < ./data/01a-test.data

run01a:
	./cs01a < ./data/01a.data

test01b:
	./cs01b test < ./data/01a-test.data

run01b:
	./cs01b < ./data/01a.data

