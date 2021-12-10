
CFLAGS=-Wall -Wpossible-overlap -Wimplicit-define -Wcolumn-overflow -Wpossible-truncate -Wunreachable -fdump=ALL -ftraceall

./%: ./%.cbl
	echo `date` $< >>build.log
	cobc $(CFLAGS) -t $@.lst -x -o $@ $<

all: cs01a cs01b cs02a cs02b cs03a cs03b cs04a cs04b cs05a cs05b cs06a cs06b cs07a cs07b cs08a cs08b cs09a cs09b cs10a

.PHONY: all test01a run01a test01b run01b test02a run02a test02b run02b test03a run03a test03b run03b test04a run04a test04b run04b test05a run05a test05b run05b test06a18 test06a80 run06a test06b18 test06b80 test06b256 run06b test07a run07a test07b run07b test08a run08a test08b run08b test09a run09a test09b run09b test10a run10a

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

test04a:
	./cs04a test < ./data/04a-test.data

run04a:
	./cs04a < ./data/04a.data

test04b:
	./cs04b test < ./data/04a-test.data

run04b:
	./cs04b < ./data/04a.data

test05a:
	./cs05a test < ./data/05a-test.data

run05a:
	./cs05a < ./data/05a.data

test05b:
	./cs05b test < ./data/05a-test.data

run05b:
	./cs05b < ./data/05a.data

test06a18:
	./cs06a test 18 < ./data/06a-test.data

test06a80:
	./cs06a NOPE 80 < ./data/06a-test.data

run06a:
	./cs06a NOPE 80 < ./data/06a.data

test06b18:
	./cs06b test 18 < ./data/06a-test.data

test06b80:
	./cs06b NOPE 80 < ./data/06a-test.data

test06b256:
	./cs06b NOPE 256 < ./data/06a-test.data

run06b:
	./cs06b NOPE 256 < ./data/06a.data

test07a:
	./cs07a test < ./data/07a-test.data

run07a:
	./cs07a < ./data/07a.data

test07b:
	./cs07b test < ./data/07a-test.data

run07b:
	./cs07b < ./data/07a.data

test08a:
	./cs08a nope < ./data/08a-test.data

run08a:
	./cs08a < ./data/08a.data

test08b:
	./cs08b nope < ./data/08a-test.data

test08b1:
	./cs08b nope < ./data/08a-test1.data

run08b:
	./cs08b < ./data/08a.data

test09a:
	./cs09a test < ./data/09a-test.data

run09a:
	./cs09a < ./data/09a.data

test09b:
	./cs09b test < ./data/09a-test.data

run09b:
	./cs09b < ./data/09a.data

test10a:
	./cs10a test < ./data/10a-test.data

run10a:
	./cs10a < ./data/10a.data


