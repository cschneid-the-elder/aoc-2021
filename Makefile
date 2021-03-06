
CFLAGS=-Wall -Wpossible-overlap -Wimplicit-define -Wcolumn-overflow -Wpossible-truncate -Wunreachable -fdump=ALL -ftraceall

./%: ./%.cbl
	echo `date` $< >>build.log
	cobc $(CFLAGS) -t $@.lst -x -o $@ $<

all: cs01a cs01b cs02a cs02b cs03a cs03b cs04a cs04b cs05a cs05b cs06a cs06b cs07a cs07b cs08a cs08b cs09a cs09b cs10a cs10b cs11a cs13a cs14a cs14b cs16a

.PHONY: all test01a run01a test01b run01b test02a run02a test02b run02b test03a run03a test03b run03b test04a run04a test04b run04b test05a run05a test05b run05b test06a18 test06a80 run06a test06b18 test06b80 test06b256 run06b test07a run07a test07b run07b test08a run08a test08b run08b test09a run09a test09b run09b test10a run10a test10b run10b test11a1 test11a2 test11a3 test11a4 test11a5 test11a10 test11a100 run11a test12a1 test12a2 test12a3 run12a test13a run13a test14a1 test14a2 test14a3 test14a4 test14a10 run14a test14b run14b

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

test10b:
	./cs10b test < ./data/10a-test.data

run10b:
	./cs10b < ./data/10a.data

test11a1:
	./cs11a test 1 < ./data/11a-test.data

test11a2:
	./cs11a test 2 < ./data/11a-test.data

test11a3:
	./cs11a test 3 < ./data/11a-test.data

test11a4:
	./cs11a test 4 < ./data/11a-test.data

test11a5:
	./cs11a test 5 < ./data/11a-test.data

test11a10:
	./cs11a test 10 < ./data/11a-test.data

test11a100:
	./cs11a nope 100 < ./data/11a-test.data

run11a:
	./cs11a nope 100 < ./data/11a.data

test11b:
	./cs11a nope 200 < ./data/11a-test.data

run11b:
	./cs11a nope 1000 < ./data/11a.data

test12a1:
	./cs12a test < ./data/12a-test1.data

test12a2:
	./cs12a test < ./data/12a-test2.data

test12a3:
	./cs12a test < ./data/12a-test3.data

test13a:
	./cs13a test 1 < ./data/13a-test.data

test13a0:
	./cs13a test 0 < ./data/13a-test.data

run13a:
	./cs13a nope 1 < ./data/13a.data

run13b:
	./cs13a nope 0 < ./data/13a.data

test14a10:
	./cs14a test 10 < ./data/14a-test.data

test14a1:
	./cs14a test 1 < ./data/14a-test.data

test14a2:
	./cs14a test 2 < ./data/14a-test.data

test14a3:
	./cs14a test 3 < ./data/14a-test.data

test14a4:
	./cs14a test 4 < ./data/14a-test.data

run14a:
	./cs14a nope 10 < ./data/14a.data

test14b1:
	./cs14b test 1 < ./data/14a-test.data

test14b2:
	./cs14b test 2 < ./data/14a-test.data

test14b3:
	./cs14b test 3 < ./data/14a-test.data

test14b4:
	./cs14b test 4 < ./data/14a-test.data

test14b10:
	./cs14b test 10 < ./data/14a-test.data

test14b:
	./cs14b nope 40 < ./data/14a-test.data

run14b:
	./cs14b nope 10 < ./data/14a.data

test16a:
	./cs16a test < ./data/16a-test$(n).data

test16a1:
	./cs16a test < ./data/16a-test1.data

test16a2:
	./cs16a test < ./data/16a-test2.data

test16a3:
	./cs16a test < ./data/16a-test3.data

test16a4:
	./cs16a test < ./data/16a-test4.data

test16a5:
	./cs16a test < ./data/16a-test5.data

test16a6:
	./cs16a test < ./data/16a-test6.data

test16a7:
	./cs16a test < ./data/16a-test7.data

run16a:
	./cs16a nope < ./data/16a.data


