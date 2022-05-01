all:
	swipl -q -g main -o flp21-log -c src/rubiks_cube.pl
clean:
	rm -rf flp21-log
run:
	time ./flp21-log < test/test1.in
	time ./flp21-log < test/test2.in