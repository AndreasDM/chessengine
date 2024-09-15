all:
	g++ main.cpp -Ofast -march=native -flto -lfmt -lbenchmark

test:
	g++ main.cpp -D TESTS -lfmt -lbenchmark

bench:
	g++ main.cpp -D BENCH -Ofast -march=native -flto -lfmt -lbenchmark

run: all
	./a.out

.PHONY: clean
clean:
	rm a.out
