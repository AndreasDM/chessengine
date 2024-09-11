all:
	g++ main.cpp -Ofast -march=native -flto -lfmt -lbenchmark && ./a.out

test:
	g++ main.cpp -D TESTS -lfmt -lbenchmark && ./a.out

bench:
	g++ main.cpp -D BENCH -Ofast -march=native -flto -lfmt -lbenchmark && ./a.out

run: all
