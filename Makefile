all:
	g++ main.cpp -Ofast -march=native -flto -lfmt -lbenchmark && ./a.out

run: all
