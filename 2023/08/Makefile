.SUFFIXES:
all: aoc_day8.o aoc_tree.o
	gfortran main.f08 *.o

aoc_day8.o: aoc_tree.mod aoc_day8.f08
	gfortran -c aoc_day8.f08

%.mod: %.f08
	gfortran -c $<

clean:
	rm -rf *.o
	rm -rf *.mod
