.SUFFIXES:
all: aoc_day7.o aoc_sets.o
	gfortran main.f08 *.o

aoc_day7.o: aoc_sets.mod aoc_day7.f08
	gfortran -c aoc_day7.f08

%.mod: %.f08
	gfortran -c $<
	

clean:
	rm -rf *.o
	rm -rf *.mod
