# Eric Laursen, 24 April, CS 442P-003 HW 2
# Makefile for hw2 - Hexapawn solver

OBJS = hw2.o board.o

.adb.o:
	gcc -c -gnat2012 -O3 $<

.SUFFIXES: .adb .o

hw2:	$(OBJS)
	gnatbind hw2.ali
	gnatlink hw2.ali

clean:
	rm -f *~ *.o *.ali hw2

tidy:
	rm -f *~
