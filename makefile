#
CMPILER=gfortran

LIBS=-lpgplot -lX11 -L/usr/lib -lfortran_stdlib -I/usr/include/fortran_stdlib/GNU-12.2.1/

CFLAGS=-Wall 

PROGRAM=main

PROGRAM_OPT=args

PROGRAM_OBJS=main.o 

PROGRAM_OBJSDIR=obj/main.o

.PHONY: clean

all: $(PROGRAM)

depuracao: CFLAGS += -D SHOW_DEBUG
depuracao: $(PROGRAM)

$(PROGRAM): $(PROGRAM_OBJS)
	$(CMPILER) $(CFLAGS) -o $@ $(PROGRAM_OBJSDIR) $(LIBS)

main.o: main.f

%.o : %.f
	$(CMPILER) $(CFLAGS) -c $< -o obj/$@ $(LIBS)


clean:
	rm -f obj/*.o coredump/*.coredump *~ $(PROGRAM) *.bak
docs: Doxyfile
	doxygen Doxyfile

Doxyfile:
	doxygen -g Doxyfile

indent:
	indent $(IFLAGS) *.f

