#
CMPILER=gfortran

LIBS=-lpgplot -lX11

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
	$(CMPILER) $(CFLAGS) -c $< -o obj/$@


clean:
	rm -f obj/*.o coredump/*.coredump *~ $(PROGRAM) *.bak
docs: Doxyfile
	doxygen Doxyfile

Doxyfile:
	doxygen -g Doxyfile

indent:
	indent $(IFLAGS) *.f

