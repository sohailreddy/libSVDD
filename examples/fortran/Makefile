FCOMPILER = gfortran


EXE = example1
SVDDDIR =../../src/
LIBSVDD = libSVDD.a


OBJECTS = example1.o example2.o

	
FLAG = -I$(SVDDDIR) $(SVDDDIR)$(LIBSVDD) 

	
COMPILE_ALL = $(OBJECTS) 
COMPILE_ALL += LINK


COMPILE: $(COMPILE_ALL) 

example1.o: example1.f90
	$(FCOMPILER) example1.f90 $(FLAG) -c
example2.o: example2.f90
	$(FCOMPILER) example2.f90 $(FLAG) -c


LINK:
	$(FCOMPILER) example1.o $(FLAG) -o example1
	$(FCOMPILER) example2.o $(FLAG) -o example2


clean :
	rm 	$(OBJECTS) example1 example2
	

