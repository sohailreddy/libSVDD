FCOMPILER = gfortran
LIB_COMPILER = ar


EXE = SVDD
LIBNAME = libSVDD


OBJECTS = slsqp_kinds.o slsqp_support.o slsqp_core.o svdd_mod.o svdd.o

LIBFLAG = rc
FLAG = -x f95-cpp-input -g -fcheck=all -fbacktrace -fdec-math

	
	
COMPILE_ALL = $(OBJECTS) 
COMPILE_ALL += CREATE_LIB


COMPILE: $(COMPILE_ALL) 

slsqp_kinds.o: slsqp_kinds.f90
	$(FCOMPILER) $(FLAG) -c slsqp_kinds.f90
slsqp_support.o: slsqp_support.f90
	$(FCOMPILER) $(FLAG) -c slsqp_support.f90
slsqp_core.o: slsqp_core.f90
	$(FCOMPILER) $(FLAG) -c slsqp_core.f90
svdd_mod.o: svdd_mod.f90
	$(FCOMPILER) $(FLAG) -c svdd_mod.f90	
svdd.o: svdd.f90
	$(FCOMPILER) $(FLAG) -c svdd.f90


LINK:
	@$(FCOMPILER) $(OBJECTS) $(FLAG) -o $(EXE)
 
CREATE_LIB: 
	@$(LIB_COMPILER) $(LIBFLAG) $(LIBNAME).a *.o
	@$(FCOMPILER) -shared -O2 $(OBJECTS) -o $(LIBNAME).so -fPIC	


clean :
	rm 	$(OBJECTS) *.mod 
	

