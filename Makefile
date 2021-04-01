FC=gfortran
FFLAGS= -Wall -Wextra -std=f2008
SRC =Y_Lcalc.f90 AlgebraicGen.f90 GridPlotter.f90 LaplaceGrid.f90 o1findifCentral.f90 o1findifBackward.f90 o1findifForward.f90 o2findifCentral.f90 o2findifForFor.f90 o2findifForBack.f90 o2findifBackFor.f90 o2findifBackBack.f90 bumpnormal.f90 bumpBC.f90 abg.f90 LaplaceNodeLogic.f90 AlgToLaplaceGrid.f90 bumpcornermover.f90
OBJ=${SRC:.f90=.o}

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

LaplaceGrid: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	@rm -f *.mod *.o LaplaceGrid