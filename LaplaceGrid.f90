PROGRAM  LaplaceGrid
   IMPLICIT  NONE
    !Written by John C. Pehrson
    !March 11, 2021
    !Laplace Grid generator written for Aero 489 Project 1

   INTEGER  :: IMax, JMax          ! nodes in each direction
   REAL     :: X_L                 ! Left Boundary Condition
   REAL     :: X_R                 ! Right Boundary Condition
   !real     :: Y_Lcalc             ! Lower Boundary Condition  used in a subroutine currently
   REAL     :: Y_U                 ! Upper Boundary Condition
   REAL, DIMENSION(:,:),ALLOCATABLE :: Zmat    !Set up matraxies for planes, but don't size
   REAL, DIMENSION(:,:),ALLOCATABLE :: Nmat
   REAL, DIMENSION(:,:),ALLOCATABLE :: Xmat
   REAL, DIMENSION(:,:),ALLOCATABLE :: Ymat
   REAL     :: dz               ! node differences
   REAL     :: dn  

   !variables for iterative solutions
    INTEGER :: iterationcount
    REAL :: itmaxerr

! Define preliminary variables and create Matraxies
IMax = 80
JMax = 20
X_L = 0.
X_R = 5.
Y_U = 1.
ALLOCATE ( Zmat(IMax,JMax) )    !Size plane matraxies
ALLOCATE ( Nmat(IMax,JMax) )
ALLOCATE ( Xmat(IMax,JMax) )
ALLOCATE ( Ymat(IMax,JMax) )
iterationcount = 0
itmaxerr = 1. !defined as a large number to start the while loop in the AlgToLaplace function


! Call the Algebraic Grid Generator Subroutine 
call AlgebraicGen(IMax,JMax,X_L,X_R,Y_U,Zmat,Nmat,Xmat,Ymat,dz,dn)

!Plot the data
call Gridplotter(IMax,JMax,Xmat,Ymat,0)

!Transform from Algebraic Grid to Laplace Grid
call AlgToLaplaceGrid(IMax,JMax,Xmat,Ymat,dz,dn,itmaxerr,iterationcount)

!Plot the data
call Gridplotter(IMax,JMax,Xmat,Ymat,1)

!Written Information about the execution of the program
WRITE(*,*) 'Number of Iterations',iterationcount
WRITE(*,*) 'Error at final iteration', itmaxerr 

END PROGRAM  LaplaceGrid