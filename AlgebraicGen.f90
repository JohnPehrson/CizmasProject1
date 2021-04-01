! A subroutine that computes the algebraic grid
! Written by Clark Pehrson 
subroutine AlgebraicGen(Imax,Jmax,X_L,X_R,Y_U,Zmat,Nmat,Xmat,Ymat,dz,dn) 
implicit none 

INTEGER     :: IMax, JMax          ! nodes in each direction
REAL  :: X_L                 ! Left Boundary Condition
REAL :: X_R         ! Right Boundary Condition
REAL :: Y_Lcalc     ! Lower Boundary Condition  Is a seperate function
REAL :: Y_U         ! Upper Boundary Condition
REAL :: dz          ! node differences
REAL :: dn

REAL, DIMENSION(IMax,JMax) :: Zmat    !Matraxies initialization
REAL, DIMENSION(IMax,JMax) :: Nmat
REAL, DIMENSION(IMax,JMax) :: Xmat
REAL, DIMENSION(IMax,JMax) :: Ymat

INTEGER :: i                 !Iteration Variables
INTEGER :: j

! Generate the Z,N grid
DO i = 1, Imax
    DO j = 1, Jmax
        Zmat(i,j) = (REAL(i,4)-1)/(REAL(Imax,4)-1)
        Nmat(i,j) = (REAL(j,4)-1)/(REAL(Jmax,4)-1)
    END DO
END DO

!find node differences
dz = Zmat(2,1)-Zmat(1,1)
dn = Nmat(1,2)-Nmat(1,1)

!Map to the X,Y plane 
DO i = 1, Imax
    DO j = 1, Jmax
        Xmat(i,j) = X_L + Zmat(i,j)*(X_R-X_L)
        Ymat(i,j) = Y_Lcalc(Xmat(i,j))+Nmat(i,j)*(Y_U-Y_Lcalc(Xmat(i,j)))
    END DO
END DO

end subroutine AlgebraicGen