SUBROUTINE AlgToLaplaceGrid(IMax,JMax,x,y,dz,dn,itmaxerr,iterationcount)
    IMPLICIT NONE

    !coming in
    INTEGER, INTENT(in) :: IMax         ! nodes in each direction
    INTEGER, INTENT(in) :: JMax         ! nodes in each direction
    REAL, DIMENSION(IMax,JMax) :: x  !Matraxies initialization
    REAL, DIMENSION(IMax,JMax) :: y
    REAL, INTENT(in) :: dz
    REAL, INTENT(in) :: dn
    REAL :: itmaxerr
    INTEGER :: iterationcount

    !internally defined 
    REAL, DIMENSION(IMax,JMax) :: alpha 
    REAL, DIMENSION(IMax,JMax) :: beta
    REAL, DIMENSION(IMax,JMax) :: gammag
    INTEGER :: i                 !Iteration Variables
    INTEGER :: j
    INTEGER :: MAXIT
    REAL :: errorlimit
    REAL :: nodeerror

    !define initialized local variables
    MAXIT = 500
    errorlimit = 0.0001

    !While loop to converge the grid
    do while ((iterationcount<MAXIT) .AND. (itmaxerr>errorlimit)) 
       iterationcount = iterationcount+1
       itmaxerr = 0

       !calculate alpha,beta,gammag
       CALL abg(IMax,JMax,x,y,dz,dn,alpha,beta,gammag) !fills out alpha,beta,gammag matraxies
        
       !do loops to filter through the nodes in the grid
       do j = 1, JMax
            do i = 1, IMax
                nodeerror = 0.
                CALL LaplaceNodeLogic(i,j,IMax,JMax,alpha(i,j),beta(i,j),gammag(i,j),x,y,dz,dn,nodeerror) 
                !if statement to check if the node error is the max error in the whole grid
                if (nodeerror>itmaxerr) then !replace the max error with node error
                    itmaxerr = nodeerror
                endif
            enddo
        enddo
    end do

    !airfoil/bump corner mover
    CALL bumpcornermover(IMax,JMax,x,y) 


end SUBROUTINE AlgToLaplaceGrid