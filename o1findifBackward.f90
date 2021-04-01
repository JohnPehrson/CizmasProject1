FUNCTION o1findifBackward(IMax,JMax,i,j,dz,dn,XORY,switchnum) result(findif)
   IMPLICIT NONE
    !inputs
    INTEGER, INTENT(in) :: i    ! input index i
    INTEGER, INTENT(in) :: j    ! input index j
    REAL, INTENT(in) :: dz   ! input delta_z
    REAL, INTENT(in) :: dn   ! input delta_n
    INTEGER  :: IMax, JMax      ! nodes in each direction, used to define the matrix
    REAL, DIMENSION(IMax,JMax) :: XORY  !the input matrix x or y
    INTEGER :: switchnum
    
    !output
    REAL :: findif ! output

    !are we computing a first-order diference of the variable with respect to z/xi (1) or n (2)
    IF (switchnum == 1) THEN
        findif = (XORY(i,j)-XORY(i-1,j))/(dz)
     ELSE IF (switchnum == 2) THEN
        findif = (XORY(i,j)-XORY(i,j-1))/(dn)
     ELSE
        ! print error
        WRITE(*,*) "Bad switchnumber in 1st order backward difference"
     END IF


end FUNCTION o1findifBackward