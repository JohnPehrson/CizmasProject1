FUNCTION bumpBC(IMax,JMax,i,j,x,y) result(xnew)
    IMPLICIT NONE
    !inputs
    INTEGER, INTENT(in) :: i    ! input index i
    INTEGER, INTENT(in) :: j    ! input index j
    INTEGER  :: IMax, JMax      ! nodes in each direction, used to define the matrix
    REAL, DIMENSION(IMax,JMax) :: x  !the input matrix x or y
    REAL, DIMENSION(IMax,JMax) :: y  !the input matrix x or y
    REAL:: bumpnormal
    REAL :: xloc
    !temp value
    REAL :: normal
    !output
    REAL :: xnew ! outputs the new x or y value

    xloc = x(i,j)
    normal = bumpnormal(xloc)
    xnew = (1/normal)*(y(i,1)-(4./3.)*y(i,2)+(1./3.)*y(i,3))+(4./3.)*x(i,2)-(1./3.)*x(i,3)

end FUNCTION bumpBC