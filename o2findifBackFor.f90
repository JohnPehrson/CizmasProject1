FUNCTION o2findifBackFor(IMax,JMax,i,j,dz,dn,XORY,alpha,beta,gamma) result(XORYnew)
    IMPLICIT NONE
    !inputs
    INTEGER, INTENT(in) :: i    ! input index i
    INTEGER, INTENT(in) :: j    ! input index j
    REAL, INTENT(in) :: dz   ! input delta_z
    REAL, INTENT(in) :: dn   ! input delta_n
    REAL, INTENT(in) :: alpha   
    REAL, INTENT(in) :: beta   
    REAL, INTENT(in) :: gamma   
    INTEGER  :: IMax, JMax      ! nodes in each direction, used to define the matrix
    REAL, DIMENSION(IMax,JMax) :: XORY  !the input matrix x or y
   !temporary array to hold values nicely
    REAL, DIMENSION(5) :: abf
    !output
    REAL :: XORYnew ! outputs the new x or y value

    abf = [-2.*alpha/(dz**2.)-2.*beta/(dz*dn),-2.*gamma/(dn**2.)-2.*beta/(dz*dn),&
    2.*beta/(dz*dn),alpha/(dz**2.),gamma/(dn**2.)]/(-1.*(alpha/(dz**2.)+2.*beta/(dz*dn)+gamma/(dn**2.)))

    XORYnew = abf(1)*XORY(i-1,j)+abf(2)*XORY(i,j+1)+abf(3)*XORY(i-1,j+1)+abf(4)*XORY(i-2,j)+abf(5)*XORY(i,j+2)

end FUNCTION o2findifBackFor