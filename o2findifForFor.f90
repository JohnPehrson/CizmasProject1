FUNCTION o2findifForFor(IMax,JMax,i,j,dz,dn,XORY,alpha,beta,gamma) result(XORYnew)
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
    REAL, DIMENSION(5) :: aff
    !output
    REAL :: XORYnew ! outputs the new x or y value

    aff = [-2.*alpha/(dz**2.)+2.*beta/(dz*dn),-2.*gamma/(dn**2.)+2.*beta/(dn*dz),-2.*beta/(dz*dn),&
    alpha/(dz**2.),gamma/(dn**2.)]/(-1.*(alpha/(dz**2.)-2.*beta/(dz*dn)+gamma/(dn**2.)))

    XORYnew = aff(1)*XORY(i+1,j)+aff(2)*XORY(i,j+1)+aff(3)*XORY(i+1,j+1)+aff(4)*XORY(i+2,j)&
    +aff(5)*XORY(i,j+2)


end FUNCTION o2findifForFor