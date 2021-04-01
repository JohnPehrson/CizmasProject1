FUNCTION o2findifCentral(IMax,JMax,i,j,dz,dn,XORY,alpha,beta,gamma) result(XORYnew)
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
    REAL, DIMENSION(8) :: ac
    !output
    REAL :: XORYnew ! outputs the new x or y value

    ac = [-beta/(2.*dz*dn),gamma/(dn**2.),beta/(2.*dz*dn),alpha/(dz**2.),alpha/(dz**2.),beta/(2.*dz*dn),&
    gamma/(dn**2.),-beta/(2.*dz*dn)]/(2.*alpha/(dz**2.)+2.*gamma/(dn**2.))

    XORYnew = ac(1)*XORY(i-1,j-1)+ac(2)*XORY(i,j-1)+ac(3)*XORY(i+1,j-1)+ac(4)*XORY(i-1,j)&
    +ac(5)*XORY(i+1,j)+ac(6)*XORY(i-1,j+1)+ac(7)*XORY(i,j+1)+ac(8)*XORY(i+1,j+1)
    

end FUNCTION o2findifCentral