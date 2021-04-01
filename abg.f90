! A subroutine that computes the algebraic grid
! Written by Clark Pehrson 
subroutine abg(IMax,JMax,x,y,dz,dn,alpha,beta,gammag) 
    implicit none 
    
    INTEGER  :: IMax, JMax          ! nodes in each direction
    REAL :: dz
    REAL :: dn
    REAL :: xz      ! initialize derivatives
    REAL :: xn
    REAL :: yz
    REAL :: yn
    REAL :: o1findifCentral     !first order finite differences
    REAL :: o1findifForward     !first order finite differences
    REAL :: o1findifBackward    !first order finite differences
    REAL, DIMENSION(IMax,JMax) :: x  !Matraxies initialization
    REAL, DIMENSION(IMax,JMax) :: y
    REAL, DIMENSION(IMax,JMax) :: alpha 
    REAL, DIMENSION(IMax,JMax) :: beta
    REAL, DIMENSION(IMax,JMax) :: gammag

    INTEGER :: i                 !Iteration Variables
    INTEGER :: j

    do j = 1, JMax      !loop over rows
        do i = 1, IMax  !loop over columns   
        
            !now identify where the node is in the grid so I know what FD method to use
            
            !derivative in n
            IF (j == 1) THEN !bottom wall
                xn = o1findifForward(IMax,JMax,i,j,dz,dn,x,2)
                yn = o1findifForward(IMax,JMax,i,j,dz,dn,y,2)
             ELSE IF (j == JMax) THEN !top wall
                xn = o1findifBackward(IMax,JMax,i,j,dz,dn,x,2)
                yn = o1findifBackward(IMax,JMax,i,j,dz,dn,y,2)
             ELSE !centered between top and bottom walls
                xn = o1findifCentral(IMax,JMax,i,j,dz,dn,x,2)
                yn = o1findifCentral(IMax,JMax,i,j,dz,dn,y,2)
             END IF

            !derivative in z
             IF (i == 1) THEN !left wall
                xz = o1findifForward(IMax,JMax,i,j,dz,dn,x,1)
                yz = o1findifForward(IMax,JMax,i,j,dz,dn,y,1)
             ELSE IF (i == IMax) THEN !right wall
                xz = o1findifBackward(IMax,JMax,i,j,dz,dn,x,1)
                yz = o1findifBackward(IMax,JMax,i,j,dz,dn,y,1)
             ELSE !centered between left and right wall
                xz = o1findifCentral(IMax,JMax,i,j,dz,dn,x,1)
                yz = o1findifCentral(IMax,JMax,i,j,dz,dn,y,1)
             END IF

             !compute the alpha,beta,gammag using the intermediate finite differences
             alpha(i,j) = xn**2. + yn**2.
             beta(i,j) = xz*xn+yz*yn
             gammag(i,j) = xz**2.+yz**2.
             
        end do
     end do





end subroutine abg