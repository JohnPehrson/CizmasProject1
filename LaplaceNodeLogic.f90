SUBROUTINE LaplaceNodeLogic(i,j,IMax,JMax,alpha,beta,gammag,x,y,dz,dn,maxerr) 
    IMPLICIT NONE

    !defining input variable types
    INTEGER, INTENT(in) :: i !index of node in 'x'
    INTEGER, INTENT(in) :: j !index of node in 'y'
    INTEGER, INTENT(in) :: IMax !maximum number of subdivisions in 'x'
    INTEGER, INTENT(in) :: JMax !maximum number of subdivisions in 'y'
    REAL, INTENT(in) :: alpha !single variable alpha/beta/gammag at the node
    REAL, INTENT(in) :: beta
    REAL, INTENT(in) :: gammag
    REAL, DIMENSION(IMax,JMax) :: x  !the input matrix x or y
    REAL, DIMENSION(IMax,JMax) :: y  !the input matrix x or y
    REAL, INTENT(in) :: dz !delta_z
    REAL, INTENT(in) :: dn !delta_n
    REAL :: maxerr

    !defining boundary conditions that I don't want to pass between functions
    REAL :: XL
    REAL :: XR 
    REAL :: YU

    !defining internal error tracking and placeholder variables
    REAL :: xnew
    REAL :: ynew

    !define functions for Gauss-Seidel and boundary
    REAL :: o2findifBackBack
    REAL :: o2findifBackFor
    REAL :: o2findifCentral
    REAL :: o2findifForBack
    REAL :: o2findifForFor
    REAL :: Y_Lcalc
    REAL :: bumpBC

    !defining bc.
    XL = 0.
    XR = 5.
    YU = 1.


    !NODE LOGIC
        !this determines where in the grid the node is located
        !prescribes a Gauss-Seidel formulation to solve for node movement
    if (i>=2 .AND. i<=(IMax-1) .AND. j>=2 .AND. j<=(JMax-1)) then  !central derivatives
        xnew = o2findifCentral(IMax,JMax,i,j,dz,dn,x,alpha,beta,gammag)
        ynew = o2findifCentral(IMax,JMax,i,j,dz,dn,y,alpha,beta,gammag)
     else if ((i==1 .AND. j==1) .OR. (i==1 .AND. j==1) .OR. (i==IMax .AND. j==1) .OR. (i==IMax .AND. j==JMax)) then !corners 
        xnew = x(i,j)
        ynew = y(i,j)
     else if (j==1) then !bottom wall     
        if (x(i,j)<(XR/5.)) then !forfor, flat wall left of bump, left side
            xnew = o2findifForFor(IMax,JMax,i,j,dz,dn,x,alpha,beta,gammag)
            ynew = Y_Lcalc(xnew)
        else if (x(i,j)<=(2.*XR/5.)) then !backfor, flat wall left of bump, right side
            xnew = o2findifBackFor(IMax,JMax,i,j,dz,dn,x,alpha,beta,gammag)
            ynew = Y_Lcalc(xnew)
        else if ((x(i,j)>(2.*XR/5.)) .AND. (x(i,j)<=(3.*XR/5.))) then !bump
            xnew = bumpBC(IMax,JMax,i,j,x,y)
            ynew = Y_Lcalc(xnew)
        else if ((x(i,j)>=(3.*XR/5.)) .AND. (x(i,j)<=(4.*XR/5.))) then !forfor, flat wall right of bump, left side
            xnew = o2findifForFor(IMax,JMax,i,j,dz,dn,x,alpha,beta,gammag)
            ynew = Y_Lcalc(xnew)
        else if (x(i,j)>(4.*XR/5.)) then !backfor, flat wall right of bump, right side
            xnew = o2findifBackFor(IMax,JMax,i,j,dz,dn,x,alpha,beta,gammag)
            ynew = Y_Lcalc(xnew)
        else
            ! print error
            WRITE(*,*) "Error in bump node logic"
        endif

     else if (j==JMax) then !top wall
        if (i<=(REAL(IMax,4)/2)) then !forback, top wall left side
            xnew = o2findifForBack(IMax,JMax,i,j,dz,dn,x,alpha,beta,gammag)
            ynew = YU
        else if (i>(REAL(IMax,4)/2)) then !backback, top wall right side
            xnew = o2findifBackBack(IMax,JMax,i,j,dz,dn,x,alpha,beta,gammag)
            ynew = YU
        endif

     else if (i==1) then !left wall
        if (j<=(REAL(JMax,4)/2)) then !forfor, left wall lower half
            xnew = XL
            ynew = o2findifForFor(IMax,JMax,i,j,dz,dn,y,alpha,beta,gammag)
        else if (j>(REAL(JMax,4)/2)) then !forback, left wall upper half
            xnew = XL
            ynew = o2findifForBack(IMax,JMax,i,j,dz,dn,y,alpha,beta,gammag)
        endif

     else if (i==Imax) then !right wall
        if (j<=(REAL(JMax,4)/2)) then !backfor, right wall lower half
            xnew = XR
            ynew = o2findifBackFor(IMax,JMax,i,j,dz,dn,y,alpha,beta,gammag)
        else if (j>(REAL(JMax,4)/2)) then !backback, right wall upper half
            xnew = XR 
            ynew = o2findifBackBack(IMax,JMax,i,j,dz,dn,y,alpha,beta,gammag)
        endif

     else       
        ! print error
        WRITE(*,*) "Error in node logic"
     endif


     !calculate error
     if (abs(x(i,j)-xnew)>maxerr) then
        maxerr = abs(x(i,j)-xnew)
     else if (abs(y(i,j)-ynew)>maxerr) then
        maxerr = abs(y(i,j)-ynew)
     endif

     !replace the x and y values in their matraxies with xnew, ynew
     x(i,j) = xnew
     y(i,j) = ynew

end SUBROUTINE LaplaceNodeLogic