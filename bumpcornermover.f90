SUBROUTINE bumpcornermover(IMax,JMax,x,y) 
    IMPLICIT NONE
        !this program will, after the final iteration, move the nearest node to the bump-corner into that corner
        !will likely depend on node count for accuracy
    !in variables
    INTEGER  :: IMax, JMax          ! nodes in each direction
    REAL, DIMENSION(IMax,JMax) :: x  !Matraxies initialization
    REAL, DIMENSION(IMax,JMax) :: y

    !temp variables
    INTEGER :: iminleft
    INTEGER :: iminright

    !used functions
    REAL :: Y_Lcalc

    iminleft = minloc(abs(x(:,1) - 2.), 1)
    iminright = minloc(abs(x(:,1) - 3.), 1)

    x(iminleft,1) = 2.
    y(iminleft,1) = Y_Lcalc(x(iminleft,1))
    x(iminright,1) = 3.
    y(iminright,1) = Y_Lcalc(x(iminright,1))

end SUBROUTINE bumpcornermover