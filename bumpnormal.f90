FUNCTION bumpnormal(xloc) result(normalslope)
    IMPLICIT NONE
    !inputs
    REAL ::  xloc    ! distance x along the bottom wall
    !static paramter
    REAL, PARAMETER :: pi = 3.141592
    !output
    REAL :: normalslope ! outputs the new x or y value

    normalslope = (-1.)/(0.2*pi*cos((xloc-2)*pi))

end FUNCTION bumpnormal