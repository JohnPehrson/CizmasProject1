FUNCTION Y_Lcalc(xloc) result(Y_L)
   IMPLICIT NONE
    REAL, INTENT(in) :: xloc ! input
    REAL             :: Y_L ! output
    
    if (xloc<=2 .and. xloc>=0) then 
        ! left of the bump
        Y_L = 0
     else if (xloc>2 .and. xloc<3) then       
        ! on the bump   
        Y_L = 0.2*sin(3.141592*(xloc-2))
     else if (xloc>=3 .and. xloc<=5) then       
        ! right of the bump  
        Y_L = 0
     else       
        ! print error
        WRITE(*,*) "Points are outside of the x domain, issue found in the YL function"
     endif


end FUNCTION Y_Lcalc