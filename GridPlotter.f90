! A subroutine to write data to the output text files
! and then call those text files to plot the grid
subroutine Gridplotter(Imax,Jmax,Xmat,Ymat,switchtype) 
    implicit none 

    INTEGER  :: IMax, JMax          ! nodes in each direction
        
    REAL, DIMENSION(IMax,JMax) :: Xmat
    REAL, DIMENSION(IMax,JMax) :: Ymat
        
    INTEGER :: i                 !Iteration Variables
    INTEGER :: j

    INTEGER :: switchtype        !0 if algebraic, 1 if laplace

   !temp out file stuff
    character(len=*), parameter :: FILE_NAMEX = 'dataX.txt'   ! File name
    integer            :: filenumberX                        ! File reference number
    character(len=*), parameter :: FILE_NAMEY = 'dataY.txt'   ! File name
    integer            :: filenumberY                        ! File reference number

    ! Write to an external Data File
    open (newunit=filenumberY, action='write', file=FILE_NAMEY, status='replace')  !plots columns

    ! loops through, each row has a unique data point for plotting in columns
    DO i = 1, Imax
        DO j = 1, Jmax
            write (filenumberY, *) Xmat(i,j), Ymat(i,j)
        END DO
        write (filenumberY,*)
    END DO

    close(filenumberY)

    ! Write to an external Data File
    open (newunit=filenumberX, action='write', file=FILE_NAMEX, status='replace')  !plots rows

    ! loops through, each row has a unique data point for plotting in columns
    DO j = 1, Jmax
        DO i = 1, Imax
            write (filenumberX, *) Xmat(i,j), Ymat(i,j)
        END DO
        write (filenumberX,*)
    END DO

    close(filenumberX)


    !  Plot the Data
    if (switchtype == 0) then
        call execute_command_line('gnuplot -p scatterplot.plt')   !calls a script file plot plt that directs to the data
    else if (switchtype == 1) then
        call execute_command_line('gnuplot -p scatterplotlaplace.plt')   !calls a script file plot plt that directs to the data
    endif

end subroutine Gridplotter