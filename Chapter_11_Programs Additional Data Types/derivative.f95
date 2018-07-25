PROGRAM derivative
    USE derivative_mod
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       to find the derivative of a function at a point x_0
    !       Source: Exercise 11-6

    !   Variable Declarations
    INTEGER :: start_time   ! execution start time
    INTEGER :: end_time     ! execution end time
    REAL(kind=dbl) :: x0    ! position to find derivative at
    REAL(kind=dbl) :: dx    ! delta x
    REAL(kind=dbl) :: deriv ! result of derivative
    ! REAL(kind=dbl) :: func  ! type of function

    !   start timer
    CALL SYSTEM_CLOCK(start_time)

    ! find the derivative
    x0 = 0
    dx = 0.00001
    CALL derivative_calculator(func, x0, dx, deriv)

    ! return results
    WRITE (unit=*, fmt=*) deriv



    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM derivative
