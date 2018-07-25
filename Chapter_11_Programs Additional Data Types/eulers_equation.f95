PROGRAM eulers_equation
    !USE module_name
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To calculate Euler's Equation with my own algorithm and with the built in CEXP function
    !       Source: Exercise 11-11

    !   Parameter Decrations
    REAL, PARAMETER    :: pi         = 3.141592653 ! pi
    ! REAL, PARAMETER    :: deg_2_rad  = pi/180      ! Degree to radian conversion facter. Used like rad=deg*deg_2_rad
    INTEGER, PARAMeTER :: thetas_len = 3           ! set the number of angles to test at

    !   Variable Declarations
    INTEGER :: start_time   ! execution start time
    INTEGER :: end_time     ! execution end time
    INTEGER :: i            ! loop indices
    REAL, DIMENSION(thetas_len) :: thetas = (/0., pi/2., pi/) ! the theta values to use
    COMPLEX, DIMENSION(thetas_len) :: own_result               ! result from my own algorithm
    COMPLEX, DIMENSION(thetas_len) :: intr_result              ! result from instrinsic function

    !   start timer
    CALL SYSTEM_CLOCK(start_time)

    ! perform calculations
    DO i = 1, thetas_len
        own_result(i) = CMPLX(COS(thetas(i)), SIN(thetas(i)))
        intr_result(i) = CEXP(CMPLX(0, y=thetas(i)))
    END DO

    DO i = 1, thetas_len
        WRITE (unit=*, fmt=*) own_result(i), intr_result(i)
    END DO



    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM eulers_equation
