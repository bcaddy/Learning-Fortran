MODULE helper

    IMPLICIT NONE
    CONTAINS
        SUBROUTINE converter (cartesian, amp, phase)
            ! this subroutine converts a complex number from polar to cartesian coordinates

            ! declarations
            COMPLEX, INTENT(IN) :: cartesian ! the cartesian complex number
            REAL, INTENT(OUT)   :: amp       ! the magnitude of the complex number
            REAL, INTENT(OUT)   :: phase     ! the phase of the complex number

            amp = CABS(cartesian)
            phase = ATAN2(REAL(cartesian), AIMAG(cartesian))
        END SUBROUTINE converter
END MODULE helper


PROGRAM complex_polar_converter
    USE helper
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To convert a complex number to from cartesian to polar coordinates
    !       Source: Exercise 11-10

    !   Variable Declarations
    INTEGER :: start_time ! execution start time
    INTEGER :: end_time   ! execution end time
    COMPLEX :: cartesian  ! complex number in cartesian coordinates
    REAL    :: amp        ! the magnitude of the complex number
    REAL    :: phase      ! the phase of the complex number


    !   start timer
    CALL SYSTEM_CLOCK(start_time)

    ! intialize the complex number
    cartesian = (3., 4.)

    ! call the subroutine converter to convert between coordinates systems
    CALL converter(cartesian, amp, phase)

    WRITE (unit=*, fmt=100) amp, phase
    100 FORMAT ('The magnitude of the complex number is ', F7.2,', the phase of it is ', F7.2)


    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM complex_polar_converter
