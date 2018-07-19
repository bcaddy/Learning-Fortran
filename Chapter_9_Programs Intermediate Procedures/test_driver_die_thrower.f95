PROGRAM test_driver_die_thrower
    USE die_thrower
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       test driver program for the module die thrower
    !       Source: Exercise

    !   Variable Declarations
    INTEGER :: start_time   ! execution start time
    INTEGER :: end_time     ! execution end time
    INTEGER :: die1, die2   ! value of the dice

    !   start timer
    CALL SYSTEM_CLOCK(start_time)


    ! call the throw subroutine
    CALL throw(die1, die2)
    WRITE (unit=*, fmt=*) 'the value of the two die are', die1,'and', die2



    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM test_driver_die_thrower
