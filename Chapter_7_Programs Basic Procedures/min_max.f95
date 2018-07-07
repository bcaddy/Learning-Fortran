MODULE helper
    IMPLICIT NONE

    CONTAINS
        REAL FUNCTION f(x)
            !   arbitrary function.  I used the one the book told me to

            !   Declarations
            REAL, INTENT(IN)  :: x  ! input value

            !   computations
            f = x**3 - 5*x**2 + 5*x + 2

        END FUNCTION f

        SUBROUTINE extremum_finder(func, first_value, last_value, num_steps, xmin, xmin_value, xmax, xmax_value)
            !   This sub finds the extremum positions and values for an arbitrary function defined in the fortran function f

            !   Declarations
            ! inputs
            REAL, INTENT(IN) :: first_value     ! first value of x
            REAL, INTENT(IN) :: last_value      ! last value of x
            INTEGER, INTENT(IN) :: num_steps    ! number of steps between those values
            REAL :: func

            ! outputs
            REAL, INTENT(OUT) :: xmin            ! position of minimum
            REAL, INTENT(OUT) :: xmin_value      ! value of minimum
            REAL, INTENT(OUT) :: xmax            ! position of maximum
            REAL, INTENT(OUT) :: xmax_value      ! value of maximum


            WRITE (*,*) f(last_value)



        END SUBROUTINE extremum_finder







END MODULE helper

! ====================================================================================
PROGRAM min_max
    !   Purpose:
    !       To find the minimum and maximum values in a function over a certain range
    !       Source: Exercise 7-21 and 7-22

    !   Modules and IMPLICIT NONE statement
    USE helper
    IMPLICIT NONE !requires me to declare all variables

    !   Variable Declarations
    INTEGER :: start_time   ! execution start time
    INTEGER :: end_time     ! execution end time
    REAL :: func            ! the function is real
    REAL :: first_value=0     ! first value of x
    REAL :: last_value =0     ! last value of x
    INTEGER :: num_steps =0   ! number of steps between those values
    REAL :: xmin            ! position of minimum
    REAL :: xmin_value      ! value of minimum
    REAL :: xmax            ! position of maximum
    REAL :: xmax_value      ! value of maximum

    !	start timer
    CALL SYSTEM_CLOCK(start_time)





    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! time in seconds
    WRITE (*,999) (REAL(end_time) - REAL(start_time))/1000
    999 FORMAT (/,'Program complete. Time to execute ', F7.3 , ' seconds')

    ! time in minutes
    !WRITE (*,999) (REAL(end_time) - REAL(start_time))/60000
    !999 FORMAT (/,'Program complete. Time to execute ', f7.3 , ' minutes')

    ! time in hours
    !WRITE (*,999) (REAL(end_time) - REAL(start_time))/3.6E6
    !999 FORMAT (/,'Program complete. Time to execute ', f7.3 , ' hours')
END PROGRAM min_max
