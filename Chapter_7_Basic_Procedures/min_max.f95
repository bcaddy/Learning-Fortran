MODULE helper

    CONTAINS
        FUNCTION func(x)
            !   arbitrary function.  I used the one the book told me to

            !   Declarations
            REAL, INTENT(IN)  :: x  ! input value
            REAL :: func               ! the value of the function

            !   computations
            func = x**3 - 5*x**2 + 5*x + 2

        END FUNCTION func

        SUBROUTINE extremum_finder(func, first_value, last_value, num_steps, xmin, xmin_value, xmax, xmax_value)
            !   This sub finds the extremum positions and values for an arbitrary function defined in the fortran function f

            !   Declarations
            ! inputs
            REAL, INTENT(IN) :: first_value     ! first value of x
            REAL, INTENT(IN) :: last_value      ! last value of x
            INTEGER, INTENT(IN) :: num_steps    ! number of steps between those values

            ! local variables
            INTEGER :: i ! loop index
            REAL :: step_size   ! size of each step
            REAL :: x           ! value to put into function
            REAL :: value       ! value of the function at a specific x
            REAL :: func        ! declare function

            ! outputs
            REAL, INTENT(OUT) :: xmin            ! position of minimum
            REAL, INTENT(OUT) :: xmin_value      ! value of minimum
            REAL, INTENT(OUT) :: xmax            ! position of maximum
            REAL, INTENT(OUT) :: xmax_value      ! value of maximum

            ! set up for loop
            x = first_value
            value = func(x)
            xmin = x
            xmax = x
            xmin_value = value
            xmax_value = value

            ! Find the mins and maxes
            step_size = (last_value - first_value) / REAL(num_steps) ! size of each step

            DO i = 1, num_steps
                ! update x
                x = x + step_size

                ! calculate the value of the function
                value = func(x)

                ! perform comparisons
                IF (value > xmax_value) THEN
                    xmax_value = value
                    xmax = x
                ELSE IF (value < xmin_value) THEN
                    xmin_value = value
                    xmin = x
                END IF
            END DO
        END SUBROUTINE extremum_finder



END MODULE helper

! ====================================================================================
PROGRAM min_max
    USE helper
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To find the minimum and maximum values in a function over a certain range
    !       Source: Exercise 7-21 and 7-22

    !   Variable Declarations
    INTEGER :: start_time   ! execution start time
    INTEGER :: end_time     ! execution end time
    REAL :: first_value=-1     ! first value of x
    REAL :: last_value=3     ! last value of x
    INTEGER :: num_steps=200   ! number of steps between those values
    REAL :: xmin            ! position of minimum
    REAL :: xmin_value      ! value of minimum
    REAL :: xmax            ! position of maximum
    REAL :: xmax_value      ! value of maximum

    !	start timer
    CALL SYSTEM_CLOCK(start_time)


    !   call subroutine
    CALL extremum_finder(func, first_value, last_value, num_steps, xmin, xmin_value, xmax, xmax_value)


    ! return values
    WRITE (*,100) xmin_value, xmin, xmax_value, xmax
    100 FORMAT ('The minimum value of the function is ', F7.2, ' at x = ', F7.2, /, &
                'The maximum value of the function is ', F7.2, ' at x = ', F7.2)

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
