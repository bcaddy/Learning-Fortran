MODULE vec_ops

CONTAINS
    FUNCTION crosser (vector1, vector2)
        IMPLICIT NONE
        ! find the cross product of two vectors

        ! Declarations
        REAL, DIMENSION(3), INTENT(IN) :: vector1, vector2
        REAL, DIMENSION(3) :: crosser

        !   Cross product
        crosser(1) = vector1(2) * vector2(3) - vector1(3) * vector2(2)
        crosser(2) = vector1(3) * vector2(1) - vector1(1) * vector2(3)
        crosser(3) = vector1(1) * vector2(2) - vector1(2) * vector2(1)

    END FUNCTION crosser
END MODULE vec_ops


PROGRAM cross_prod
    USE vec_ops
    IMPLICIT NONE !requires me to declare all variables

	!   Purpose:
	!       Basic function and module to calculate the cross product of two vectors
	!       Source: Exercise 7-19


	!   Variable Declarations
    INTEGER :: start_time   ! execution start time
    INTEGER :: end_time     ! execution end time
    REAL, DIMENSION(3) :: vec1=(/-2., 4., 0.5/), vec2 = (/0.5, 3., 2./)  ! vectors
    REAL, DIMENSION(3) :: cross ! cross product

	!	start timer
    CALL SYSTEM_CLOCK(start_time)


    !   Call and use function
    cross = crosser(vector1 = vec1, vector2 = vec2)
    WRITE (*,100) cross
    100 FORMAT ('The cross product is ', 3F6.2)


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
END PROGRAM cross_prod

