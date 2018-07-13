MODULE helper
    CONTAINS
        SUBROUTINE carry_sorter(a, b, asort, bsort)
            ! Purpose:
            !   Perform a carry sort with two arrays

            ! declarations
            REAL, DIMENSION(9), INTENT(INOUT)  :: a, b
            REAL, DIMENSION(9), INTENT(OUT) :: asort, bsort
            REAL :: init_max ! initial maximum value in array a
            INTEGER :: i

            ! set intial value
            init_max=MAXVAL(a)

            ! sort the lists
            DO i = 1, 9
                asort(i) = MINVAL(a)
                bsort(i) = b(MINLOC(a, DIM=1))
                a(MINLOC(a, DIM=1)) = init_max+1
            END DO

        END SUBROUTINE carry_sorter
END MODULE helper

PROGRAM carry_sort
    USE helper
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To develop a simple carry sort algorithm
    !       The output should be a = -23, -6, -1, 0, 1, 1, 5, 11, 17
    !                            b =   0, 36, -1,10,31,-1,-8,101,-17
    !       Source: Exercise 7-20

    !   Variable Declarations
    INTEGER :: start_time   ! execution start time
    INTEGER :: end_time     ! execution end time
    REAL, DIMENSION(9) :: &
        a = (/ 1.,  11.,  -6.,  17., -23.,  0.,  5.,  1., -1. /), &
        b = (/ 31., 101., 36., -17.,   0., 10., -8., -1., -1. /)
    REAL, DIMENSION(9) :: asort, bsort ! sorted versions of a and b

    !   start timer
    CALL SYSTEM_CLOCK(start_time)


    !   call sorting subroutine
    CALL carry_sorter(a, b, asort, bsort)

    !   return sorted arrays
    WRITE (*,*) 'The sorted arrays are'
    WRITE (*,100) asort, bsort
    100 FORMAT (9F7.1, /, 9F7.1)

    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! execution time
    end_time = end_time - start_time
    WRITE (*,999) end_time/1000., end_time/60000., end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM carry_sort