PROGRAM limiter
    !USE module_name
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To search a random integer array for values greater than 1000 and reduce them to 1000 using DO loops and a WHERE
    !       statement
    !       Source: Exercise 8-11

    ! parameter Declarations
    INTEGER, PARAMETER              :: dim1 = 1000, dim2 = 10, dim3 = 30 ! dimensions for the arrays

    !   Variable Declarations
    INTEGER                         :: start_time ! execution start time
    INTEGER                         :: end_time   ! execution end time
    INTEGER                         :: i,j,k      ! loop indices
    REAL, DIMENSION(dim1,dim2,dim3) :: arr1, arr2 ! arrays

    !   start timer
    CALL SYSTEM_CLOCK(start_time)

    ! initialize random array with range 0 to 1200
    CALL RANDOM_NUMBER(arr1)
    arr1 = arr1 * 1200
    arr2 = arr1

    ! convert all values greater than 1000 to 1000 with DO loops
    outer: DO i = 1, dim1
        mid: DO j = 1, dim2
            inner: DO k = 1, dim3
                IF (arr1(i,j,k) > 1000.) arr1(i,j,k) = 1000.
            END DO inner
        END DO mid
    END DO outer

    ! convert all values greater than 1000 to 1000 with WHERE construct
    WHERE ( arr2 > 1000.  ) arr2 = 1000

    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM limiter
