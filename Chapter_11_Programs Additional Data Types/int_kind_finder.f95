PROGRAM int_kind_finder
    !USE module_name
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To find all the different kinds of integers supported by a system
    !       Source: Exercise 11-8

    !   Variable Declarations
    INTEGER                :: start_time   ! execution start time
    INTEGER                :: end_time     ! execution end time
    INTEGER                :: i,j=1        ! loop indices
    INTEGER                :: num          ! the kind number
    INTEGER, DIMENSION(10) :: kind_nums=-1 ! the kind numbers
    INTEGER, DIMENSION(10) :: ranges=-1    ! the range of a given kind number

    !   start timer
    CALL SYSTEM_CLOCK(start_time)

    ! determine kinds
    DO i = 100, 1, -1
        num = SELECTED_INT_KIND(i)

        IF (.NOT. ANY(kind_nums==num)) THEN
            kind_nums(j) = num
            ranges(j) = i
            j = j+1
        END IF
    END DO

    ! return results
    100 FORMAT ('The integer kind with range 10^',I2,' is ',I2)

    DO i = 10, 1, -1
        IF (ranges(i)>-1) WRITE (unit=*, fmt=100) ranges(i), kind_nums(i)
    END DO


    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM int_kind_finder
