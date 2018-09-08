MODULE helper
    ! contains the definition of the linked_list data type and the least square subroutine
    IMPLICIT NONE

    ! Parameter Decrations
    INTEGER, PARAMETER :: dbl       = SELECTED_REAL_KIND(p=13) ! kind number of double

    TYPE :: linked_list
        REAL(dbl)                   :: xvalue
        REAL(dbl)                   :: yvalue
        TYPE (linked_list), POINTER :: p
    END TYPE linked_list

    CONTAINS

        SUBROUTINE perform_least_squares (head, nvals, a, b)
            ! performs least squars fitting on the data in the linked list head

            ! Variable Decrations
            TYPE (linked_list), POINTER :: head  ! the head pointer of the linked list
            TYPE (linked_list), POINTER :: ptr
            REAL(dbl), INTENT(OUT)      :: a     ! zero order coefficient
            REAL(dbl), INTENT(OUT)      :: b     ! first order coefficient
            INTEGER, INTENT(IN)         :: nvals ! total number of values
            INTEGER                     :: i     ! loop index
            REAL(dbl) :: sumx=0, sumy=0, sumxy=0, sumx2=0, ybar=0, xbar=0 ! sum of all x values, sum of x*y, sum of x^2, y average, x average

            ptr => head ! point the active pointer at the head of the list
            DO i = 1, nvals
                ! update all the sums
                sumx  = sumx  + ptr%xvalue
                sumy  = sumy  + ptr%yvalue
                sumx2 = sumx2 +(ptr%xvalue)**2
                sumxy = sumxy + ptr%xvalue * ptr%yvalue

                ! update the pointer
                ptr => ptr%p
            END DO

            ! calculate the average values
            xbar = sumx/DBLE(nvals)
            ybar = sumy/DBLE(nvals)

            ! calculate the first order coefficient
            b = (sumxy - sumx*ybar) / (sumx2 - sumx*xbar)

            ! calculate the zero order coefficient
            a = ybar - b*xbar


        END SUBROUTINE perform_least_squares

END MODULE helper


PROGRAM least_squares
    USE helper
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To read in the data from a file and fit a linear least squares fit to it.
    !       The correct answer is c=a+b*x where a=2.5 and b=5.0
    !       Source: Exercise 15-19

    !   Parameter Decrations
    CHARACTER(len=68) :: filename = '/Users/Bob/Desktop/Learning_Fortran/Chapter_15_Pointers/fit_data.dat' ! name of the data file

    !   Variable Declarations
    INTEGER                              :: start_time  ! execution start time
    INTEGER                              :: end_time    ! execution end time
    INTEGER                              :: istat       ! status indicator
    INTEGER                              :: nvals = 0   ! number of data red
    TYPE (linked_list), POINTER          :: head        ! beginning of linked list
    TYPE (linked_list), POINTER          :: tail        ! end of linked list
    REAL(dbl)                            :: tempx, tempy! temp variable
    REAL(dbl)                            :: a, b        ! zeroth and first order coefficients

    nullify (head)
    !   start timer
    CALL SYSTEM_CLOCK(start_time)

    ! open input data file
    OPEN(unit=9, file=filename, iostat=istat)
    IF ( istat /= 0 ) STOP "Error opening data file"

    ! read the data into the linked list
    input: DO
        ! read in the next value
        READ(unit=9, fmt="(2E25.18)", iostat=istat) tempx, tempy
        IF ( istat /= 0 ) EXIT ! this indicates the end of the file so we exit the loop
        nvals = nvals + 1 ! increase the count on the number of entries

        IF (.NOT. ASSOCIATED(head)) THEN ! checks to see if the first value has been associated
            ALLOCATE (head, STAT=istat)  ! allocate new variable
            tail => head                 ! points the tail at the new value
            NULLIFY (tail%p)             ! nullify p in the new variable
            tail%xvalue = tempx          ! set the variable value
            tail%yvalue = tempy          ! set the variable value
        ELSE ! if this is not the first value then
            ALLOCATE (tail%p, STAT=istat) ! allocate new variable
            tail => tail%p               ! assign tail to the new end of linked list
            NULLIFY (tail%p)             ! nullify the pointer
            tail%xvalue = tempx          ! set the variable value
            tail%yvalue = tempy          ! set the variable value
        END IF
        
    END DO input

    ! close the file
    CLOSE(unit=9, iostat=istat)
    IF ( istat /= 0 ) STOP "Error closing file unit 9"

    ! perform the least squares fit
    CALL perform_least_squares(head, nvals, a, b)

    ! return results to user
    WRITE (unit=*, fmt=*) 'a=',a,'  b = ', b

    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM least_squares
