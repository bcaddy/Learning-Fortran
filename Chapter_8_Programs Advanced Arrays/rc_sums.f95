PROGRAM rc_sums
    !USE module_name
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To calculate the sum of each row and each column of a 2D array
    !       Source: Exercises 8-8, 8-9, and 8-10

    !   Variable Declarations
    INTEGER                           :: start_time ! execution start time
    INTEGER                           :: end_time   ! execution end time
    INTEGER                           :: i,j        ! loop index
    INTEGER                           :: collen     ! number of columns
    INTEGER                           :: rowlen     ! number of rows
    INTEGER                           :: err        ! status indicator
    REAL, ALLOCATABLE, DIMENSION(:,:) :: matrix     ! the original matrix
    ! CHARACTER(50)                     :: fmtstring  ! write format string for array


    !   start timer
    CALL SYSTEM_CLOCK(start_time)

    ! open the file
    OPEN(unit=3, file='rc_sums.dat', iostat=err, status="old", action="read")
    IF ( err /= 0 ) STOP "Error opening file "

    ! read in the first line
    READ (3,*) rowlen, collen

    ! ALLOCATE arrays
    ALLOCATE(matrix(rowlen, collen), stat=err)
    IF ( err /= 0) WRITE (*,*) "matrix: Allocation request denied"

    ! read in the rest of the matrix and close the file
    READ (3,*) ((matrix(i,j),j=1,collen), i=1,rowlen)
    CLOSE(unit=3)

    ! test that the array was read correctly
    ! WRITE (fmtstring,*)  '(',collen,'F9.2,/)'
    ! WRITE (*,fmtstring) (matrix(i,:), i=1,rowlen)

    ! find the sums across all the rows
    100 FORMAT ('The sum of row ', I4, ' is ', F9.2)
    DO i = 1, rowlen
        WRITE (*,100) i, SUM(matrix(i,:))
    END DO

    ! find the sums across all the columns
    101 FORMAT ('The sum of column ', I4, ' is ', F9.2)
    DO i = 1, collen
        WRITE (*,101) i, SUM(matrix(:,i))
    END DO

    ! DEALLOCATE arrays
    IF (ALLOCATED(matrix)) DEALLOCATE(matrix, stat=err)
    IF ( err /= 0) WRITE (*,*) "matrix: Deallocation request denied"


    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM rc_sums
