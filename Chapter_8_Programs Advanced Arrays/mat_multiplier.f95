PROGRAM mat_multiplier
    IMPLICIT NONE !requires me to declare all variables

    !   Purpose:
    !       To read two matrices from files and multiply them
    !       Source: Exercise 8-13, 8-14, 8-15

    !   Variable Declarations
    INTEGER                           :: start_time      ! execution start time
    INTEGER                           :: end_time        ! execution end time
    INTEGER                           :: i,j             ! loop indices
    INTEGER                           :: err             ! status indicator
    INTEGER, DIMENSION(2)             :: rowlen,collen   ! number of rows and collums in each matrix.  rowlen(1) is for the first matrix etc
    REAL, ALLOCATABLE, DIMENSION(:,:) :: matrix1,matrix2 ! the original matrix
    REAL, ALLOCATABLE, DIMENSION(:,:) :: output_own      ! the output matrix from my own algorithm
    REAL, ALLOCATABLE, DIMENSION(:,:) :: output_intr     ! the output matrix from MATMUL
    CHARACTER(50)                     :: fmtstring

    !   start timer
    CALL SYSTEM_CLOCK(start_time)

    ! open the files
    OPEN(unit=3, file='matrix1.dat', iostat=err, status="old", action="read")
    IF ( err /= 0 ) STOP "err opening matrix1.dat "
    OPEN(unit=4, file='matrix2.dat', iostat=err, status="old", action="read")
    IF ( err /= 0 ) STOP "err opening matrix2.dat "

    ! read in the first line to determine sizes
    READ (3,*) rowlen(1), collen(1)
    READ (4,*) rowlen(2), collen(2)
    IF (collen(1) /= rowlen(2)) STOP "Matrices are not of compatible size"

    ! ALLOCATE arrays, including output_own array
    ALLOCATE(matrix1(rowlen(1), collen(1)), stat=err)
    IF ( err /= 0) WRITE (*,*) "matrix1: Allocation request denied"
    ALLOCATE(matrix2(rowlen(2), collen(2)), stat=err)
    IF ( err /= 0) WRITE (*,*) "matrix2: Allocation request denied"
    ALLOCATE(output_own(rowlen(1), collen(2)), stat=err)
    IF ( err /= 0) WRITE (*,*) "output_own: Allocation request denied"
    ALLOCATE(output_intr(rowlen(1), collen(2)), stat=err)
    IF ( err /= 0) WRITE (*,*) "output_intr: Allocation request denied"

    ! read in the rest of the matrix and close the file
    READ (3,*) ((matrix1(i,j),j=1,collen(1)), i=1,rowlen(1))
    READ (4,*) ((matrix2(i,j),j=1,collen(2)), i=1,rowlen(2))
    CLOSE(unit=3)
    CLOSE(unit=4)

    ! multiply using own algorithm
    rowdo: DO i = 1, rowlen(1)
        coldo: DO j = 1, collen(2)
            output_own(i,j) = SUM(matrix1(i,:) * matrix2(:,j))
        END DO coldo
    END DO rowdo

    ! multiply using MATMUL
    output_intr = MATMUL(matrix1, matrix2)

    ! return values of the two Matrices
    WRITE (fmtstring,*)  '(',collen(2),'F9.2,/)' ! format for printing
    WRITE (unit=*, fmt=*) 'The value of output_own is'
    WRITE (*,fmtstring) (output_own(i,:), i=1,rowlen(1)) ! print output_own in a neat way
    WRITE (unit=*, fmt=*) 'The value of output_intr is'
    WRITE (*,fmtstring) (output_intr(i,:), i=1,rowlen(1)) ! print output_intr in a neat way


    ! DEALLOCATE arrays
    IF (ALLOCATED(matrix1)) DEALLOCATE(matrix1, stat=err)
    IF ( err /= 0) WRITE (*,*) "matrix1: Deallocation request denied"
    IF (ALLOCATED(matrix2)) DEALLOCATE(matrix2, stat=err)
    IF ( err /= 0) WRITE (*,*) "matrix2: Deallocation request denied"
    IF (ALLOCATED(output_own)) DEALLOCATE(output_own, stat=err)
    IF ( err /= 0) WRITE (*,*) "output_own: Deallocation request denied"
    IF (ALLOCATED(output_intr)) DEALLOCATE(output_intr, stat=err)
    IF ( err /= 0) WRITE (*,*) "output_intr: Deallocation request denied"

    !   Finish up.
    CALL SYSTEM_CLOCK(end_time)
    ! print execution time to terminal
    end_time = end_time - start_time
    WRITE (*,999) end_time/1.E3, end_time/6.E4, end_time/3.6E6
    999 FORMAT (/,'Program complete. Time to execute ', F7.3, 's or ', F7.3, 'min or ', F7.3, 'hrs')
END PROGRAM mat_multiplier
