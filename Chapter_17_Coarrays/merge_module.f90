MODULE merge_module
  !   Purpose:
  !       To implement a merge subroutine
  !       Exercises: 17.1-17.3

  ! Import modules
    IMPLICIT NONE !requires me to declare all variables


  PRIVATE
  PUBLIC merge_sub

CONTAINS

  SUBROUTINE merge_sub(b1, size1, b2, size2, out, size_out)
    ! Subroutine to merge two sorted arrays together in increasing order.
    IMPLICIT NONE

    ! Declare calling arguments
    INTEGER, INTENT(IN)               :: size1    ! Size of array b1
    REAL,DIMENSION(size1), INTENT(IN) :: b1       ! Input array b1
    INTEGER, INTENT(IN)               :: size2    ! Size of array b2
    REAL,DIMENSION(size1), INTENT(IN) :: b2       ! Input array b2
    INTEGER                           :: size_out ! Size of array out
    REAL,DIMENSION(size_out)          :: out      ! Output array out

    ! Declare local variables
    INTEGER :: i1 = 1   ! Pointer in b1
    INTEGER :: i2 = 1   ! Pointer in b2
    INTEGER :: iout = 1 ! Pointer in out

    ! Now do the merge, putting the smaller value
    ! from either input into the output array at
    ! each step.
    DO
      IF ( iout > size_out ) THEN
        ! All done, get out.
        EXIT
      ELSE IF ( i1 > size1 ) THEN
        ! If b1 is finished, use b2
        out(iout) = b2(i2)
        iout = iout + 1
        i2 = i2 + 1

      ELSE IF ( i2 > size2 ) THEN
        ! If b2 is finished, use b1
        out(iout) = b2(i1)
        iout = iout + 1
        i1 = i1 + 1

      ELSE IF ( b1(i1) <= b2(i2) ) THEN
        ! If b1 is smaller, use it
        out(iout) = b1(i1)
        iout = iout + 1
        i1 = i1 + 1

      ELSE IF ( b1(i1) > b2(i2) ) THEN
        ! If b2 is smaller, use it
        out(iout) = b2(i2)
        iout = iout + 1
        i2 = i2 + 1
      END IF
    END DO

  END SUBROUTINE merge_sub

END MODULE merge_module
