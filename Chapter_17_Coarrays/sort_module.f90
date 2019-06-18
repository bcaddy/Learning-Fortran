MODULE sort_module
  !   Purpose:
  !       Implementation of Sorting algorithm
  !       Project: 17.1-17.3

  ! Import modules
  IMPLICIT NONE !requires me to declare all variables


  PRIVATE
  PUBLIC sort !insert the names of what should be public here

CONTAINS
  !add procedures, data types, and data here

  SUBROUTINE sort(arr, n)
    !
    ! Purpose:
    ! To sort real array "arr" into ascending order using a selection ! sort.
    !
    IMPLICIT NONE
    ! Data dictionary: declare calling parameter types & definitions
    INTEGER, INTENT(IN)               :: n   ! Number of values
    REAL, DIMENSION(n), INTENT(INOUT) :: arr ! Array to be sorted

    ! Data dictionary: declare local variable types & definitions
    INTEGER :: i    ! Loop index
    INTEGER :: iptr ! Pointer to smallest value
    INTEGER :: j    ! Loop index
    REAL    :: temp ! Temp variable for swaps

    ! Sort the array
    outer: DO i = 1, n-1

      ! Find the minimum value in arr(I) through arr(N)
      iptr = i
      inner: DO j = i+1, n
        minval: IF ( arr(j) < arr(iptr) ) THEN
          iptr = j
        END IF minval
      END DO inner

      ! iptr now points to the minimum value, so swap arr(iptr)
      ! with arr(i) if i /= iptr.
      swap: IF ( i /= iptr ) THEN
        temp = arr(i)
        arr(i) = arr(iptr)
        arr(iptr) = temp
      END IF swap

    END DO outer
  END SUBROUTINE sort

END MODULE sort_module
