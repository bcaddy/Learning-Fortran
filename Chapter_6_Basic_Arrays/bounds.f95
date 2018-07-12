PROGRAM bounds
IMPLICIT NONE

REAL, DIMENSION(5) :: test = (/1., 2., 3., 4., 5./)
REAL, DIMENSION(5) :: test1
INTEGER :: i

DO i=1, 6
    test1(i) = SQRT(test(i))
    WRITE (*, 100) 'SQRT(',test(i),') = ',test1(i)
    100 FORMAT (A,F6.3,A,F14.4)
END DO
END PROGRAM bounds