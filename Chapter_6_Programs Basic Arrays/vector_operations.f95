PROGRAM vector_operations
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       This program takes 2 N-dimensional position vectors and calculates their inner product, outer product, and separation
!       Source: Exercises 6-16, 6-16, and 6-20

!   Parameter Decrations
INTEGER, PARAMETER :: dimens = 3 ! dimensionality

!   Variable Declarations
REAL, DIMENSION(dimens) :: vec1  ! vector 1
REAL, DIMENSION(dimens) :: vec2  ! vector 2
REAL, DIMENSION(dimens,dimens) :: outer ! outer product
REAL, DIMENSION(dimens) :: cross ! cross product
REAL :: inner   ! inner product
REAL :: dist    ! distance

!   Assign values to vec1 and vec2
vec1 = (/ 5., -3., 2. /)
vec2 = (/ 2., 3., 4. /)

!   Inner product
inner = SUM(vec1 * vec2)

!   Outer product
outer = SPREAD(vec1, DIM=2, NCOPIES=dimens) * SPREAD(vec2, DIM=1, NCOPIES=dimens)

!   Cross product
cross(1) = vec1(2) * vec2(3) - vec1(3) * vec2(2)
cross(2) = vec1(3) * vec2(1) - vec1(1) * vec2(3)
cross(3) = vec1(1) * vec2(2) - vec1(2) * vec2(1)

!   Distance
dist = SQRT(SUM((vec1 - vec2)**2))


!   Return results
!the vectors
WRITE (*,100) vec1, vec2
100 FORMAT ('The two vectors are as follows',/,'vec1 = ',3(F7.2),'  vec2 = ',3(F7.2))
!inner product
WRITE (*,101) inner
101 FORMAT (/, 'The inner product is ',3(F7.2))
!outer product
WRITE (*,102) outer
102 FORMAT (/, 'The outer product is',/, 3(3(F7.2)/))
!distance
WRITE (*,103) dist
103 FORMAT (/,'The distance between the two vectors is',2(F7.2))
!cross
WRITE (*,104) cross
104 FORMAT (/, 'The cross product is', 3(F7.2))





!   Finish up.
WRITE (*,'(//, A)') 'Program complete'
END PROGRAM vector_operations