PROGRAM hyp_calc
!   Purpose:
!       To find the hypotenuse of a right triangle using the lengths of the other two sides
!       Excercise 2-17

IMPLICIT NONE

!   Variable Definitions
REAL :: adj  ! length of side adjacent to the angle used (meters)
REAL :: op   ! length of side opposite to the angle used (meters)
REAL :: theta! angle (meters)
REAL :: hyp  ! length of the hypotenuse (meters)

!   Get values from the user about the lengths of the sides
WRITE (*,*) 'Enter the lengths of the two sides in meters.  Adjacent then opposite: '
READ (*,*)  adj, op

!   Echo values
WRITE (*,*) 'The length of the adjacent side is ', adj, 'meters. The length of the opposite side is ', op, 'meters'

!   Calculate hypotonuse
theta = ATAN(op/adj) ! calculate theta
hyp = op / SIN(theta) ! calculate the hypotenuse

!   Return value
WRITE (*,*) 'The length of the hypotenuse is ',hyp, 'meters'

!   Finish
END PROGRAM hyp_calc

