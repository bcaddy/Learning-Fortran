PROGRAM distance_calc
IMPLICIT NONE !requires me to declare all variables
!   Purpose:
!       Calculate the Euclidean distance between two points in R^2
!       Source: Exercise 2-20

!   Variable Declarations
REAL :: x1 = 2.  ! x position of first point (meters)
REAL :: y1 = 3.  ! y position of first point (meters)
REAL :: x2 = 8.  ! x position of second point (meters)
REAL :: y2 = -5.  ! y position of second point (meters)
REAL :: d   ! distance between the two points

!   Calculations
d = SQRT( (x1-x2)**2 + (y1-y2)**2 )

!   return result
WRITE (*,*) 'The distance between the two points is', d, 'meters'


!   Finish up.
END PROGRAM distance_calc